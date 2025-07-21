#!/usr/bin/env python

from os import environ
from os.path import abspath, dirname, join
from pathlib import Path
import sys

from e3.fs import mkdir
from e3.testsuite import Testsuite
from e3.testsuite.driver.classic import ClassicTestDriver
from e3.testsuite.driver.diff import DiffTestDriver
from e3.testsuite.report.index import ReportIndex
from e3.testsuite.report.xunit import XUnitImporter


class VSSLegacyDriver(ClassicTestDriver):
    """Legacy driver, it run "make check" to execute VSS tests."""

    xunit_dir = None

    def run(self):
        root_dir = dirname(dirname(abspath(__file__)))

        self.xunit_dir = join(root_dir, "xunit")
        self.env.environ["XUNIT_XML_DIR"] = self.xunit_dir

        mkdir(self.xunit_dir)
        self.shell(
            args=["make", "check"],
            cwd=root_dir,
            env=self.env.environ,
            analyze_output=True,
        )

    def analyze(self):
        xunit_files = list(Path(self.xunit_dir).glob("*.xml"))

        index = ReportIndex(self.xunit_dir)
        importer = XUnitImporter(index)

        for xunit_file in xunit_files:
            importer.run(xunit_file)

        if index.has_failures:
            self.push_failure("Some test failed.")

        for entry in index.entries.values():
            self.push_result(entry.load())

        return super().analyze()


class VSSGDBPPDriver(DiffTestDriver):
    """Driver to run tests of the GDB pretty printer."""

    def set_up(self):
        super().set_up()

        self.test_environment = environ
        self.test_environment["VSS_GDBPP"] = join(
            dirname(dirname(self.test_env["test_dir"])), "gdb"
        )

    def run(self):
        script_path = join(self.test_env["test_dir"], "test.sh")

        self.shell(args=["bash", script_path], env=self.test_environment)


class VSSTestsuite(Testsuite):
    """Testsuite for the VSS library"""

    test_driver_map = {"legacy": VSSLegacyDriver, "gdbpp": VSSGDBPPDriver}
    default_driver = "legacy"


if __name__ == "__main__":
    sys.exit(VSSTestsuite().testsuite_main())

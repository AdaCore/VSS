#!/usr/bin/env python

from os import environ
from os.path import abspath, dirname, join
import sys

from e3.testsuite import Testsuite
from e3.testsuite.driver.classic import ClassicTestDriver
from e3.testsuite.driver.diff import DiffTestDriver


class VSSLegacyDriver(ClassicTestDriver):
    """Legacy driver, it run "make check" to execute VSS tests."""

    def run(self):
        root_dir = dirname(dirname(abspath(__file__)))

        self.shell(args=["make", "check"], cwd=root_dir, analyze_output=True)


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

#!/usr/bin/env python

from os.path import abspath, dirname
import sys

from e3.testsuite import Testsuite
from e3.testsuite.driver.classic import ClassicTestDriver


class VSSLegacyDriver(ClassicTestDriver):
    """Legacy driver, it run "make check" to execute VSS tests."""

    def run(self):
        root_dir = dirname(dirname(abspath(__file__)))

        self.shell(args=["make", "check"], cwd=root_dir, analyze_output=True)


class VSSTestsuite(Testsuite):
    """Testsuite for the VSS library"""

    test_driver_map = {"legacy": VSSLegacyDriver}
    default_driver = "legacy"


if __name__ == "__main__":
    sys.exit(VSSTestsuite().testsuite_main())

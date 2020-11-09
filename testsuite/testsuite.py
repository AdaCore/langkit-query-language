#! /usr/bin/env python

from e3.testsuite import Testsuite
from drivers import (
    CheckerDriver, ParserDriver, InterpreterDriver
)
import os
import sys


class LKQLTestsuite(Testsuite):
    tests_subdir = "tests"
    test_driver_map = {'parser': ParserDriver,
                       'interpreter': InterpreterDriver,
                       'checker': CheckerDriver}

    def add_options(self, parser):
        parser.add_argument(
            '--rewrite', '-r', action='store_true',
            help='Rewrite test baselines according to current output.'
        )

    def set_up(self):
        super().set_up()
        self.env.rewrite_baselines = self.env.options.rewrite


if __name__ == "__main__":
    sys.exit(LKQLTestsuite().testsuite_main())

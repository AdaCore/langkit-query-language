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
            '--no-auto-path', action='store_true',
            help='Do not add test programs to the PATH. Useful to test'
                 ' packages.'
        )
        parser.add_argument(
            '--rewrite', '-r', action='store_true',
            help='Rewrite test baselines according to current output.'
        )

    def set_up(self):
        super().set_up()
        self.env.rewrite_baselines = self.env.options.rewrite

        # Directory that contains GPR files, shared by testcases
        self.env.ada_projects_path = os.path.join(
            self.root_dir, 'ada_projects'
        )

        # Unless specifically told not to, add test programs to the environment
        if not self.env.options.no_auto_path:
            repo_root = os.path.dirname(self.root_dir)

            def in_repo(*args):
                return os.path.join(repo_root, *args)

            os.environ['PATH'] = os.path.pathsep.join([
                in_repo('lkql', 'build', 'obj-mains'),
                in_repo('lkql_checker', 'bin'),
                os.environ['PATH'],
            ])


if __name__ == "__main__":
    sys.exit(LKQLTestsuite().testsuite_main())

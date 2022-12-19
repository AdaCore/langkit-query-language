#! /usr/bin/env python

from e3.fs import mkdir, rm
from e3.testsuite import Testsuite
from drivers import (
    CheckerDriver, ParserDriver, InterpreterDriver, GnatcheckDriver
)
import glob
import os
import subprocess
import sys


class LKQLTestsuite(Testsuite):
    tests_subdir = "tests"
    test_driver_map = {'parser': ParserDriver,
                       'interpreter': InterpreterDriver,
                       'checker': CheckerDriver,
                       'gnatcheck': GnatcheckDriver}

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
        parser.add_argument(
            '--coverage',
            help='Compute code coverage. Argument is the output directory for'
                 ' the coverage report.'
        )

    def set_up(self):
        super().set_up()
        self.env.rewrite_baselines = self.env.options.rewrite

        # Directory that contains GPR files, shared by testcases
        os.environ['GPR_PROJECT_PATH'] = os.path.pathsep.join([
            os.path.join(self.root_dir, 'ada_projects'),
            os.environ.get('GPR_PROJECT_PATH', ''),
        ])

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

            os.environ['LKQL_PATH'] = os.path.pathsep.join([
                in_repo('lkql_checker/share/lkql'),
                os.environ.get('LKQL_PATH', '')
            ])

            if os.environ.get('LKQL_JIT', 'false') == 'true':
                os.environ['LKQL_RULES'] = ':'.join([
                    in_repo('lkql_checker/share/lkql'),
                    in_repo('lkql_checker/share/lkql/kp'),
                ])

        # Ensure the testsuite starts with an empty directory to store source
        # trace files.
        self.env.traces_dir = os.path.join(self.working_dir, 'traces')
        if self.env.options.coverage:
            rm(self.env.traces_dir)
            mkdir(self.env.traces_dir)

    def tear_down(self):
        # Generate code coverage report if requested
        if self.env.options.coverage:
            # Create a response file to contain the list of traces
            traces_list = os.path.join(self.working_dir, "traces.txt")
            with open(traces_list, "w") as f:
                for filename in glob.glob(
                    os.path.join(self.env.traces_dir, "*", "*.srctrace")
                ):
                    f.write(f"{filename}\n")

            subprocess.check_call([
                'gnatcov',
                'coverage',
                '-Plkql_checker.gpr',
                '--externally-built-projects',
                '--no-subprojects',
                '--projects=lkql_checker',
                '--projects=liblkqllang',
                '--level=stmt',
                '--annotate=dhtml',
                f'--output-dir={self.env.options.coverage}',
                f'@{traces_list}',
            ])

        super().tear_down()


if __name__ == "__main__":
    sys.exit(LKQLTestsuite().testsuite_main())

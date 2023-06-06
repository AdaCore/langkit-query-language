#! /usr/bin/env python

from e3.fs import mkdir, rm
from e3.testsuite import Testsuite
from support.drivers import (
    CheckerDriver, ParserDriver, InterpreterDriver
)
from support.gnatcheck_driver import GnatcheckDriver
import glob
import os
import os.path as P
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
        parser.add_argument(
            '--mode', default='ada',
            help='The LKQL implementations to test.'
                 ' Possible values are "ada", "jit" and "native_jit".'
        )

    def lkql_executables(self):
        """
        Return a pair containing the name of the lkql "interpreter",
        "checker" and "gnatcheck worker" executables according to the chosen
        testsuite mode.
        """
        # If the mode is Ada
        if self.env.options.mode == "ada":
            return (["lkql_ada"], ["lkql_checker"], [])

        # If the mode is JIT
        elif self.env.options.mode == "jit":
            # Prepare the command for calling Java with the LKQL JIT implementation
            graal_home = os.environ['GRAAL_HOME']
            lkql_jit_home = os.environ.get(
                'LKQL_JIT_HOME',
                P.join(graal_home, 'languages', 'lkql')
            )

            # Get the Java executable
            java = (
                P.join(graal_home, 'bin', 'java.exe')
                if os.name == 'nt' else
                P.join(graal_home, 'bin', 'java')
            )

            # Create the class path
            base_class_path = [
                P.join(graal_home, 'lib', 'truffle', 'truffle-api.jar'),
                P.join(lkql_jit_home, 'lkql_jit.jar'),
            ]
            launcher_class_path = os.pathsep.join(base_class_path + [
                P.join(lkql_jit_home, 'lkql_jit_launcher.jar')
            ])
            checker_class_path = os.pathsep.join(base_class_path + [
                P.join(lkql_jit_home, 'lkql_jit_checker.jar')
            ])
            gnatcheck_worker_class_path = os.pathsep.join(base_class_path + [
                P.join(lkql_jit_home, 'gnatcheck_worker.jar')
            ])

            # Get the java.library.path
            java_library_path = (
                os.environ.get('PATH', "")
                if os.name == 'nt' else
                os.environ.get('LD_LIBRARY_PATH', "")
            )

            java_defs = [
                f'-Djava.library.path={java_library_path}',
                f'-Dtruffle.class.path.append={P.join(lkql_jit_home, "lkql_jit.jar")}',
            ]

            # Prepare the Java commands
            launcher_cmd = [
                java, "-cp", launcher_class_path
            ] + java_defs + ["com.adacore.lkql_jit.LKQLLauncher"]

            checker_cmd = [
                java, "-cp", checker_class_path
            ] + java_defs + ["com.adacore.lkql_jit.LKQLChecker"]

            worker_cmd = [
                java, "-cp", gnatcheck_worker_class_path
            ] + java_defs + ["com.adacore.lkql_jit.GNATCheckWorker"]

            return (launcher_cmd, checker_cmd, worker_cmd)

        # If the mode is native JIT
        elif self.env.options.mode == "native_jit":
            return (
                ["native_lkql_jit"],
                ["native_lkql_jit_checker"],
                ["native_gnatcheck_worker"]
            )

        # Else, there is an error
        else:
            raise RuntimeError("invalid testsuite mode"
                               f" '{self.env.options.mode}'")

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

            os.environ['LKQL_RULES_PATH'] = os.path.pathsep.join([
                in_repo('lkql_checker/share/lkql'),
                in_repo('lkql_checker/share/lkql/kp'),
            ])

        (lkql_exe, lkql_checker_exe, worker_exe) = self.lkql_executables()

        os.environ['LKQL_EXE'] = "&".join(lkql_exe)
        os.environ['LKQL_CHECKER_EXE'] = "&".join(lkql_checker_exe)
        if worker_exe:
            os.environ['GNATCHECK_WORKER'] = " ".join(worker_exe)

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

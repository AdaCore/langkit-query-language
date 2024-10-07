#! /usr/bin/env python

from argparse import ArgumentParser
import glob
import os
import os.path as P
import statistics
import subprocess
import sys
from typing import TextIO, Callable

from e3.fs import mkdir, rm
from e3.testsuite import Testsuite, logger, TestsuiteCore
from e3.testsuite.testcase_finder import (
    ProbingError, TestFinder, YAMLTestFinder, TestFinderResult
)

from drivers import (
    checker_driver, gnatcheck_driver, interpreter_driver, parser_driver, java_driver,
    benchmarks_driver, refactor_driver
)

class PerfTestFinder(YAMLTestFinder):
    """
    Testcase finder to use in perf mode.

    This finder automatically discard tests that do not have performance
    measuring instructions. This is preferable to creating these tests but
    skipping them (SKIP status) as most tests do not support performance
    measurements: less noise in the testsuite report.
    """

    def probe(self,
              testsuite: TestsuiteCore,
              dirpath: str,
              dirnames: list[str],
              filenames: list[str]) -> TestFinderResult:
        # Probe testcases as usual
        result = super().probe(testsuite, dirpath, dirnames, filenames)

        # Reject testcases which do not contain performance measuring
        # instructions.
        if result is None or P.join("tests", "perf") not in result.test_dir:
            return None

        # Make sure that the driver supports performance measuring
        if not result.driver_cls.perf_supported:
            raise ProbingError(
                f"The '{result.driver_cls.__name__}' driver does not support"
                 " performance measuring"
            )

        return result


class StandardTestFinder(YAMLTestFinder):
    """
    Testcase finder to use in stadard mode.

    This finder exclude test cases from the 'tests/perf/' directory to avoid
    running them in standard mode. This allow performance exclusive test
    cases.
    This finder doesn't exclude all performance compatible tests because
    we want to be able to write baseline/performance hybrid tests.
    """

    def probe(self,
              testsuite: TestsuiteCore,
              dirpath: str,
              dirnames: list[str],
              filenames: list[str]) -> TestFinderResult:
        # Probe testcases as usual
        result = super().probe(testsuite, dirpath, dirnames, filenames)

        # Reject all tests which have 'tests/perf' in their directory name
        if result is None or P.join("tests", "perf") in result.test_dir:
            return None

        return result


class LKQLTestsuite(Testsuite):
    tests_subdir = "tests"
    test_driver_map = {'parser': parser_driver.ParserDriver,
                       'interpreter': interpreter_driver.InterpreterDriver,
                       'java': java_driver.JavaDriver,
                       'checker': checker_driver.CheckerDriver,
                       'gnatcheck': gnatcheck_driver.GnatcheckDriver,
                       'benchmarks': benchmarks_driver.BenchmarksDriver,
                       'refactor': refactor_driver.RefactorDriver,}

    def add_options(self, parser: ArgumentParser) -> None:
        parser.add_argument(
            '--mode', default='jit', choices=['jit', 'native_jit'],
            help='The LKQL implementations to test.'
                 ' Possible values are "jit" and "native_jit".'
        )
        parser.add_argument(
            "--codepeer", action="store_true",
            help="Run the testsuite with the codepeer target and runtime"
        )
        parser.add_argument(
            '--no-auto-path', action='store_true',
            help='Do not add test programs to the PATH. Useful to test'
                 ' packages.'
        )
        parser.add_argument(
            '--no-flag-checking', action='store_true',
            help='Disable the checking phase for the FLAG/NOFLAG annotations.'
        )
        parser.add_argument(
            '--rewrite', '-r', action='store_true',
            help='Rewrite test baselines according to current output.'
        )
        parser.add_argument(
            '--add-missing-flags', action='store_true',
            help='Add the missing "FLAG" annotations in the tests Ada sources.'
        )
        parser.add_argument(
            '--coverage',
            help='Compute code coverage. Argument is the output directory for'
                 ' the coverage report.'
        )

        parser.add_argument(
            '--perf-mode',
            help='Run the testsuite in performance mode: only run tests with'
                 ' instructions to measure performance. The argument is the'
                 ' directory in which to put profile data files.',
            dest='perf_output_dir'
        )
        parser.add_argument(
            '--perf-no-profile',
            action='store_true',
            help='When running the testsuite in performance mode, only run'
                 ' the default perf measurements (no profile). This is useful'
                 ' to get feedback quickly during development.'
        )

    @property
    def test_finders(self) -> list[TestFinder]:
        return [
            PerfTestFinder()
            if self.env.perf_mode else
            StandardTestFinder()
        ]

    def set_up(self) -> None:
        super().set_up()
        self.env.rewrite_baselines = self.env.options.rewrite

        # Perf mode is incompatible with some other modes
        if self.env.options.perf_output_dir:
            if self.env.options.coverage:
                logger.error(f"--perf-mode incompatible with --coverage")
                raise RuntimeError

        # Give access ot the testsuite root dir to drivers
        self.env.testsuite_root_dir = self.root_dir

        # Give access to python support directory to drivers
        self.env.support_dir = P.join(self.root_dir, "python_support")

        # Directory that contains GPR files, shared by testcases
        os.environ['GPR_PROJECT_PATH'] = P.pathsep.join([
            P.join(self.root_dir, 'ada_projects'),
            P.abspath(
                P.join(
                    self.root_dir,
                    '..',
                    'lkql_checker',
                    'share',
                    'examples'
                )
            ),
            os.environ.get('GPR_PROJECT_PATH', ''),
        ])

        # If the performance mode is enabled, verify that the user has checked
        # out the common-testsuite-sources in the "ada_projects" directory.
        # Additionally add the internal sources to the GPR project path.
        if self.env.options.perf_output_dir:
            common_sources = P.join(
                self.root_dir,
                "ada_projects",
                "common-testsuite-sources"
            )
            if not P.isdir(common_sources):
                raise RuntimeError("You need to check out"
                                   " 'common-testsuite-sources' to enable"
                                   " performance testing")
            else:
                lalinttest_sources = glob.glob(P.join(common_sources, "*"))
                for source_dir in [
                    s for s in lalinttest_sources if P.isdir(s)
                ]:
                    os.environ['GPR_PROJECT_PATH'] = P.pathsep.join([
                        source_dir,
                        os.environ.get("GPR_PROJECT_PATH", ""),
                    ])

        # Unless specifically told not to, add test programs to the environment
        repo_root = P.dirname(self.root_dir)
        self.env.root_dir = repo_root
        if not self.env.options.no_auto_path:
            def in_repo(*args):
                return P.join(repo_root, *args)

            os.environ['PATH'] = P.pathsep.join([
                in_repo('lkql', 'build', 'obj-mains'),
                in_repo('lkql_checker', 'bin'),
                os.environ['PATH'],
            ])

            os.environ['LKQL_PATH'] = P.pathsep.join([
                in_repo('lkql_checker/share/lkql'),
                os.environ.get('LKQL_PATH', '')
            ])

            os.environ['LKQL_RULES_PATH'] = P.pathsep.join([
                in_repo('lkql_checker/share/lkql'),
                in_repo('lkql_checker/share/lkql/kp'),
            ])

        # Ensure the testsuite starts with an empty directory to store source
        # trace files.
        self.env.traces_dir = P.join(self.working_dir, 'traces')
        if self.env.options.coverage:
            rm(self.env.traces_dir)
            mkdir(self.env.traces_dir)

        # If requested, enable the performance mode and ensure that the output
        # profile data exists.
        if self.env.options.perf_output_dir:
            perf_dir = P.abspath(self.env.options.perf_output_dir)
            if not P.isdir(perf_dir):
                os.makedirs(perf_dir)

            self.env.perf_mode = True
            self.env.perf_dir = perf_dir
            self.env.perf_no_profile = self.env.options.perf_no_profile
        else:
            self.env.perf_mode = False

    def tear_down(self) -> None:
        # Generate code coverage report if requested
        if self.env.options.coverage:
            # Create a response file to contain the list of traces
            traces_list = P.join(self.working_dir, "traces.txt")
            with open(traces_list, "w") as f:
                for filename in glob.glob(
                    P.join(self.env.traces_dir, "*", "*.srctrace")
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

        # If the performance mode is enabled, print a small report
        if self.env.perf_mode:
            self.perf_report()

        super().tear_down()


    def perf_report(self, output_file: TextIO = sys.stdout) -> None:
        """
        Print a small report of the performance testing.
        """
        print("===== Performance metrics =====", file=output_file)

        # Define functions to format metrics into strings
        def format_time(seconds: float) -> str:
            return "{:.2f}s".format(seconds)

        def format_memory(bytes_count: int) -> str:
            units = ["KB", "MB", "GB"]
            unit = units.pop(0)
            while units and bytes_count > 1000:
                unit = units.pop(0)
                bytes_count /= 1000
            return "{:.2f}{}".format(bytes_count, unit)

        # Define the function to compute the given statistics
        def compute_stats(numbers_str: str,
                          get_value: Callable[[str], any],
                          format_value: Callable[[any], str]) -> str:
            numbers = [get_value(n) for n in numbers_str.split()]
            return (
                f"{format_value(min(numbers))} .. {format_value(max(numbers))}"
                f" (median: {format_value(statistics.median(numbers))} |"
                f" mean: {format_value(statistics.mean(numbers))})"
            )

        # Define the function to display the statistics of the ``time`` output
        def print_time_stats(entry):
            print(f"--- {test_name} (run {entry.info['run_count']} time(s))", file=output_file)
            print(
                f"    time:"
                f" {compute_stats(entry.info['time'], float, format_time)}",
                file=output_file
            )
            print(
                f"    memory:"
                f" {compute_stats(entry.info['memory'], int, format_memory)}",
                file=output_file
            )

        # Define the function to display the statistics of the benchmarks
        def print_benchmark_stats(entry):
            for benchmark, runs in entry.info['benchmark_results'].items():
                print(f"    {benchmark}")
                for run, score in runs.items():
                    print(f"      {run} : {score['score']} {score['unit']}")

        # For each test result, display the statistics
        for test_name, entry in sorted(self.report_index.entries.items()):
            print(f"--- {test_name}", file=output_file)
            if entry.info.get('time') and entry.info.get('memory'):
                print_time_stats(entry)
            else:
                print_benchmark_stats(entry)

if __name__ == "__main__":
    sys.exit(LKQLTestsuite().testsuite_main())

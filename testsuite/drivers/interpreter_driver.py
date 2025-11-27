import os
from e3.testsuite.result import FailureReason
from e3.testsuite.driver.classic import TestAbortWithFailure
from drivers.base_driver import BaseDriver


class InterpreterDriver(BaseDriver):
    """
    This driver runs the interpreter with the given arguments and compares the
    interpreter's output to the provided output file.

    The LKQL script to run must be placed in a file called `script`.
    The expected output must be written in a file called `output`.

    Test arguments:
        - project: GPR build file to use (if any)
        - input_sources: A list of Ada sources to run analyze with LKQL
        - lkql_path: A list of directories forwarded to the `LKQL_PATH`
            variable when the test is run.
        - lkt_refactor: Should the test try refactor to Lkt syntax (default is False).
    """

    perf_supported = True
    flag_checking_supported = False
    lkt_output = None

    def base_args(self):
        return self.lkql_exe

    def build_args(self, script_path):
        # Build the process's arguments list
        args = [*self.base_args(), '--script-path', script_path]

        input_sources = self.test_env.get('input_sources', None)
        project = self.test_env.get('project', None)

        if project:
            args += ['-P', project]

        if input_sources:
            args += input_sources

        return args


    def compute_failures(self):
        filename, baseline, is_regexp = self.baseline

        # Normal test
        result = self.compute_diff(filename, baseline, self.output.log)

        # If the test has to change no point in checking lkt_refactor
        if self.rewrite_baseline:
            return result

        # Lkt Refactor test
        match self.test_env.get('lkt_refactor'):
            case None:
                if baseline == self.lkt_output:
                    self.result.diff = 'Test unexpectedly succeeded after refactor TO_LKQL_V2'
                    raise TestAbortWithFailure

            case True:
                result += self.compute_diff(
                    None,
                    baseline,
                    self.lkt_output,
                    failure_message='execution after refactor TO_LKQL_V2: unexpected output'
                )

            case False:
                pass

        if result:
            self.result.failure_reasons.add(FailureReason.DIFF)

        return result

    def run(self) -> None:

        lkql_path = [
            self.working_dir(d)
            for d in self.test_env.get('lkql_path', [])
        ]

        # If lkt_refactor flag is set, then before we run the interpreter,
        # we refactor the "script.lkql" file and run the interpreter on it.
        # This way we can compare the results of the rewritten script
        # with the original
        if self.test_env.get('lkt_refactor') is not False:
            # Translate "script.lkql" to "refactored.lkql"
            refactored_file_path = self.working_dir('refactored.lkql')
            with open(refactored_file_path, 'w') as file:
                file.write(self.shell(
                    [*self.command_base, 'refactor', '-r', 'TO_LKQL_V2', 'script.lkql'],
                    analyze_output=False
                ).out)

            # Run test on refactored script
            lkt_result = self.check_run(
                self.build_args(refactored_file_path),
                lkql_path=os.pathsep.join(lkql_path),
                analyze_output=False,
            )

            self.lkt_output = lkt_result.out

        # Run the interpreter
        self.check_run(self.build_args('script.lkql'), lkql_path=os.pathsep.join(lkql_path))

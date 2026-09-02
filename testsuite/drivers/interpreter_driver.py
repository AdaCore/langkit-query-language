import os
from e3.os.process import DEVNULL, Run
from e3.testsuite.result import FailureReason
from e3.testsuite.driver.classic import TestAbortWithFailure
from drivers.base_driver import BaseDriver
from e3.fs import find


class InterpreterDriver(BaseDriver):
    """
    This driver runs the interpreter with the given arguments and compares the
    interpreter's output to the provided output file.

    The LKQL script to run must be placed in a file called `script`.
    The expected output must be written in a file called `output`.

    Test arguments:
        - project: GPR build file to use (if any)
        - input_sources: A list of Ada sources to run analyze with LKQL
        - script: The LKQL script to interpret (default is script.lkql)
        - lkql_path: A list of directories forwarded to the `LKQL_PATH`
            variable when the test is run.
        - lkt_refactor: Should the test try refactor to Lkt syntax (default is
            False).
        - typecheck: Whether to perform the typechecking pass on the given
            script (default is False).
    """

    perf_supported = True
    flag_checking_supported = False
    lkt_output = None

    def base_args(self):
        return self.lkql_exe

    def build_args(self, script_path):
        # Build the process's arguments list
        args = [*self.base_args(), "--script-path", script_path]

        typecheck = self.test_env.get("typecheck", False)
        input_sources = self.test_env.get("input_sources", None)
        project = self.test_env.get("project", None)

        if typecheck:
            args += ["--typecheck"]

        if project:
            args += ["-P", project]

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
        match self.test_env.get("lkt_refactor"):
            case None:
                if baseline == self.lkt_output:
                    self.result.diff = (
                        "Test unexpectedly succeeded after refactor TO_LKQL_V2"
                    )
                    raise TestAbortWithFailure

            case True:
                result += self.compute_diff(
                    None,
                    baseline,
                    self.lkt_output,
                    failure_message=(
                        "execution after refactor TO_LKQL_V2: unexpected output"
                    ),
                )

            case False:
                pass

        if result:
            self.result.failure_reasons.add(FailureReason.DIFF)

        return result

    def refactor(self) -> None:
        for source_filepath in find(self.test_env["working_dir"], "*.lkql"):
            Run(
                cmds=[
                    *self.command_base,
                    "refactor",
                    "-i",
                    "-r",
                    "TO_LKQL_V2",
                    source_filepath,
                ],
                cwd=self.test_env["working_dir"],
                output=DEVNULL,
                error=DEVNULL,
                input=DEVNULL,
            )

    def run(self) -> None:

        lkql_path = os.pathsep.join(
            [self.working_dir(d) for d in self.test_env.get("lkql_path", [])]
            + [(os.environ["LKQL_PATH"])]
        )

        script = self.test_env.get("script", "script.lkql")

        # Run the interpreter
        self.check_run(self.build_args(script), lkql_path=lkql_path)

        # The default behavior is to try to refactor the *.lkql script files
        # into the Lkt syntax and run the interpreter on it.
        #
        # We do this refactor unless the test is *explicitly* set to False.
        #
        # This way we can compare the results of the rewritten scripts with the
        # original and in the case where the flag is set to True throw an error
        # if the result is different
        if self.test_env.get("lkt_refactor", True):

            # Run refactor step
            self.refactor()

            # Run test on refactored script
            lkt_result = self.check_run(
                self.build_args(script),
                lkql_path=lkql_path,
                analyze_output=False,
            )

            self.lkt_output = lkt_result.out

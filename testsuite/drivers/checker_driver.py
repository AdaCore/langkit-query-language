import os

from drivers.base_driver import BaseDriver

from e3.testsuite.driver.diff import (
    OutputRefiner,
    PatternSubstitute,
    ReplacePath,
)


class CheckerDriver(BaseDriver):
    """
    This driver runs the LKQL checker with the given arguments and compares
    the checkers's output to the provided output file.

    Test arguments:
        - ``sources`` (list[str]): Source files to analyze;
        - ``rules`` (list[str]): List of rules to run.
        - ``rule_arguments`` (dict[str, dict[str, any]]): A dict mapping each
          rule name to arguments to run it with.
        - ``rule_file`` (str): LKQL rule file to provide to the checker.
        - ``format`` (str): Output format to forward to the LKQL checker.
          Default is "text".
    """

    perf_supported = True

    def run(self) -> None:
        # Get a list with all rule arguments
        full_args_list = []
        for name, args in self.test_env.get("rule_arguments", {}).items():
            for arg, value in args.items():
                full_args_list.append(f"-a={name}.{arg}={value}")

        # Create checker arguments
        args = [
            *self.test_env.get("sources", []),
            *[f"-r={r}" for r in self.test_env.get("rules", [])],
            *full_args_list,
            "--format",
            self.test_env.get("format", "text").upper(),
            "--rules-dir",
            self.test_env["test_dir"],
        ]

        if self.test_env.get("rule_file"):
            args += ["--rule-file", self.test_env["rule_file"]]

        # Run the checker
        if self.perf_mode:
            self.perf_run(args)
        else:
            # Use `catch_error=False` to avoid failing on non-zero status code,
            # as some tests actually exert erroneous behaviors.
            self.check_run(
                self.lkql_checker_exe + args,
                catch_error=False,
                lkql_path=os.environ["LKQL_PATH"],
            )

    @property
    def output_refiners(self) -> list[OutputRefiner]:
        return [
            # Insert this refiner first in the list so that canonicalize_backslashes
            # is run after the following substitution.
            ReplacePath(self.test_env["test_dir"], "<test-dir>"),
            *super().output_refiners,
            PatternSubstitute(r"lkql \d+\.\d*\w* \(.*\)", "lkql <version>"),
        ]

import re

from drivers.base_driver import BaseDriver, Flags


class CheckerDriver(BaseDriver):
    """
    This driver runs the checker with the given arguments and compares the
    checkers's output to the provided output file.

    This driver supports the flag checking procedure.
    Ada lines flagged by the checker must be annotated with `-- FLAG`. The
    number of `FLAG` on the line must be equals to the number of times
    the line is flagged by the driver.

    Test arguments:
        - project: GPR build file to use (if any)
        - input_sources: Ada files to analyze (if explicit, optional if project
          is passed)
        - rule_name: The name of the rule to check
        - rule_arguments: A dict mapping rule argument names to their values
    """

    perf_supported = True
    flag_checking_supported = True

    def run(self) -> None:
        args = [*self.lkql_checker_exe]

        # Use the test's project, if any
        if self.test_env.get('project', None):
            args += ['-P', self.test_env['project']]
        else:
            args += self.test_env['input_sources']

        # Use the wanted charset, if any
        if self.test_env.get('source_charset'):
            args += ['--charset', self.test_env['source_charset']]

        for k, v in self.test_env.get('rule_arguments', {}).items():
            args += ['--rule-arg', '{}={}'.format(k, v)]

        args += ['-r', self.test_env['rule_name']]
        args += ['--rules-dir', self.test_env['test_dir']]

        if self.test_env.get("keep_going_on_missing_file", False):
            args += ['--keep-going-on-missing-file']

        # Run the checker
        if self.perf_mode:
            self.perf_run(args)
        else:
            # Use `catch_error=False` to avoid failing on non-zero status code,
            # as some tests actually exert erroneous behaviors.
            self.check_run(args, catch_error=False)

    def parse_flagged_lines(self, output: str) -> Flags:
        # Compile the pattern to match a checker output
        pattern = re.compile(
            r"^([a-zA-Z][a-zA-Z0-9_\-]*\.(adb|ads)):(\d+):\d+: .*$"
        )

        # Prepare the result
        res = Flags()

        # For each line of the output search the groups in the line
        for line in output.splitlines():
            search_result = pattern.search(line)
            if search_result is not None:
                (file, _, line_num) = search_result.groups()
                res.add_flag(file, int(line_num))

        # Return the result
        return res

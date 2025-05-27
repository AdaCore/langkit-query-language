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
        - auto_fix: If 'True', run the "fix" command for the provided rule
          after the "check" run.
        - auto_fix_mode: This can be "DISPLAY", "NEW_FILE" or "PATCH_FILE" and
          is forwarded to the fix driver if the fix mode is enabled.
    """

    perf_supported = True
    flag_checking_supported = True

    _flag_line_pattern = re.compile(
        rf"^({BaseDriver.ada_file_pattern}):(\d+):\d+: rule violation: .*$"
    )

    def run(self) -> None:
        args = []

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
            self.check_run(
                self.lkql_checker_exe + args,
                catch_error=False
            )

            # If required, run the LKQL fix command
            if self.test_env.get("auto_fix"):
                patched_file_pattern = re.compile(
                    r"^File \"(.*)\" has been patched( \(result in \"(.*)\"\))?$"
                )

                auto_fix_mode = self.test_env.get("auto_fix_mode", "DISPLAY")
                assert auto_fix_mode in ["DISPLAY", "NEW_FILE", "PATCH_FILE"]
                self.check_run(
                    self.lkql_fix_exe + args + ['--auto-fix-mode', auto_fix_mode],
                    check_flags=False,
                    catch_error=False,
                )

                # If the auto-fix mode is "NEW_FILE" or "PATCH_FILE", then
                # display resulting files.
                if auto_fix_mode in ['NEW_FILE', 'PATCH_FILE']:
                    # Get the list of patched files by parsing the output
                    patched_files = []
                    for l in str(self.output).splitlines():
                        search_result = patched_file_pattern.search(l)
                        if search_result is not None:
                            groups = search_result.groups()
                            patched_files.append(groups[2] or groups[0])

                    # Then, for each patched file, display its content
                    for pf in patched_files:
                        with open(self.working_dir(pf), 'r') as f:
                            self.output += f"=== {pf} content:\n"
                            self.output += f.read()

    def parse_flagged_lines(self, output: str) -> Flags:
        # Prepare the result
        res = Flags()

        # For each line of the output search the groups in the line
        for line in output.splitlines():
            search_result = self._flag_line_pattern.search(line)
            if search_result is not None:
                (file, _, line_num) = search_result.groups()
                res.add_flag(file, int(line_num))

        # Return the result
        return res

from drivers.base_driver import BaseDriver


class CheckerDriver(BaseDriver):
    """
    This driver runs the checker with the given arguments and compares the
    checkers's output to the provided output file.

    The expected output must be written in a file called `output`.

    Test arguments:
        - project: GPR build file to use (if any)
        - input_sources: Ada files to analyze (if explicit, optional if project
          is passed)
        - rule_name: The name of the rule to check
        - rule_arguments: A dict mapping rule argument names to their values
    """

    def run(self):
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
        args += ['--rules-dirs', self.test_env['test_dir']]
        args += ['--keep-going-on-missing-file']

        # Run the checker
        self.check_run(args)

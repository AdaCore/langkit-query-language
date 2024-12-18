from drivers.base_driver import BaseDriver


class RefactorDriver(BaseDriver):
    """
    This driver runs an 'lkql refactor' command on a given lkql file, and
    compares the resulting file to the output file.

    The LKQL script to refactor must be placed in a file called 'input.lkql'

    The expected output must be written in a file called `test.out`

    Test arguments:
        - refactoring_name: The name of the refactoring to run
    """

    perf_supported = True

    def run(self) -> None:
        self.check_run([
            *self.command_base, 'refactor',
            '--refactoring', self.test_env['refactoring'], 'input.lkql'
        ])

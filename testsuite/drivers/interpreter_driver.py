from drivers.base_driver import BaseDriver


class InterpreterDriver(BaseDriver):
    """
    This driver runs the interpreter with the given arguments and compares the
    interpreter's output to the provided output file.

    The LKQL script to run must be placed in a file called `script`.
    The expected output must be written in a file called `output`.

    Test arguments:
        - project: GPR build file to use (if any)
    """

    perf_supported = True

    def run(self) -> None:
        # Build the process's arguments list
        args = [*self.lkql_exe, '--script-path', 'script.lkql']

        input_sources = self.test_env.get('input_sources', None)
        project = self.test_env.get('project', None)

        if project:
            args += ['-P', self.test_env['project']]

        if input_sources:
            args += input_sources

        # Run the interpreter
        self.check_run(args)

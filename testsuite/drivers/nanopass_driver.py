from drivers.interpreter_driver import InterpreterDriver


class NanopassDriver(InterpreterDriver):
    """
    This driver runs the interpreter in nanopass mode with the given arguments
    and compares the interpreter's output to the provided output file.

    The LKQL script to run must be placed in a file called `script`.
    The expected output must be written in a file called `output`.

    Test arguments:
        - project: GPR build file to use (if any)
        - input_sources: A list of Ada sources to run analyze with LKQL
        - lkql_path: A list of directories forwarded to the `LKQL_PATH`
            variable when the test is run.
    """

    def base_args(self):
        return self.lkql_pass_exe

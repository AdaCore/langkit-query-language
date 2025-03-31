import os

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
    """

    perf_supported = True
    flag_checking_supported = False

    def base_args(self):
        return self.lkql_exe

    def run(self) -> None:
        # Build the process's arguments list
        args = [*self.base_args(), "--script-path", "script.lkql"]

        auto_rewrite_sources = self.test_env.get('to_lkt', None)

        input_sources = self.test_env.get('input_sources', None)
        project = self.test_env.get('project', None)
        lkql_path = [
            self.working_dir(d)
            for d in self.test_env.get('lkql_path', [])
        ]

        if project:
            args += ['-P', project]

        if input_sources:
            args += input_sources

        # If auto_rewrite_sources are present, then we run the interpreter
        # twice, once with rewrite, and once without, so that we can compare
        # the results.
        if auto_rewrite_sources:
            self.output += "lkql to lkt\n"
            self.output += "===========\n"
            new_args = args + ["--to-lkt"] + auto_rewrite_sources
            self.check_run(new_args)

            self.output += "\n"
            self.output += "lkql run\n"
            self.output += "========\n"

        # Run the interpreter
        self.check_run(args, lkql_path=os.pathsep.join(lkql_path))

import os

from e3.fs import mkdir
from e3.testsuite.driver.diff import DiffTestDriver


def read_to_string(path):
    with open(path, 'r') as f:
        text = f.read()
    return text.strip()


class BaseTestDriver(DiffTestDriver):
    """
    Common code for all test drivers.
    """

    def set_up(self):
        super().set_up()

        if getattr(self.env.options, "coverage", None):
            # Unique number to generate separate trace files in the "shell"
            # method.
            self.trace_counter = 0

            # Create a subdirectory in the global testsuite traces directory
            # and put traces there.
            self.traces_dir = os.path.join(
                self.env.traces_dir, os.path.basename(self.working_dir())
            )
            mkdir(self.traces_dir)

    def shell(self, args, **kwargs):
        env = dict(os.environ)

        # If code coverage is enabled, put trace files in the dedicated
        # directory.
        if getattr(self.env.options, "coverage", None):
            env['LIBLKQLLANG_TRACE_FILE'] = os.path.join(
                self.traces_dir, f'lkql-{self.trace_counter}.srctrace'
            )
            env['GNATCOV_TRACE_FILE'] = os.path.join(
                self.traces_dir, f'prog-{self.trace_counter}.srctrace'
            )

        return super().shell(args, env=env, **kwargs)


class ParserDriver(BaseTestDriver):
    """
    Compare the output of LKQL's `parse` program on the script in the `input`
    file to the expected output in the `output` text file. Tests pass iff the
    output is the same.

    Note that this compares the output of `parse` passing to it the
    `--hide-slocs` option.

    Test arguments:
        - rule: name of the grammar rule to pass to `parse`
    """
    def run(self):
        rule = self.test_env['rule']

        # Run the `parse` program.
        # The `f` option is used to specify a file name, instead of passing
        # the text to parse as a command-line argument.
        # The `r` option is used to choose a starting rule for the parser.
        self.shell(['lkql_parse', '-f', 'input', '-r', rule, '--hide-slocs'])


class InterpreterDriver(BaseTestDriver):
    """
    This driver runs the interpreter with the given arguments and compares the
    interpreter's output to the provided output file.

    The LKQL script to run must be placed in a file called `script`.
    The expected output must be written in a file called `output`.

    Test arguments:
        - project: GPR build file to use (if any)
    """

    def run(self):
        # Build the process's arguments list
        lkql_exe = os.environ["LKQL_EXE"].split("&")
        args = [*lkql_exe, '--script-path', 'script.lkql']

        input_sources = self.test_env.get('input_sources', None)
        project = self.test_env.get('project', None)

        if project:
            args += ['-P', self.test_env['project']]

        if input_sources:
            args += input_sources

        # Run the interpreter
        self.shell(args)


class CheckerDriver(BaseTestDriver):
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
        args = os.environ["LKQL_CHECKER_EXE"].split("&")
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

        # Run the interpreter
        self.shell(args)


def remove_whitespace(text):
    """
    Removes the file names and the empty lines from the given text.
    """
    filtered_lines = [line for line in text.splitlines() if line != ""]
    return '\n'.join(filtered_lines).strip()

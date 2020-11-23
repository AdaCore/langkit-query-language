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

        if self.env.options.coverage:
            # Unique number to generate separate trace files in the "shell"
            # method.
            self.trace_counter = 0

            # Create a subdirectory in the global testsuite traces directory
            # and put traces there.
            self.traces_dir = os.path.join(
                self.env.traces_dir, os.path.basename(self.working_dir())
            )
            mkdir(self.traces_dir)

    def shell(self, args):
        env = dict(os.environ)

        # If code coverage is enabled, put trace files in the dedicated
        # directory.
        if self.env.options.coverage:
            env['LIBLKQLLANG_TRACE_FILE'] = os.path.join(
                self.traces_dir, f'lkql-{self.trace_counter}.srctrace'
            )
            env['GNATCOV_TRACE_FILE'] = os.path.join(
                self.traces_dir, f'prog-{self.trace_counter}.srctrace'
            )

        return super().shell(args, env=env)


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
        - project: relative path of the GPR build file to use (if any), from
                   $root_dir/ada_projects.
    """

    def run(self):
        # Put the absolute path of the test's project in project_path, if any
        if self.test_env['project']:
            project_path = os.path.join(self.env.ada_projects_path,
                                        self.test_env['project'])
        else:
            project_path = ''

        # Build the process's arguments list
        args = [a for a in [
            'lkql_ada', '--script-path', 'script.lkql',
            '-P', project_path
        ] if a != '']

        # Run the interpreter
        self.shell(args)


class CheckerDriver(BaseTestDriver):
    """
    This driver runs the checker with the given arguments and compares the
    checkers's output to the provided output file.

    The LKQL script to run must be placed in a file called `script`.
    The expected output must be written in a file called `output`.

    Test arguments:
        - project: relative path of the GPR build file to use (if any), from
                   $root_dir/ada_projects.
        - files: Ada files to analyze
    """

    def run(self):
        args = ['lkql_checker']
        # Put the absolute path of the test's project in project_path, if any
        if self.test_env.get('project', None):
            args += [
                '-P', os.path.join(self.env.ada_projects_path,
                                   self.test_env['project'])
            ]
        else:
            args += self.test_env['input_sources']

        args += ['-r', self.test_env['rule_name']]

        # Run the interpreter
        self.shell(args)


def remove_whitespace(text):
    """
    Removes the file names and the empty lines from the given text.
    """
    filtered_lines = [line for line in text.splitlines() if line != ""]
    return '\n'.join(filtered_lines).strip()

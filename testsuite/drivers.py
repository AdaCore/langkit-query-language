import os

from e3.testsuite.driver.diff import DiffTestDriver


TESTSUITE_ROOT_DIR = os.path.dirname(os.path.abspath(__file__))
LKQL_ROOT_DIR = os.path.dirname(TESTSUITE_ROOT_DIR)
INTERPRETER_PATH = os.path.join(
    LKQL_ROOT_DIR, 'build', 'bin', 'lkql_ada'
)
CHECKER_PATH = os.path.join(
    LKQL_ROOT_DIR, 'lkql_checker', 'obj', 'dev', 'checker'
)


def read_to_string(path):
    with open(path, 'r') as f:
        text = f.read()
    return text.strip()


class ParserDriver(DiffTestDriver):
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
        parse_bin = ParserDriver.get_parse_path()

        # Run the `parse` program.
        # The `f` option is used to specify a file name, instead of passing
        # the text to parse as a command-line argument.
        # The `r` option is used to choose a starting rule for the parser.
        self.shell([parse_bin, '-f', 'input', '-r', rule, '--hide-slocs'])

    @staticmethod
    def get_parse_path():
        """
        Return the path of the `parse` binary.
        """
        project_root = os.path.dirname(TESTSUITE_ROOT_DIR)
        return os.path.join(
            project_root, 'build', 'bin', 'liblkqllang_parse'
        )


# Path of the test Ada projects
ADA_PROJECTS_PATH = os.path.join(TESTSUITE_ROOT_DIR, 'ada_projects')


class InterpreterDriver(DiffTestDriver):
    """
    This driver runs the interpreter with the given arguments and compares the
    interpreter's output to the provided output file.

    The LKQL script to run must be placed in a file called `script`.
    The expected output must be written in a file called `output`.

    Test arguments:
        - project: relative path of the GPR build file to use (if any), from
                   ADA_PROJECTS_PATH
    """

    def run(self):
        # Put the absolute path of the test's project in project_path, if any
        if self.test_env['project']:
            project_path = os.path.join(ADA_PROJECTS_PATH,
                                        self.test_env['project'])
        else:
            project_path = ''

        # Build the process's arguments list
        args = [a for a in [
            INTERPRETER_PATH, '--script-path', 'script.lkql',
            '-P', project_path
        ] if a != '']

        # Run the interpreter
        self.shell(args)


class CheckerDriver(DiffTestDriver):
    """
    This driver runs the checker with the given arguments and compares the
    checkers's output to the provided output file.

    The LKQL script to run must be placed in a file called `script`.
    The expected output must be written in a file called `output`.

    Test arguments:
        - project: relative path of the GPR build file to use (if any), from
                   ADA_PROJECTS_PATH
        - files: Ada files to analyze
    """

    def run(self):
        args = [CHECKER_PATH]
        # Put the absolute path of the test's project in project_path, if any
        if self.test_env.get('project', None):
            args += [
                '-P', os.path.join(ADA_PROJECTS_PATH, self.test_env['project'])
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

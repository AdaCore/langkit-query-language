from util import read_to_string
from e3.testsuite.driver import BasicTestDriver
from e3.testsuite.process import Run
from e3.testsuite.result import TestStatus
import os
import sys


def get_interpreter_path():
    """
    Return the path of the interpreter's binary.
    The debug binary will be used if the release binary can't be located.
    """
    project_root = os.path.dirname(os.getcwd())
    interpreter_bin = os.path.join(project_root, 'obj', 'main')
    interpreter_debug_bin = os.path.join(project_root, 'obj', 'debug', 'main')
    if os.path.exists(interpreter_bin):
        return interpreter_bin
    elif os.path.exists(interpreter_debug_bin):
        return interpreter_debug_bin
    else:
        sys.stderr.write("Interpreter binary unavailable !")
        sys.exit(1)


# Path of the test Ada projects
ADA_PROJECTS_PATH = os.path.join(
    os.path.dirname(os.getcwd()), 'testsuite', 'ada_projects'
)

INTERPRETER_PATH = get_interpreter_path()


class InterpreterDriver(BasicTestDriver):
    """
    This driver runs the interpreter with the given arguments and compares the
    interpreter's output to the provided output file.

    The LKQL script to run must be placed in a file called `script`.
    The expected output must be written in a file called `output`.

    Test arguments:
        - project: relative path of the GPR build file to use (if any), from
                   ADA_PROJECTS_PATH
        - failure (optional): True if the interpreter's execution must end with
                              a non-zero exit code, False otherwise
    """

    def run(self, prev):
        test_dir = os.path.dirname(self.test_env['test_case_file'])
        script_path = os.path.join(test_dir, 'script')
        expected = read_to_string(os.path.join(test_dir, 'output')).strip('\n')

        failure_expected = self.test_env.get('failure', False)

        # Put the absolute path of the test's project in project_path, if any
        if self.test_env['project']:
            project_path = os.path.join(ADA_PROJECTS_PATH,
                                        self.test_env['project'])
        else:
            project_path = ''

        # Build the process's arguments list
        args = [a for a in [INTERPRETER_PATH, script_path, project_path]
                if a != '']

        status = TestStatus.PASS

        # Run the interpreter
        process = Run(args)
        process_output = remove_paths_and_whitespace(process.out)
        process_failure = process.status != 0

        # If there is a difference between the process's output and the
        # expected output, or between the process's exit status and the
        # expected exit status, the test fails.
        if process_output != expected or process_failure != failure_expected:
            status = TestStatus.FAIL

        self.result.set_status(status)
        self.result.env['output'] = process_output
        self.result.env['expected'] = expected
        self.push_result()

    def analyze(self, prev):
        pass


def whitespace_or_file(text):
    """
    Returns True if the given text is empty or ends with .ad[bs].
    """
    return text == "" or text.endswith('.adb') or text.endswith('.ads')


def remove_paths_and_whitespace(text):
    """
    Removes the file names and the empty lines from the given text.
    """
    filtered_lines = [l for l in text.splitlines()
                      if not whitespace_or_file(l)]
    return '\n'.join(filtered_lines)

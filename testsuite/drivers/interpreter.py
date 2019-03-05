from util import read_to_string
from e3.testsuite.driver import BasicTestDriver
from e3.testsuite.process import Run
from e3.testsuite.result import TestStatus
import os
import re


def get_interpreter_path():
    project_root = os.path.dirname(os.getcwd())
    interpreter_bin = os.path.join(project_root, 'obj', 'main')
    if not os.path.exists(interpreter_bin):
        return os.path.join(project_root, 'obj', 'debug', 'main')
    return interpreter_bin


ADA_PROJECTS_PATH = os.path.join(os.path.dirname(os.getcwd()), 'testsuite', 'ada_projects')

INTERPRETER_PATH = get_interpreter_path()


class InterpreterDriver(BasicTestDriver):
    def run(self, prev):
        test_dir = os.path.dirname(self.test_env['test_case_file'])
        script_path = os.path.join(test_dir, 'script')

        expected = read_to_string(os.path.join(test_dir, 'output')).strip('\n')
        status = TestStatus.PASS
        failure_expected = self.test_env.get('failure', False)
        if self.test_env['project']:
            project_path = os.path.join(ADA_PROJECTS_PATH, self.test_env['project'])
        else:
            project_path = ''
        args = [a for a in [INTERPRETER_PATH, script_path, project_path] if a != '']

        process = Run(args)
        process_output = remove_paths_and_whitespace(process.out)
        process_failure = process.status != 0

        if process_failure != failure_expected:
            status = TestStatus.ERROR

        if process_output != expected:
            status = TestStatus.FAIL

        self.result.set_status(status)
        self.result.env['output'] = process_output
        self.result.env['expected'] = expected
        self.push_result()

    def analyze(self, prev):
        pass


def whitespace_or_file(text):
    return text == "" or text.endswith('.adb') or text.endswith('.ads')


def remove_paths_and_whitespace(text):
    filtered_lines = [l for l in text.splitlines() if not whitespace_or_file(l)]
    return '\n'.join(filtered_lines)

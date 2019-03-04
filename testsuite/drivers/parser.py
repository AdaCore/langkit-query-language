from util import read_to_string
from e3.testsuite.driver import BasicTestDriver
from e3.testsuite.process import Run
from e3.testsuite.result import TestStatus
import os


class ParserDriver(BasicTestDriver):

    def analyze(self, prev):
        pass

    def run(self, previous_values):
        test_dir = os.path.dirname(self.test_env['test_case_file'])
        test_file = os.path.join(test_dir, 'input')
        output_file = os.path.join(test_dir, 'output')
        rule = self.test_env['rule']
        parse_bin = ParserDriver.get_parse_path()

        status = TestStatus.PASS

        process = Run([parse_bin, '-f', test_file, '-r', rule, '--hide-slocs'])

        if process.err is not None:
            status = TestStatus.ERROR

        output = process.out.strip()
        expected = read_to_string(output_file)

        if output != expected:
            self.result.env['expected'] = expected
            self.result.env['output'] = output
            status = TestStatus.FAIL

        self.result.set_status(status)
        self.push_result()

    @staticmethod
    def get_parse_path():
        project_root = os.path.dirname(os.getcwd())
        return os.path.join(project_root, 'lkql', 'build', 'bin', 'parse')

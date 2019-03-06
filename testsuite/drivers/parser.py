from util import read_to_string
from e3.testsuite.driver import BasicTestDriver
from e3.testsuite.process import Run
from e3.testsuite.result import TestStatus
import os


class ParserDriver(BasicTestDriver):
    """
    This driver used the `parse` binary to produce an AST an compare it with
    an AST provided by the user.
    The provided AST must respect `parse`'s output format, with --hide-slocs enabled.

    Test arguments:
        - rule: starting rule of the parser
    """
    def analyze(self, prev):
        pass

    def run(self, previous_values):
        test_dir = os.path.dirname(self.test_env['test_case_file'])
        test_file = os.path.join(test_dir, 'input')
        output_file = os.path.join(test_dir, 'output')
        rule = self.test_env['rule']
        parse_bin = ParserDriver.get_parse_path()

        status = TestStatus.PASS

        # run the `parse` program.
        # The `f` option is used to specify a file name, instead of passing the text
        # to parse as a command-line argument.
        # The `r` option is used to choose a starting rule for the parser.
        process = Run([parse_bin, '-f', test_file, '-r', rule, '--hide-slocs'])

        if process.err is not None:
            status = TestStatus.ERROR
        else:
            output = process.out.strip()
            expected = read_to_string(output_file)

            self.result.env['expected'] = expected
            self.result.env['output'] = output

            if output != expected:
                status = TestStatus.FAIL

        self.result.set_status(status)
        self.push_result()

    @staticmethod
    def get_parse_path():
        """
        Return the path of the `parse` binary.
        """
        project_root = os.path.dirname(os.getcwd())
        return os.path.join(project_root, 'lkql', 'build', 'bin', 'parse')

import os
import liblkqllang
from util import read_to_string
from e3.testsuite.result import TestStatus
from e3.testsuite.driver import BasicTestDriver


class TypeCheckDriver(BasicTestDriver):
    """
    Print the type_name of every top_level node (declaration or expression)
    contained in a given LKQL script.
    """

    def analyze(self, prev):
        pass

    def run(self, previous_values):
        test_dir = os.path.dirname(self.test_env['test_case_file'])
        context = liblkqllang.AnalysisContext()
        unit = context.get_from_file(os.path.join(test_dir, 'script'))
        expected = read_to_string(os.path.join(test_dir, 'output'))

        for (node, expected_type) in zip(unit.root, expected.split('\n')):
            if node.p_type_name != expected_type:
                self.result.set_status(TestStatus.FAIL)
                self.result.env['literal'] = str(node.text)
                self.result.env['expected'] = expected_type
                self.result.env['actual'] = str(node.p_type_name)
                self.push_result()
                return

        self.result.set_status(TestStatus.PASS)
        self.push_result()

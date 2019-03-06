from e3.testsuite import Testsuite
from drivers import ParserDriver, InterpreterDriver, make_interpreter
import os

TESTSUITE_ROOT_DIR = os.path.dirname(
    os.path.dirname(os.path.abspath(__file__))
)

INTERPRETER_PATH = os.path.join(TESTSUITE_ROOT_DIR, 'build', 'obj', 'main')

INTERPRETER_GPR_FILE = os.path.join(os.path.dirname(os.getcwd()), 'lkql_interpreter.gpr')


class LKQLTestsuite(Testsuite):
    DRIVERS = {'parser': ParserDriver,
               'interpreter': InterpreterDriver}


if __name__ == "__main__":
    make_interpreter(True)
    suite = LKQLTestsuite(os.path.dirname(__file__))
    suite.testsuite_main()
    for k, v in suite.test_status_counters.iteritems():
        if v != 0:
            print "%s: %d" % (k, v)

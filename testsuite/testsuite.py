from e3.testsuite import Testsuite
from drivers import ParserDriver, InterpreterDriver
import os
import subprocess

INTERPRETER_GPR_FILE = os.path.join(os.path.dirname(os.getcwd()), 'lkql_interpreter.gpr')


class LKQLTestsuite(Testsuite):
    DRIVERS = {'parser': ParserDriver,
               'interpreter': InterpreterDriver}


if __name__ == "__main__":
    subprocess.call(['gprbuild', INTERPRETER_GPR_FILE])
    suite = LKQLTestsuite(os.path.dirname(__file__))
    suite.testsuite_main()
    for k, v in suite.test_status_counters.iteritems():
        if v != 0:
            print "%s: %d" % (k, v)
    subprocess.call(["dot", "-Tpng", "out/new/tests.dot", "-o", "out/out.png"])

from e3.testsuite import Testsuite
from drivers import ParserDriver
import os
import subprocess


class LKQLTestsuite(Testsuite):
    DRIVERS = {'parser': ParserDriver}


if __name__ == "__main__":
    suite = LKQLTestsuite(os.path.dirname(__file__))
    suite.testsuite_main()
    for k, v in suite.test_status_counters.iteritems():
        if v != 0:
            print "%s: %d" % (k, v)
    subprocess.call(["dot", "-Tpng", "out/new/tests.dot", "-o", "out/out.png"])
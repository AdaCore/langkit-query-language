from e3.testsuite import Testsuite
from e3.os.process import Run
from drivers import (ParserDriver, InterpreterDriver, make_interpreter,
    TypeCheckDriver
)
import os
import glob
import sys

TESTSUITE_ROOT_DIR = os.path.dirname(
    os.path.dirname(os.path.abspath(__file__))
)

OBJ_DIR = os.path.join('build', 'obj')


def run_gcov():
    """
    Run `gcov` on the .gcda files produced during test execution
    """
    gcda_files = glob.glob(os.path.join(OBJ_DIR, '*.gcda'))

    for f in gcda_files:
        # Local path of the current .gcda file from the `out` directory,
        # where `gcov` is run
        path_from_out = os.path.join('..', f)
        p = Run(['gcov', path_from_out], cwd='out')
        if p.status != 0:
            sys.stderr.write(p.out)


class LKQLTestsuite(Testsuite):
    DRIVERS = {'parser': ParserDriver,
               'interpreter': InterpreterDriver,
               'type_checker': TypeCheckDriver}


if __name__ == "__main__":
    make_interpreter(True)
    suite = LKQLTestsuite(os.path.dirname(__file__))
    suite.testsuite_main()
    for k, v in suite.test_status_counters.iteritems():
        if v != 0:
            print "%s: %d" % (k, v)
    run_gcov()

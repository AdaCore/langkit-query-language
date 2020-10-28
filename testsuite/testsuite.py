from e3.testsuite import Testsuite
from drivers import (
    CheckerDriver, ParserDriver, InterpreterDriver
)
import os

TESTSUITE_ROOT_DIR = os.path.dirname(
    os.path.dirname(os.path.abspath(__file__))
)

OBJ_DIR = os.path.join('build', 'obj')


class LKQLTestsuite(Testsuite):
    test_driver_map = {'parser': ParserDriver,
                       'interpreter': InterpreterDriver,
                       'checker': CheckerDriver}

    def add_options(self, parser):
        parser.add_argument(
            '--rewrite', '-r', action='store_true',
            help='Rewrite test baselines according to current output.'
        )

    def set_up(self):
        super().set_up()
        self.env.rewrite_baselines = self.env.options.rewrite


if __name__ == "__main__":
    suite = LKQLTestsuite(os.path.dirname(__file__))
    suite.testsuite_main()
    for k, v in suite.test_status_counters.items():
        if v != 0:
            print(f"{k}: {v}")

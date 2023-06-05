import os
import os.path as P

from e3.fs import mkdir
from e3.testsuite.driver.diff import DiffTestDriver


class BaseDriver(DiffTestDriver):
    """
    Common code for all test drivers.
    """

    def set_up(self):
        super().set_up()

        if self.env.options.coverage:
            # Unique number to generate separate trace files in the "shell"
            # method.
            self.trace_counter = 0

            # Create a subdirectory in the global testsuite traces directory
            # and put traces there.
            self.traces_dir = P.join(
                self.env.traces_dir, P.basename(self.working_dir())
            )
            mkdir(self.traces_dir)

    def shell(self, args, **kwargs):
        env = dict(os.environ)

        # If code coverage is enabled, put trace files in the dedicated
        # directory.
        if self.env.options.coverage:
            env['LIBLKQLLANG_TRACE_FILE'] = P.join(
                self.traces_dir, f'lkql-{self.trace_counter}.srctrace'
            )
            env['GNATCOV_TRACE_FILE'] = P.join(
                self.traces_dir, f'prog-{self.trace_counter}.srctrace'
            )

        return super().shell(args, env=env, **kwargs)

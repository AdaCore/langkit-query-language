import os
import os.path as P

from e3.fs import mkdir
from e3.testsuite.driver.diff import DiffTestDriver
from e3.testsuite.driver.classic import TestAbortWithError


class BaseDriver(DiffTestDriver):
    """
    Common code for all test drivers.
    """

    perf_supported = False

    @property
    def lkql_exe(self):
        """
        Return the LKQL interpreter executable.
        """
        return self.env.lkql_exe

    @property
    def lkql_checker_exe(self):
        """
        Return the LKQL checker executable.
        """
        return self.env.lkql_checker_exe

    @property
    def perf_mode(self):
        return self.env.perf_mode

    @property
    def baseline(self):
        # In perf mode, our purpose is to measure performance, not to check
        # results.
        return (None, "", False) if self.perf_mode else super().baseline

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

    def check_run(self, args, **kwargs):
        """
        Run a process and check that its output is the expected one.
        """
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

        return self.shell(args, env=env, **kwargs)

    def perf_run(self, args):
        """
        Run a process and collect performance data from it.

        Time and memory metrics go to the ``TestResult.info`` table:

        * The "time" entry contains a space-separated list of floats for the
          time in seconds it took to run each instance of the subprocess.

        * The "memory" entry contains a space-separated list of integers for
          the approximative number of bytes each instance of the subprocess
          allocated.

        When created, file for time profile go to the provided perf directory
        and the corresponding file name are stored in the "time-profile" entry
        in the ``TestResult.info`` table.
        """
        env = dict(os.environ)
        perf_dir = self.env.perf_dir
        cwd = self.working_dir()

        def run(*prefix):
            """
            Run the process with the given prefix additional argument and
            return the output.
            """
            return self.shell(
                args=list(prefix) + args,
                cwd=cwd,
                env=env,
                analyze_output=False,
                catch_error=False,
            ).out

        # For each performance measuring mode execute the needed process
        for mode, param in self.test_env["perf"].items():
            # If the mode is the default on, just run the "time" tool
            if mode == "default":
                times = []
                memories = []
                for i in range(param):
                    result = run("time", "-f", "%M %e")
                    memory, time = result.split()[-2:]
                    times.append(time)
                    memories.append(memory)
                self.result.info["time"] = " ".join(times)
                self.result.info["memory"] = " ".join(memories)

            # If we explicitly asked for not profiling just continue
            elif self.env.perf_no_profile:
                continue

            # If the mode is time profiling, use the "perf" tool
            elif mode == "profile-time":
                perf_filename = f"{self.test_name}__perf.data"
                run(
                    "perf",
                    "record",
                    "--call-graph=dwarf",
                    "-F100",
                    "-o",
                    P.join(perf_dir, perf_filename),
                    "--"
                )
                self.result.info["time-profile"] = perf_filename

            # Else the test mode is invalid
            else:
                raise TestAbortWithError(f"invalid perf mode: {mode}")

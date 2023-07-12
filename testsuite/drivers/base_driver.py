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
    def perf_mode(self):
        return hasattr(self.env, 'perf_mode') and self.env.perf_mode

    @property
    def baseline(self):
        # In perf mode, our purpose is to measure performance, not to check
        # results.
        return (None, "", False) if self.perf_mode else super().baseline

    def set_up(self):
        super().set_up()

        self.define_lkql_executables()

        if hasattr(self.env.options, 'coverage') and self.env.options.coverage:
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

    def define_lkql_executables(self):
        # If the mode is Ada
        if self.env.options.mode == "ada":
            self.lkql_exe = ["lkql_ada"]
            self.lkql_checker_exe = ["lkql_checker"]
            self.gnatcheck_worker_exe = ["gnatcheck"]

        # If the mode is JIT
        elif self.env.options.mode == "jit":
            # Prepare the command for calling Java with the LKQL JIT implementation
            graal_home = os.environ['GRAAL_HOME']
            lkql_jit_home = os.environ.get(
                'LKQL_JIT_HOME',
                P.join(graal_home, 'languages', 'lkql')
            )

            # Get the Java executable
            java = (
                P.join(graal_home, 'bin', 'java.exe')
                if os.name == 'nt' else
                P.join(graal_home, 'bin', 'java')
            )

            # Create the class path
            base_class_path = [
                P.join(graal_home, 'lib', 'truffle', 'truffle-api.jar'),
                P.join(lkql_jit_home, 'lkql_jit.jar'),
            ]
            launcher_class_path = os.pathsep.join(base_class_path + [
                P.join(lkql_jit_home, 'lkql_jit_launcher.jar')
            ])
            checker_class_path = os.pathsep.join(base_class_path + [
                P.join(lkql_jit_home, 'lkql_jit_checker.jar')
            ])
            gnatcheck_worker_class_path = os.pathsep.join(base_class_path + [
                P.join(lkql_jit_home, 'gnatcheck_worker.jar')
            ])

            # Get the java.library.path
            java_library_path = (
                os.environ.get('PATH', "")
                if os.name == 'nt' else
                os.environ.get('LD_LIBRARY_PATH', "")
            )

            java_defs = [
                f'-Djava.library.path={java_library_path}',
                f'-Dtruffle.class.path.append={P.join(lkql_jit_home, "lkql_jit.jar")}',
            ]

            # Prepare the Java commands
            launcher_cmd = [
                java, "-cp", launcher_class_path
            ] + java_defs + ["com.adacore.lkql_jit.LKQLLauncher"]

            checker_cmd = [
                java, "-cp", checker_class_path
            ] + java_defs + ["com.adacore.lkql_jit.LKQLChecker"]

            worker_cmd = [
                java, "-cp", gnatcheck_worker_class_path
            ] + java_defs + ["com.adacore.lkql_jit.GNATCheckWorker"]

            self.lkql_exe = launcher_cmd
            self.lkql_checker_exe = checker_cmd
            self.gnatcheck_worker_exe = worker_cmd

        # If the mode is native JIT
        elif self.env.options.mode == "native_jit":
            self.lkql_exe = ["native_lkql_jit"]
            self.lkql_checker_exe = ["native_lkql_jit_checker"]
            self.gnatcheck_worker_exe = ["native_gnatcheck_worker"]

import glob
import os
import os.path as P
import re
import sys
from typing import TextIO

from e3.fs import mkdir
from e3.testsuite.control import YAMLTestControlCreator
from e3.testsuite.driver.diff import DiffTestDriver
from e3.testsuite.driver.classic import (
    TestAbortWithError, ProcessResult, TestSkip
)


_flag_pattern = re.compile(r"--\s*FLAG\s*(\((\d+)\))?\s*(.*)")
_noflag_pattern = re.compile(r"--\s*NOFLAG")
_ada_source_encodings = ['utf-8', 'iso-8859-1']


def read_ada_file(file_name: str) -> tuple[list[str], str]:
    """
    Read the given Ada file with an automatic encoding detection strategy.
    Returns a tuple containing the lines of the file and its encoding.

    :param file_name: The Ada file to read.
    """
    with open(file_name, mode='rb') as ada_file:
        ada_bytes = ada_file.read()
        for encoding in _ada_source_encodings:
            try:
                lines = ada_bytes.decode(encoding).split('\n')
                lines = lines[:-1] if not lines[-1] else lines
                return (lines, encoding)
            except ValueError as _:
                pass


class Flags:
    """
    Represents the flags in the source files.
    """

    _flags: dict[str, list[int]]

    def __init__(self) -> None:
        self._flags = {}

    def add_flag(self, file: str, line: int):
        """
        Add a flag to the file.

        :param file: The file to add a flag for.
        :param line: The line to flag in the file.
        """
        current_flags = self._flags.get(file, [])
        current_flags.append(line)
        self._flags[file] = current_flags

    def add_flags(self, file: str, lines: list[int]):
        """
        Add all flags to the file.

        :param file: The file to add the flags to.
        :param flags: The flags to add.
        """
        if len(lines) > 0:
            current_flags = self._flags.get(file, [])
            current_flags.extend(lines)
            self._flags[file] = current_flags

    def add_other_flags(self, other):
        """
        Add all flags from the other Flags object.

        :param other: The other flags object to add from.
        """
        for file, lines in other.items():
            if len(lines) > 0:
                current_flags = self._flags.get(file, [])
                current_flags.extend(lines)
                self._flags[file] = current_flags

    def get_files(self) -> set[str]:
        """
        Get the files which have flags.
        """
        return self._flags.keys()

    def has_flags(self, file: str) -> bool:
        """
        Get if the file has some flags.

        :param file: The file to check the flags for.
        """
        return file in self._flags.keys()

    def get_flags(self, file: str) -> list[int]:
        """
        Get the flags associated to the file.

        :param file: The file to get the flags for.
        """
        return self._flags.get(file,  [])

    def items(self) -> set[tuple[str, list[int]]]:
        return self._flags.items()

    def __str__(self) -> str:
        return str(self._flags)

    def __bool__(self) -> bool:
        return len(self._flags) > 0


class BaseDriver(DiffTestDriver):
    """
    Common code for all test drivers.
    """

    perf_supported = False
    flag_checking_supported = False

    @property
    def perf_mode(self) -> bool:
        return hasattr(self.env, 'perf_mode') and self.env.perf_mode

    @property
    def flag_checking(self) -> bool:
        return ((not self.env.options.no_flag_checking) and
                self.test_env.get("check_flags", True))

    @property
    def baseline(self) -> tuple[str, str, bool]:
        # In perf mode, our purpose is to measure performance, not to check
        # results.
        return (None, "", False) if self.perf_mode else super().baseline

    @property
    def test_control_creator(self):
        return YAMLTestControlCreator({
            'mode': self.env.options.mode,
            'os': self.env.build.os.name,
        })

    def set_up(self) -> None:
        super().set_up()

        # If requested, skip internal testcases
        if (
            hasattr(self.env.options, "skip_internal_tests") and
            self.env.options.skip_internal_tests and
            self.test_env['test_name'].startswith('internal__')
        ):
            raise TestSkip('Skipping internal testcase')

        self._define_lkql_executables()

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

    def check_run(self, args: list[str], **kwargs) -> ProcessResult:
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

        if self.flag_checking_supported and self.flag_checking:
            run_result = self.shell(args, env=env, **kwargs)
            self.check_flags(self.parse_flagged_lines(run_result.out))
            return run_result
        else:
            return self.shell(args, env=env, **kwargs)

    def perf_run(self, args: list[str]) -> None:
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

    def parse_flagged_lines(self, output: str) -> Flags:
        """
        Parse the output of a call and return an object containing files
        and their flagged lines.

        :param output: The process output to parse.
        """
        raise TestAbortWithError("Unimplemented output parsing method")

    def check_flags(self, execution_flags: Flags) -> None:
        """
        Check that the source flagged lines are correctly flagged by the test
        driver, otherwise display the differences and add missing FLAG if
        asked for.

        :param execution_flags: The source lines flagged by the output parsing.
        """
        flag_annotations, noflag_annotations = self.count_source_flags()
        missing_flags, noflag_violations, extra_flags = self.compare_flagged_lines(execution_flags, flag_annotations, noflag_annotations)
        self.display_flag_errors(missing_flags, noflag_violations, extra_flags)
        if self.env.options.add_missing_flags:
            self.add_missing_flags(missing_flags)

    def count_source_flags(self) -> tuple[Flags, Flags]:
        """
        Count the FLAG/NOFLAG annotations in the test Ada sources and return a tuple
        containing (flag_annotations, noflag_annotation)
        """
        # Get all Ada sources of the test
        ada_sources = glob.glob(P.join(self.test_dir(), "**/*.ad*"), recursive=True)

        # Prepare the result
        flag_annotations = Flags()
        noflag_annotations = Flags()

        # For each file, read it and parse the annotations
        for ada_file_name in ada_sources:
            base_name = P.basename(ada_file_name)
            ada_lines, _ = read_ada_file(ada_file_name)
            for line_num, line in enumerate(ada_lines, 1):
                flag_search = _flag_pattern.search(line)
                if flag_search:
                    count = int(flag_search.group(2)) if flag_search.group(2) else 1
                    flag_annotations.add_flags(base_name, [line_num] * count)
                elif _noflag_pattern.search(line):
                    noflag_annotations.add_flag(base_name, line_num)

        # Return the tuple result
        return (flag_annotations, noflag_annotations)

    def compare_flagged_lines(self,
                              execution_flags: Flags,
                              flag_annotations: Flags,
                              noflag_annotations: Flags) -> tuple[Flags, Flags, Flags]:
        """
        Check that the flagged lines are correcty annotated. Return a tuple
        containing missing flags, noflag violations and extra flag annotations.

        :param execution_flags: The lines flagged by the output parsing.
        :param flag_annotations: The FLAG annotations in the ada sources.
        :param noflag_annotations: The NOFLAG annotations in the ada sources.
        """
        # Prepare the result objects
        missing_flags = Flags()
        noflag_violations = Flags()
        extra_flags = Flags()

        # For each file with execution flags
        for file_name, lines in execution_flags.items():
            source_flags = list(flag_annotations.get_flags(file_name))
            source_noflags = noflag_annotations.get_flags(file_name)
            for line in lines:
                if line in source_noflags:
                    noflag_violations.add_flag(file_name, line)
                elif line in source_flags:
                    source_flags.remove(line)
                else:
                    missing_flags.add_flag(file_name, line)
            extra_flags.add_flags(file_name, source_flags)

        # Return the results
        return (missing_flags, noflag_violations, extra_flags)

    def display_flag_errors(self,
                            missing_flags: Flags,
                            noflag_violations: Flags,
                            extra_flags: Flags) -> None:
        """
        Display the flag annotations violations on the test output to force
        a failure and show the user flag violations.

        :param missing_flags: The missing flag annotations.
        :param noflag_violation: The 'NOFLAG' annotation violations
        :param extra_flags: The extra 'FLAG' annotations.
        """
        # If there is extra of missing flag create a message with the
        # information and add it to the test output.
        if missing_flags or noflag_violations or extra_flags:
            failure_message = f"Errors in Ada sources FLAG annotations!{os.linesep * 2}"

            # Function to display failure flags
            def format_failure_flags(flags):
                res = ""
                for file in flags.get_files():
                    lines = flags.get_flags(file)
                    for line in sorted(list(set(lines))):
                        count = lines.count(line)
                        res += (
                            f"  {file} at line {line}"
                            f"{f' ({count} times)' if count > 1 else ''}"
                            f"{os.linesep}"
                        )
                return res

            # Output NOFLAG violations
            if noflag_violations:
                failure_message += (f"'NOFLAG' violations:{os.linesep}"
                                    f"{format_failure_flags(noflag_violations)}")

            # Output all missing flags
            if missing_flags:
                failure_message += (f"Missing 'FLAG' annotations:{os.linesep}"
                                    f"{format_failure_flags(missing_flags)}")

            # Output all extra flags
            if extra_flags:
                failure_message += (f"Extra 'FLAG' annotations:{os.linesep}"
                                    f"{format_failure_flags(extra_flags)}")

            self.output += failure_message

    def add_missing_flags(self, missing_flags: Flags) -> None:
        """
        Add the missing "-- FLAG" annotations in the test ada sources.

        :param missing_flags: The flags which are missing from the source.
        """
        # Get all Ada sources of the test
        ada_sources = glob.glob(P.join(self.test_dir(), "**/*.ad*"), recursive=True)

        # For each source if there a missing flag add it
        for ada_file_name in ada_sources:
            base_name = P.basename(ada_file_name)

            # If the file has some missing flags
            if missing_flags.has_flags(base_name):
                # Get the file lines and encoding
                file_lines = []
                ada_lines, file_encoding = read_ada_file(ada_file_name)
                file_lines = [line.rstrip() for line in ada_lines]

                # For each missing flag we add in in the file lines
                for line_num in set(missing_flags.get_flags(base_name)):
                    count = missing_flags.get_flags(base_name).count(line_num)

                    # Split the line to get the possible existing comment
                    split = file_lines[line_num - 1].split("--")
                    code, comment = split[0], split[1].strip() if len(split) == 2 else ""

                    # If there is already a "FLAG" annotation we have to count the number of it then rewrite it
                    flag_search = _flag_pattern.search(f"-- {comment}")
                    if flag_search:
                        count += int(flag_search.group(2) if flag_search.group(2) else 1)
                        comment = flag_search.group(3)

                    # Join the fetched Ada statement and the comment with the "FLAG" annotation
                    file_lines[line_num - 1] = "--".join((
                        f"{code} " if code and code[-1].strip() else code,
                        f"  FLAG{f' ({count})' if count > 1 else ''}{f' {comment}' if comment else ''}"
                    ))

                # Rewrite the file with the missing FLAG annotations
                with open(ada_file_name, 'w', encoding=file_encoding) as ada_file:
                    for line in file_lines:
                        print(line, file=ada_file)

    def _define_lkql_executables(self) -> None:
        # If the mode is JIT
        if self.env.options.mode == "jit":
            python_wrapper = P.join(self.env.support_dir, "lkql_jit.py")
            command_base = [sys.executable, python_wrapper]
            self.lkql_exe = [*command_base, "run"]
            self.lkql_checker_exe = [*command_base, "check"]
            self.gnatcheck_worker_exe = [*command_base, "gnatcheck_worker"]

        # If the mode is native JIT
        elif self.env.options.mode == "native_jit":
            self.lkql_exe = ["lkql", "run"]
            self.lkql_checker_exe = ["lkql", "check"]
            self.gnatcheck_worker_exe = ["lkql", "gnatcheck_worker"]

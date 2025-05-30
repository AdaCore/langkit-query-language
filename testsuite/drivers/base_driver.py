from dataclasses import dataclass
import errno
import glob
import os
import os.path as P
import re
import select
import subprocess
import sys

from e3.fs import mkdir
from e3.testsuite.control import YAMLTestControlCreator
from e3.testsuite.driver.diff import (
    DiffTestDriver,
    Substitute,
    OutputRefiner,
)
from e3.testsuite.driver.classic import (
    TestAbortWithError, ProcessResult, TestSkip
)


@dataclass
class TaggedLines:
    """
    This class represents a set of lines that are tagged in a given buffer.
    Each line may be tagged one or more times.
    """

    tagged_lines: dict[int, int]

    def __init__(self):
        self.tagged_lines = {}

    def tag_count(self, line: int) -> int:
        """
        Get the count of tags on the provided `line`.
        """
        return self.tagged_lines.get(line, 0)

    def tag_line(self, line: int, n: int = 1):
        """
        Tag the `line` `n`th times.
        """
        self.tagged_lines[line] = self.tagged_lines.get(line, 0) + n

    def combine(self, other):
        res = TaggedLines()
        res.tagged_lines = dict(self.tagged_lines)
        for line, count in other.tagged_lines.items():
            res.tag_line(line, count)
        return res


@dataclass
class FileFlags:

    file_name: str
    """
    The base name of the file.
    """

    matching_sources: list[str]
    """
    Path to Ada source files which match the `file_name`.
    """

    flag_annotations: TaggedLines
    """
    All lines that are annotated with `FLAG`. The same line can be annotated
    multiple times.
    """

    noflag_annotations: TaggedLines
    """
    All lines that are annotated with `NOFLAG`.
    """

    flagged_lines: TaggedLines
    """
    Lines that have been flagged by running the test.
    """

    @property
    def errors(self) -> list[tuple[int, str]]:
        """
        Get all errors by comparing `FLAG/NOFLAG` annotations and lines flagged
        by the test run. Each element of the result list is a tuple containing
        the number of the line with an error associated to the error message.
        """
        res: list[tuple[int, str]] = []

        for line, count in self.flagged_lines.tagged_lines.items():
            flag_count = self.flag_annotations.tag_count(line)
            if self.noflag_annotations.tag_count(line) != 0:
                res.append((line, "'NOFLAG' annotation violated"))
            elif flag_count == 0:
                res.append((
                    line,
                    f"no 'FLAG' annotation (line flagged {count} time(s))"
                ))
            elif flag_count != count:
                res.append((
                    line,
                    f"unexpected flag count (expecting {flag_count}, actual {count})"
                ))

        for line, count in self.flag_annotations.tagged_lines.items():
            if self.flagged_lines.tag_count(line) == 0:
                res.append((
                    line, f"line is never flagged (expecting {count} time(s))"
                ))

        return sorted(res, key=lambda t: t[0])

    def add_missing_flags(self):
        """
        Add the missing 'FLAG' annotations in the file designated by this
        instance.

        :param searching dir
        """
        # Get the missing flag annotations
        missing_flag_annotations = {
            line: count
            for line, count in self.flagged_lines.tagged_lines.items()
            if self.flag_annotations.tag_count(line) < count
        }

        # If there is no missing 'FLAG' just exit the function
        if len(missing_flag_annotations) < 1:
            return

        # Get the source file to rewrite, ensuring there is only one matching
        # source.
        if len(self.matching_sources) > 1:
            raise TestAbortWithError(
                f"Cannot automatically add flags annotations in {self.file_name}, multiple sources are matching this name"
            )
        source = self.matching_sources[0]

        # Get the file lines and encoding
        source_lines = []
        source_lines, source_encoding = BaseDriver.read_ada_file(source)

        # For each line where an annotation is missing, add it before any other comment
        for line, count in missing_flag_annotations.items():
            # Split the source line to get the code and the comment parts
            line_split = source_lines[line - 1].split("--")
            code, comment = line_split[0], line_split[1].strip() if len(line_split) == 2 else ""

            # Remove the already existing FLAG annotation if there is one
            search_res = re.search(r"FLAG\s*(\(\d+\))?\s*(.*)", comment)
            if search_res:
                comment = search_res.group(2)
            comment = " " + comment if comment else ""

            # Adjust space between code and comment
            if not code.endswith(" "):
                code += "  "
            elif not code.endswith("  "):
                code += " "

            # Then  create the new annotation and rewrite the line
            count_str = f"({count})" if count > 1 else ""
            source_lines[line - 1] = f"{code}--  FLAG{count_str}{comment}"

        # Finally write the modified lines in the source file
        with open(source, 'w', encoding=source_encoding) as f:
            for line in source_lines:
                print(line, file=f)


class BaseDriver(DiffTestDriver):
    """
    Common code for all test drivers.
    """

    flag_pattern = re.compile(r"--\s*FLAG\s*(\((\d+)\))?\s*(.*)")
    noflag_pattern = re.compile(r"--\s*NOFLAG")
    ada_file_pattern = r"[a-zA-Z][a-zA-Z0-9_\.\-]*\.(adb|ads|ada|ada_spec)"
    ada_source_encodings = ['utf-8', 'iso-8859-1']

    perf_supported = False
    flag_checking_supported = False

    @property
    def is_codepeer(self) -> bool:
        return self.testsuite_options.codepeer

    @property
    def perf_mode(self) -> bool:
        return hasattr(self.env, 'perf_mode') and self.env.perf_mode

    @property
    def flag_checking(self) -> bool:
        return ((not self.env.options.no_flag_checking) and
                self.test_env.get("check_flags", True))

    @property
    def lkql_jit_dir(self):
        return P.join(self.env.root_dir, "lkql_jit")

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
            'is_codepeer': self.is_codepeer,
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

    def check_run(
            self,
            args: list[str],
            check_flags: bool = True,
            lkql_path = "",
            **kwargs
        ) -> ProcessResult:
        """
        Run a process and check that its output is the expected one.

        :param check_flags: Whether to perform the flag checking process after
            the test run.
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

        # Add the provided LKQL path to environment variables
        env["LKQL_PATH"] = lkql_path

        if (
            self.flag_checking_supported and
            self.flag_checking and
            check_flags
        ):
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
                self.result.info["run_count"] = param
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

    def run_in_tty(self, args: list[str], **kwargs) -> tuple[str, int]:
        """
        Run a process in a pseudo-TTY using the ``pty`` module. Returns a
        tuple containing the process output and its status code.
        """
        # Ensure the current system is not windows, according to the
        # documentation, the ``pty`` module is not working fine on it (see
        # https://docs.python.org/fr/3/library/pty.html).
        if self.env.build.os.name == "windows":
            raise TestAbortWithError(
                "Cannot run a pseudo-TTY on Windows systems"
            )

        # Only import ``pty`` after checking that we are not on a Windows
        # system.
        import pty

        # Ensure the process is run in the testsuite working dir
        if not kwargs.get("cwd"):
            kwargs["cwd"] = self.working_dir()

        # Open a subprocess with using a pseudo TTY as output
        m, s = pty.openpty()
        p = subprocess.Popen(
            args=args,
            stdout=s,
            stderr=s,
            close_fds=True,
            **kwargs
        )
        os.close(s)

        # Read result of the process execution and get its return code
        fully_read = False
        output = b""
        status_code = 0
        try:
            while not fully_read:
                ready, _, _ = select.select([m], [], [], 0.05)
                for fd in ready:
                    try:
                        data = os.read(fd, 512)
                    except OSError as e:
                        if e.errno != errno.EIO:
                            raise e
                        fully_read = True
                    else:
                        if not data:
                            fully_read = True
                        output += data
        finally:
            os.close(m)
            if p.poll() is None:
                p.kill()
            status_code = p.wait()

        return (output.decode(), status_code)

    @property
    def output_refiners(self) -> list[OutputRefiner]:
        result = super().output_refiners
        result.append(Substitute(self.working_dir(), "<working-dir>"))
        if self.test_env.get("canonicalize_backslashes", True):
            result.append(Substitute("\\", "/"))
        return result

    def parse_flagged_lines(self, output: str) -> dict[str, TaggedLines]:
        """
        Parse the output of a test run and return a dictionary associating each
        Ada file to lines that have been flagged by the test.
        """
        raise TestAbortWithError("Unimplemented flagged lines parsing")

    def check_flags(self, execution_flags: dict[str, TaggedLines]) -> None:
        """
        Check that the source flagged lines are correctly flagged by the test
        driver, otherwise display the differences and add missing FLAG if
        asked for.

        :param execution_flags: The source lines flagged by the output parsing.
        """
        ada_source_names = set(
            [
                os.path.basename(f)
                for f in glob.glob(P.join(self.test_dir(), "**/*.ad*"), recursive=True)
            ]
        )
        all_flags: list[FileFlags] = []
        for source_name in sorted(ada_source_names):
            matching_sources = glob.glob(P.join(self.test_dir(), f"**/{source_name}"), recursive=True)
            flags, noflags = self.count_annotations_in_files(matching_sources)
            all_flags.append(
                FileFlags(
                    file_name=source_name,
                    matching_sources=matching_sources,
                    flag_annotations=flags,
                    noflag_annotations=noflags,
                    flagged_lines=execution_flags.get(source_name, TaggedLines())
                )
            )

        # Collect all error messages for all analyzed files
        errors_buffer: list[str] = []
        for flags in all_flags:
            errors = flags.errors
            if errors:
                errors_buffer.append(f"In file \"{flags.file_name}\":")
                for line, msg in errors:
                    errors_buffer.append(f"  - at line {line}: {msg}")
                errors_buffer.append("")

        # Raise an test failure if there are any errors
        if errors_buffer:
            self.output += "==== Errors in 'FLAG/NOFLAG' annotations ====\n\n"
            self.output += "\n".join(errors_buffer)

        # If required, add the missing 'FLAG' annotations
        if self.env.options.add_missing_flags:
            for flags in all_flags:
                flags.add_missing_flags()

    def count_annotations_in_files(self, files: list[str]) -> tuple[TaggedLines, TaggedLines]:
        """
        Count `FLAG/NOFLAG` annotations in the provided list of files and
        return the result in a tuple structured like
        `(flag_annotations, noflag_annotation)`.
        """
        flags = TaggedLines()
        noflags = TaggedLines()

        for source in files:
            lines, _ = self.read_ada_file(source)
            for line_num, line in enumerate(lines, 1):
                flag_search = self.flag_pattern.search(line)
                if flag_search:
                    count = flag_search.group(2) or 1
                    flags.tag_line(line_num, int(count))
                elif self.noflag_pattern.search(line):
                    noflags.tag_line(line_num)

        return (flags, noflags)

    @classmethod
    def read_ada_file(cls, file_name: str) -> tuple[list[str], str]:
        """
        Read the given Ada file with an automatic encoding detection strategy.
        Returns a tuple containing the lines of the file and its encoding.

        :param file_name: The Ada file to read.
        """
        with open(file_name, mode='rb') as ada_file:
            ada_bytes = ada_file.read()
            for encoding in cls.ada_source_encodings:
                try:
                    lines = ada_bytes.decode(encoding).split('\n')
                    lines = lines[:-1] if not lines[-1] else lines
                    return (lines, encoding)
                except ValueError as _:
                    pass

    def _define_lkql_executables(self) -> None:
        # If the mode is JIT
        if self.env.options.mode == "jit":
            python_wrapper = P.join(self.env.support_dir, "lkql_jit.py")
            self.command_base = [sys.executable, python_wrapper]

        # If the mode is native JIT
        elif self.env.options.mode == "native_jit":
            self.command_base = ["lkql"]

        self.lkql_exe = [*self.command_base, "run"]
        self.lkql_checker_exe = [*self.command_base, "check"]
        self.lkql_fix_exe = [*self.command_base, "fix"]
        self.gnatcheck_worker_exe = [*self.command_base, "gnatcheck_worker"]

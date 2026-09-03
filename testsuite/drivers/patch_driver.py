import glob
import os
import os.path as P
from pathlib import Path

from drivers.base_driver import BaseDriver

from e3.testsuite.driver.classic import TestAbortWithFailure
from e3.testsuite.driver.diff import OutputRefiner, ReplacePath


class PatchDriver(BaseDriver):
    """
    This driver runs 'lkql patch' on a SARIF report and compares the run
    transcript, followed by the content of the patched sources, to the
    provided output file.

    Test arguments:
        - ``sarif`` (str): The SARIF report template to apply, "report.sarif"
          by default. The driver looks for the "<sarif>.in" file in the test
          directory, and instantiates it by replacing its "@TEST_DIR@" markers
          with the working directory, as a report holds absolute paths.
        - ``auto_mode`` (bool): Whether to pass "--auto", so that the fixes
          are applied without prompting. True by default, as most tests do
          not exercise the interactive mode. It can also be set per run, for
          the tests mixing an interactive run with an automatic one.
        - ``args`` (list[str]): Extra arguments to pass to 'lkql patch'
          (e.g. "--dry-run", "--exclude-rule").
        - ``input`` (str): Answers to feed to the interactive prompt.
        - ``runs`` (list[dict]): For tests running the command several times
          (e.g. to test resuming), a list of {"args", "input", "auto_mode",
          "writable"} dicts, one per run. When provided, the top-level
          ``args`` and ``input`` arguments are ignored. The optional
          "writable" entry gives files to make writable again before that
          run.
        - ``source_filters`` (list[str]): Files and directories to restrict
          the fixes to. They are passed after the report, as the command line
          expects.
        - ``read_only`` (list[str]): Files to make read-only before running,
          to exercise write failures. Such a test must be restricted to
          POSIX systems with a "control" entry, and fails when the testsuite
          runs with enough privileges to write them anyway.
        - ``show`` (list[str]): Files to display after the run. Default is
          every Ada source of the test, looked up recursively and named
          relatively to the working directory.
    """

    flag_checking_supported = False

    def run(self) -> None:
        sarif = self.test_env.get("sarif", "report.sarif")

        # Instantiate the SARIF template. The markers stand inside "file:"
        # URIs, so the working directory is substituted in URI form, which a
        # filesystem path is not: its separators, and the characters an URI
        # gives a meaning to, have to be converted and encoded first.
        test_dir = Path(self.working_dir()).as_uri().removeprefix("file://")
        with open(self.working_dir(f"{sarif}.in"), encoding="utf-8") as f:
            content = f.read()
        with open(self.working_dir(sarif), "w", encoding="utf-8", newline="") as f:
            f.write(content.replace("@TEST_DIR@", test_dir))

        # Make the requested files read-only, to exercise write failures. The
        # testsuite is expected to have the rights to do so, so failing to
        # take them away is a failure: the run below would exercise nothing.
        read_only = self.test_env.get("read_only", [])
        for file_name in read_only:
            path = self.working_dir(file_name)
            os.chmod(path, 0o555 if P.isdir(path) else 0o444)
            if os.access(path, os.W_OK):
                raise TestAbortWithFailure(
                    f"cannot make {file_name} read-only for the current user"
                )

        # Run 'lkql patch' once for each run specification, feeding it the
        # scripted prompt answers if any. Use `catch_error=False` to avoid
        # failing on non-zero status code, as some tests actually exert
        # erroneous behaviors.
        runs = self.test_env.get(
            "runs",
            [
                {
                    "args": self.test_env.get("args", []),
                    "input": self.test_env.get("input"),
                }
            ],
        )
        auto_mode = self.test_env.get("auto_mode", True)
        for index, run in enumerate(runs):
            # Separate the transcripts of successive runs with a blank line,
            # as the fix counter of a run restarts from one and is thus no
            # help in telling where the previous run ended
            if index > 0:
                self.output += "\n"
            for file_name in run.get("writable", []):
                os.chmod(self.working_dir(file_name), 0o644)
            kwargs = {}
            if run.get("input") is not None:
                kwargs["stdin"] = "|" + run["input"]
            args = run.get("args", [])
            if run.get("auto_mode", auto_mode):
                args = ["--auto", *args]
            self.check_run(
                [
                    *self.command_base,
                    "patch",
                    *args,
                    sarif,
                    *self.test_env.get("source_filters", []),
                ],
                catch_error=False,
                **kwargs,
            )

        # Restore the permissions, so that the working directory stays
        # removable whatever the outcome of the runs
        for file_name in read_only:
            path = self.working_dir(file_name)
            os.chmod(path, 0o755 if P.isdir(path) else 0o644)

        # Then display the resulting sources
        default_show = sorted(
            P.relpath(f, self.working_dir())
            for f in glob.glob(self.working_dir("**/*.ad[sb]"), recursive=True)
        )
        show = self.test_env.get("show", default_show)
        if show:
            self.output += "\nResulting files\n===============\n"
        for file_name in show:
            # Separate the sources from each other, and from the header, with
            # a blank line. The content itself is displayed as it is, so that
            # a source with no final line separator shows as such.
            self.output += f"\n== {file_name} ==\n"
            with open(self.working_dir(file_name), encoding="utf-8", newline="") as f:
                self.output += f.read()

    @property
    def output_refiners(self) -> list[OutputRefiner]:
        # 'lkql patch' displays canonical paths, so the working directory must
        # also be canonicalized to be substituted. A report may moreover name
        # a file above the working directory, which is then displayed as such.
        # Order matters: the longest paths must be substituted first.
        dirs = list(dict.fromkeys([self.working_dir(), P.realpath(self.working_dir())]))
        return [
            *super().output_refiners,
            *[ReplacePath(d, "<working-dir>") for d in dirs],
            *[ReplacePath(P.dirname(d), "<tmp-dir>") for d in dirs],
        ]

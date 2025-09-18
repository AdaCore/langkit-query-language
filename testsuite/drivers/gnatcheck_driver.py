import os
import os.path as P
import re
import shutil
import xml.etree.ElementTree as ET

from drivers.base_driver import BaseDriver, TaggedLines

from e3.testsuite.driver.diff import (
    PatternSubstitute,
    OutputRefiner,
)


class GnatcheckDriver(BaseDriver):
    """
    This driver runs gnatcheck with the given arguments and compares the output
    of the run to the expected output.  The expected output must be written in
    a file called ``test.out``.


    It has several ways of processing the output depending on the output format
    passed. The ``format`` key can be either 'brief', 'full', 'short' or 'xml',
    and allows to switch the way the output of GNATcheck will be triggered and
    processed:

    * In ``brief`` mode, the ``--brief`` option will be passed to GNATcheck, and
      only stdout/err will be captured
    * In ``short`` mode, the ``-s`` option will be passed to GNATcheck, and the
      content of ``gnatcheck.out`` will be the output of the test
    * ``full`` mode, is the same except the ``-s`` option is not passed. The first
      lines of the ``gnatcheck.out`` report are stripped because they contain run
      specific information
    * In ``xml`` mode, the output of the test is the content of the
      ``gnatcheck.xml`` file.

    If the ``test.yaml`` contains a ``tests`` key, then its contents need to be
    a list, and each entry of the list is a separate test which can contain a
    specific value for any of the test keys specified here.

    In this case, the test keys specified at the top level are taken as
    default, and can be overriden in specific subtests.

    Test env keys are provided for the user to execute arbitrary Python code as
    part of the test/subtests. The ``global_python`` key can be used to alter
    the python execution environment to store data/functions that need to be
    called by test-specific python hooks described below.

    This driver also supports performance testing.

    Test arguments:
        - ``mode`` (str): The mode to run gnatcheck in; could be `gnatcheck` or
          `gnatkp`. Default is `gnatcheck`.
        - ``label`` (str): An arbitrary label to add at the top of the gnatcheck
          output.
        - ``worker``: Provide a custom worker for the GNATcheck run.
        - ``gnatkp_autoconfig`` (bool): Whether to automatically configure the
          target and runtime when running in "gnatkp" mode. Default is True.
        - ``auto_codepeer_target`` (bool): Whether to automatically add the
          codepeer target when the test is run in CodePeer mode. Default is
          True.
        - ``in_tty`` (bool): Whether to run GNATcheck in a pseudo TTY using the
          ``pty`` Python module.
        - ``lkql_path`` (list[str]): A list of directories forwarded to the
            `LKQL_PATH` environment variable when the test is run.

        - ``jobs`` (int): The number of jobs to forward to the GNATcheck command.
        - ``project`` (str): GPR build file to use (if any).
        - ``target`` (str): The target to forward to LibGPR for project
          resolution.
        - ``subdirs`` (str): The directory to forward to GNATcheck `--subdirs`
          option.
        - ``input_sources`` (list[str]): Ada files to analyze (if explicit,
          optional if `project` is passed).
        - ``include_file`` (str): If passed, include all sources listed in the
          provided file.
        - ``ignore_file`` (str): If passed, ignore all sources listed in the
          provided file.
        - ``output_file`` (str): The file to pass to GNATcheck with the `-o` or
          `-ox` option then to read the output in.
        - ``gpr_config_file`` (str): A GPR configuration file to pass through
          the '--config' option.
        - ``scenario_variables`` (dict[str, str]): Dict containing key to value
          associations to pass as scenario variables to GNATcheck.
        - ``show_rule`` (bool): Whether to add the rule at the end of
          diagnostics.
        - ``show_instantiation_chain`` (bool): whether to add instantiation
          chain in diagnostics concerning generic constructs.
        - ``extra_args`` (list[str]): Extra arguments to pass to GNATcheck.

        - ``check_semantic`` (bool): Whether to add the ``--check-semantic``
          flag when running GNATcheck.
        - ``rules`` (list[str]): A list of rules with their arguments, in the
          gnatcheck format.  Note that the list can be empty, and people can
          instead decide to pass rules via the project file.
        - ``rule_file`` (str): If passed, will be forwarded as `-from` to
          gnatcheck.
        - ``lkql_rule_file`` (str): If passed, will be forwarded as `--rule-file`
          to gnatcheck.
        - ``rules_dirs`` (list[str]): A list of directories to pass to gnatcheck
          as rule containing directories.
        - ``extra_rule_options`` (list[str]): Extra arguments for the rules
          section.

        - ``canonicalize_worker`` (bool): Whether to replace the GNATcheck worker
          name by a constant string in then test output. Default is True.
        - ``pre_python``/``post_python`` (str): Python code to be executed
          before/after the test.
        - ``perf``: Enable and configure the performance testing. Perf arguments:
            - ``default``: Time measuring repetition number as an integer.
            - ``profile-time``: Enable the time profiling or not as a boolean.
        - ``timeout`` (int): Set the test timeout in seconds.

    .. NOTE:: In practice, the above allows several different ways to express
        the same test, which is not ideal. It was necessary to transition
        painlessly existing bash tests.
    """

    perf_supported = True
    flag_checking_supported = True

    modes = {"gnatcheck": "gnatcheck", "gnatkp": "gnatkp"}
    output_formats = set(["brief", "full", "short", "xml"])

    flag_line_pattern = re.compile(
        rf"^({BaseDriver.ada_file_pattern}):(\d+):\d+: (rule violation|warning|error): .*$"
    )

    @classmethod
    def _parse_full(cls, output: str) -> dict[str, TaggedLines]:
        """
        Parse the full formatted GNATcheck output.
        """
        # Remove useless parts of the full output
        try:
            output = output[output.index("2. Exempted Coding Standard Violations") :]
            output = output[: output.index("5. Language violations")]

            # Then use the "short" parser since we remove the useless part of the
            # "full" format.
            return cls._parse_short_and_brief(output)
        except ValueError:
            # When catching this error, it means that the `index` method failed
            # to find required textual bounds.
            return {}

    @classmethod
    def _parse_short_and_brief(cls, output: str) -> dict[str, TaggedLines]:
        """
        Parse the short formatted GNATcheck output.
        """
        res: dict[str, TaggedLines] = {}

        for line in output.splitlines():
            search_result = cls.flag_line_pattern.search(line)
            if search_result is not None:
                (file, _, line_num, _) = search_result.groups()
                if not res.get(file):
                    res[file] = TaggedLines()
                res[file].tag_line(int(line_num))

        return res

    @classmethod
    def _parse_xml(cls, output: str) -> dict[str, TaggedLines]:
        """
        Parse the XML formatted GNATcheck output.
        """
        res: dict[str, TaggedLines] = {}

        def tag_line(file: str, line: int):
            if not res.get(file):
                res[file] = TaggedLines()
            res[file].tag_line(line)

        # Parse the xml result
        xml_tree = ET.fromstring(output)
        violations = xml_tree.find("violations")

        # If the "violations" tag exists in the output, parse it as a full XML output
        if violations is not None:
            for violation in violations:
                file, line_num = violation.attrib["file"], int(violation.attrib["line"])
                tag_line(file, line_num)

        # Else the output is a brief XML one
        else:
            for elem in xml_tree.findall("*"):
                if elem.tag in ("violation", "exemption-problem", "exempted-violation"):
                    file, line_num = elem.attrib["file"], int(elem.attrib["line"])
                    tag_line(file, line_num)

        # Return the result
        return res

    @classmethod
    def parsers(cls):
        return {
            "full": cls._parse_full,
            "short": cls._parse_short_and_brief,
            "brief": cls._parse_short_and_brief,
            "xml": cls._parse_xml,
        }

    @classmethod
    def replace_xml_tag_content(
        cls,
        xml_buffer: str,
        tag_name: str,
        new_content: str
    ) -> str:
        """
        Match all tags with the provided name and replace their content by the
        provided new content, returning the result.
        """
        return re.sub(
            f"<{tag_name}>.*</{tag_name}>",
            f"<{tag_name}>{new_content}</{tag_name}>",
            xml_buffer
        )

    @property
    def default_process_timeout(self):
        return self.test_env.get("timeout", 300)

    def run(self) -> None:
        gnatcheck_env = dict(os.environ)

        # Here we don't want to pollute the LKQL_RULES_PATH and LKQL_PATH with
        # this repository's LKQL rules: GNATcheck will find those itself by
        # looking next to its executable. If we let this variable, we might end
        # up with duplicate definitions of rules, for example if this repository
        # is a copy of the original LKQL repository (which is actually what
        # happens in production: the checkout used for testing is separate
        # from that used for building).
        gnatcheck_env["LKQL_RULES_PATH"] = getattr(self.env, "gnatcheck_rules_path", "")
        gnatcheck_env["LKQL_PATH"] = ""

        # Get the test provided custom GNATcheck worker
        custom_worker = self.test_env.get("worker", None)

        gnatcheck_env["GNATCHECK_WORKER"] = custom_worker or " ".join(
            self.gnatcheck_worker_exe
        )

        # Get the provided project path and set from the testsuite root
        project_path = self.test_env.get("project_path", None)
        if project_path:
            gnatcheck_env["GPR_PROJECT_PATH"] = os.pathsep.join(
                [
                    P.join(self.env.testsuite_root_dir, project_dir)
                    for project_dir in project_path
                ]
            )

        # Create the environment for post and pre Python snippets
        def ls(dirname: str):
            """
            Add the sorted content of the provided directory to the test output
            if the directory exists and is readable, otherwise add an error
            message.

            :param dirname: Name of the directory to display the content of
                relatively to the test working directory.
            """
            dirpath = self.working_dir(dirname)
            self.output += (
                "\n".join(sorted(os.listdir(dirpath)))
                if os.path.isdir(dirpath) else
                f"Cannot find the directory {dirname}"
            ) + "\n"

        def tree(root_dirname: str):
            """
            Add the directory tree of the provided root directory to the test
            output.

            :param root_dirname: Name of the directory to start the exploration
                from relatively to the test working directory.
            """
            root_dirpath = self.working_dir(root_dirname)
            if os.path.isdir(root_dirpath):
                for path, _, files in os.walk(root_dirpath):
                    self.output += f"{path}:\n"
                    for f in sorted(files):
                        self.output += f"    {f}\n"
            else:
                self.output += f"Cannot find the directory {root_dirname}\n"

        def cat(
            filename: str, sort: bool = False, trim_start: int = 0, trim_end: int = 0
        ):
            """
            Add the content of ``filename`` to the test output if it is readable.
            This filename must be relative to the test working dir.

            :param sort: Whether to sort the content of the file.
            :param trim_start: Count of lines to trim from the file start.
            :param trim_end: Count of lines to trim from the file end.
            """
            try:
                with open(self.working_dir(filename), "r") as f:
                    lines = f.readlines()
                    lines = lines[trim_start : len(lines) - trim_end]
                    if sort:
                        lines.sort()
                    self.output += ''.join(lines)
            except FileNotFoundError:
                self.output += f"Cannot find the file {filename}\n"

        def mkdir(dirname: str):
            """
            Create a new empty directory name as specified in the test working
            directory.
            """
            dirpath = self.working_dir(dirname)
            if os.path.isdir(dirpath) or os.path.isfile(dirpath):
                self.output += f"Cannot create {dirname}, already exists\n"
            else:
                os.makedirs(dirpath)

        def ln(source: str, dest: str):
            """
            Create a symbolic link from the source file to the specified
            destination. File names must be relative to the test working
            directory.
            """
            source_path = self.working_dir(source)
            dest_path = self.working_dir(dest)
            try:
                os.symlink(source_path, dest_path)
            except Exception as e:
                self.output += str(e) + "\n"

        def rm(relative_path: str):
            """
            Remove the element designated by the provided path relatively to
            the test working directory.
            If the path points to a file, just remove this file.
            If the path points to a directory, the directory and all its
            content are removed.
            """
            to_remove = self.working_dir(relative_path)
            if os.path.isfile(to_remove):
                os.remove(to_remove)
            elif os.path.isdir(to_remove):
                shutil.rmtree(to_remove)

        def cmd(f):
            """
            Wrap a Python function inside a logging function to add the name
            of the underlying function and its unnamed arguments to the test
            output before running it.
            """
            def res(*args, **kwargs):
                args_image = f" {' '.join(args)}" if args else ""
                self.output += f"Running \"{f.__name__}{args_image}\"...\n"
                f(*args, **kwargs)
            return res

        globs, locs = {
            "ls": cmd(ls),
            "tree": cmd(tree),
            "cat": cmd(cat),
            "mkdir": cmd(mkdir),
            "ln": cmd(ln),
            "rm": cmd(rm),
        }, {}
        global_python = self.test_env.get("global_python", None)
        if global_python:
            exec(global_python, globs, locs)

        # Prepare the execution flagged lines as an empty object
        files_flagged_lines: dict[str, TaggedLines] = {}

        def add_to_files_flagged_lines(to_add: dict[str, TaggedLines]):
            for file, tagged_lines in to_add.items():
                files_flagged_lines[file] = files_flagged_lines.get(
                    file, TaggedLines()
                ).combine(tagged_lines)

        def capture_exec_python(code: str) -> None:
            """
            Execute the python code, and capture it's output. Add it to the
            test's output.
            """
            from io import StringIO
            from contextlib import redirect_stdout

            f = StringIO()
            cwd = os.getcwd()
            os.chdir(self.test_env["working_dir"])
            with redirect_stdout(f):
                exec(code, globs, locs)
            self.output += f.getvalue()
            os.chdir(cwd)

        def run_one_test(test_data: dict[str, any]) -> None:
            output_format = test_data.get("format", "full")
            assert output_format in GnatcheckDriver.output_formats
            brief = output_format == "brief"
            exe = GnatcheckDriver.modes[test_data.get("mode", "gnatcheck")]
            args = [exe, "-q"]
            output_file_name = self.working_dir(
                test_data.get(
                    "output_file", f"{exe}.{'xml' if output_format == 'xml' else 'out'}"
                )
            )
            test_env = dict(gnatcheck_env)

            pre_python = test_data.get("pre_python", None)
            post_python = test_data.get("post_python", None)

            # If python code to be executed pre running gnatcheck was passed
            # for this test, run it and capture its output
            if pre_python:
                capture_exec_python(pre_python)

            # If required, add provided directories to the LKQL_PATH variable
            for d in test_data.get("lkql_path", []):
                test_env["LKQL_PATH"] = os.pathsep.join(
                    [
                        self.working_dir(d),
                        test_env.get("LKQL_PATH", ""),
                    ]
                )

            # Set the target if one has been provided
            if test_data.get("target"):
                args.append(f"--target={test_data['target']}")

            # If the executable is gnatkp, we must provide an explicit runtime
            # and target
            if exe == "gnatkp" and test_data.get("gnatkp_autoconfig", True):
                if not self.is_codepeer and not test_data.get("target"):
                    args.append(f"--target={self.env.host.triplet}")
                args.append("--RTS=default")

            # Set the codepeer target if needed and no other one has been
            # provided.
            if (
                self.is_codepeer
                and test_data.get("auto_codepeer_target", True)
                and not test_data.get("target")
            ):
                args.append("--target=codepeer")

            # Set the "--show-rule" flag according to the test
            if test_data.get("show_rule", False):
                args.append("--show-rule")

            # Set the "--show-instantiation-chain" flag according to the data
            if test_data.get("show_instantiation_chain", False):
                args.append("--show-instantiation-chain")

            # Set the number of wanted jobs
            if test_data.get("jobs", None):
                args.append(f'-j{test_data["jobs"]}')

            # Use the test's project, if any
            if test_data.get("project", None):
                args += ["-P", test_data["project"]]

            if test_data.get("gpr_config_file", None):
                args += [f'--config={test_data["gpr_config_file"]}']

            # Forward the subdirs option
            if test_data.get("subdirs", None):
                args += ["--subdirs", test_data["subdirs"]]

            # Set the output file path according to the requested format
            if output_format in ["short", "full"]:
                args.append(f"-o={output_file_name}")
            elif output_format == "xml":
                args.append(f"-ox={output_file_name}")

            # Add the include file if any
            include_file = test_data.get("include_file", None)
            if include_file:
                abs_include_file = self.working_dir(include_file)
                include_file = (
                    abs_include_file if P.isfile(abs_include_file) else include_file
                )
                args += [f"-files={include_file}"]

            # Add the ignore file if any
            ignore_file = test_data.get("ignore_file", None)
            if ignore_file:
                abs_ignore_file = self.working_dir(ignore_file)
                ignore_file = (
                    abs_ignore_file if P.isfile(abs_ignore_file) else ignore_file
                )
                args += [f"--ignore={ignore_file}"]

            # Add the specified sources to GNATcheck arguments
            if test_data.get("input_sources", None):
                args += test_data["input_sources"]

            # Precise the wanted format to the GNATcheck command line
            if output_format == "brief":
                args.append("--brief")
            elif output_format == "xml":
                args.append("-xml")
            elif output_format == "short":
                args.append("-s")

            # Add the scenario variables
            for k, v in test_data.get("scenario_variables", {}).items():
                args.append(f"-X{k}={v}")

            # If required, add the "--check-semantic" flag
            if test_data.get("check_semantic"):
                args.append("--check-semantic")

            # Add the rule directories
            for rule_dir in test_data.get("rules_dirs", []):
                args.append(f"--rules-dir={rule_dir}")

            # Add the LKQL rule file
            if test_data.get("lkql_rule_file", None):
                args.append(f"--rule-file={test_data['lkql_rule_file']}")

            # Finally add all extra arguments given in the test
            for extra_arg in test_data.get("extra_args", []):
                args.append(extra_arg)

            # Start the GNATcheck rule section
            args.append("-rules")

            # Add the rule configuration arguments
            rule_file = test_data.get("rule_file", None)
            if rule_file:
                abs_rule_file = self.working_dir(rule_file)
                rule_file = abs_rule_file if P.isfile(abs_rule_file) else rule_file
                args += ["-from", rule_file]

            # Add all rules options, add it to the command-line
            for r in test_data.get("rules", []):
                args.append(r)

            # Finally, add the extra rule options
            for arg in test_data.get("extra_rule_options", []):
                args.append(arg)

            # Run the interpreter
            # TODO: For the moment, not trying to do anything with the error code,
            # and instead relying solely on the diff. We might want to check that
            # the return code is consistent at some later stage.

            if self.perf_mode:
                self.perf_run(args)
            else:
                label = test_data.get("label", None)
                if label:
                    self.output += label + "\n" + ("=" * len(label)) + "\n\n"

                # Execute GNATcheck and get its output
                exec_output = ""
                status_code = 0
                if test_data.get("in_tty"):
                    exec_output, status_code = self.run_in_tty(args, env=test_env)
                else:
                    p = self.shell(
                        args, env=test_env, catch_error=False, analyze_output=False
                    )
                    exec_output = p.out
                    status_code = p.status

                # If required, canonicalize gnatcheck worker name in the output
                if test_data.get("canonicalize_worker", True):
                    worker = " ".join(
                        [
                            P.basename(self.gnatcheck_worker_exe[0]),
                            *self.gnatcheck_worker_exe[1:],
                        ]
                    )
                    exec_output = exec_output.replace(worker, "<gnatcheck_worker_exe>")

                # Then read GNATcheck report file if there is one
                report_file_content = ""
                parse_output_for_flags = True
                if output_format in ["full", "short", "xml"]:
                    try:
                        with open(output_file_name) as f:
                            if output_format in ["short", "xml"]:
                                report_file_content = f.read()
                                if output_format == "xml":
                                    # Remove changing parts of the GNATcheck XML report
                                    for (tag_name, new_content) in [
                                        ("date", "DATE"),
                                        ("version", "VERSION"),
                                        ("command-line", "COMMAND_LINE"),
                                        ("runtime", "RUNTIME"),
                                    ]:
                                        report_file_content = self.replace_xml_tag_content(report_file_content, tag_name, new_content)
                            else:
                                # Strip the 10 first lines of the report, which contain
                                # run-specific information that we don't want to
                                # include in the test baseline.
                                report_file_content = "".join(f.readlines()[9:])
                    except FileNotFoundError as _:
                        self.output += (
                            "testsuite_driver: No output file generated by gnatcheck\n"
                        )
                        parse_output_for_flags = False

                # Get the lines flagged by the test running and add it to all flagged lines
                if self.flag_checking and parse_output_for_flags:
                    add_to_files_flagged_lines(
                        self.parse_flagged_lines(
                            (
                                exec_output + report_file_content
                                if output_format != "xml"
                                else report_file_content
                            ),
                            output_format,
                        )
                    )

                # Add the execution output to the test output
                self.output += exec_output + report_file_content

                if (not brief and status_code not in [0, 1]) or (
                    brief and status_code != 0
                ):
                    self.output += ">>>program returned status code {}\n".format(
                        status_code
                    )

            # If python code to be executed post running gnatcheck was passed
            # for this test, run it and capture its output
            if post_python:
                capture_exec_python(post_python)

        tests = self.test_env.get("tests")
        if tests:
            for i, test in enumerate(tests):
                run_one_test(self.test_env | test)
                if i < len(tests) - 1:
                    self.output += "\n"
        else:
            run_one_test(self.test_env)

        # Check the execution flagged lines
        if self.flag_checking:
            self.check_flags(files_flagged_lines)

    def parse_flagged_lines(self, output: str, format: str) -> dict[str, TaggedLines]:
        assert format in self.output_formats
        return self.parsers()[format](output)

    @property
    def output_refiners(self) -> list[OutputRefiner]:
        result = super().output_refiners
        # Remove version information printed by gnatcheck in verbose mode
        result.append(
            PatternSubstitute(
                "gnatcheck [0-9]+.[0-9]w \\([a-z0-9 ]+\\)",
                "gnatcheck <version> (<date>)",
            )
        )
        result.append(PatternSubstitute("Copyright [0-9-]+", "Copyright <date>"))

        # Remove project search path information
        result.append(PatternSubstitute(
            "project search path: .*",
            "project search path: <gpr_path>",
        ))
        return result

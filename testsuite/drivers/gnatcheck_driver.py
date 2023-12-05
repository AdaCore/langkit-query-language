import os
import os.path as P
import re
import sys
import xml.etree.ElementTree as ET

from e3.testsuite.driver.diff import (
    ReplacePath,
    Substitute,
    OutputRefiner,
)

from drivers.base_driver import BaseDriver, Flags


# --- GNATcheck output parsing functions

_flag_line_pattern = re.compile(
    r"^([a-zA-Z][a-zA-Z0-9_\.\-]*\.(adb|ads|ada|ada_spec)):(\d+):\d+: .*$"
)

def _parse_full(output: str) -> Flags:
    """
    Parse the full formatted gnatcheck output.
    """
    # Prepare the result
    res = Flags()

    # Parse the gnatcheck full output
    is_parsing = False
    for line in output.splitlines():
        if not is_parsing:
            is_parsing = "2. Exempted Coding Standard Violations" in line
        else:
            search_result = _flag_line_pattern.search(line)
            if search_result is not None:
                (file, _, line_num) = search_result.groups()
                res.add_flag(file, int(line_num))
            is_parsing = "5. Language violations" not in line

    # Return the result
    return res

def _parse_short_and_brief(output: str) -> Flags:
    """
    Parse the short formatted gnatcheck output.
    """
    # Prepare the result
    res = Flags()

    # Parse the output
    for line in output.splitlines():
        search_result = _flag_line_pattern.search(line)
        if search_result is not None:
            (file, _, line_num) = search_result.groups()
            res.add_flag(file, int(line_num))

    # Return the result
    return res

def _parse_xml(output: str) -> Flags:
    """
    Parse the xml formatted gnatcheck output.
    """
    # Prepare the result
    res = Flags()

    # Parse the xml result
    xml_tree = ET.fromstring(output)
    violations = xml_tree.find("violations")

    # If the "violations" tag exists in the output, parse it as a full XML output
    if violations is not None:
        for violation in violations:
            file, line_num = violation.attrib["file"], int(violation.attrib["line"])
            res.add_flag(file, line_num)

    # Else the ouput is a brief XML one
    else:
        for elem in xml_tree.findall("*"):
            if elem.tag in ("violation", "exemption-problem", "exempted-violation"):
                file, line_num = elem.attrib["file"], int(elem.attrib["line"])
                res.add_flag(file, line_num)

    # Return the result
    return res


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
        - ``project``: GPR build file to use (if any)
        - ``input_sources``: Ada files to analyze (if explicit, optional if
          project is passed)
        - ``rule_file``: If passed, files to analyse will be fetched from this
          file
        - ``extra_args``: Extra arguments to pass to GNATcheck
        - ``rules``: A list of rules with their arguments, in the gnatcheck
          format.  Note that the list can be empty, and people can instead
          decide to pass rules via the project file.
        - ``scenario_variables``: Dict containing key to value associations to
          pass as scenario variables to GNATcheck
        - ``perf``: Enable and configure the performance testing. Perf arguments:
            - ``default``: Time measuring repetition number as an integer
            - ``profile-time``: Enable the time profiling or not as a boolean
        - ``pre_python``/``post_python``: Python code to be executed
          before/after the test
        - ``worker``: Provide a custom worker for the GNATcheck run

    .. NOTE:: In practice, the above allows several different ways to express
        the same test, which dis not ideal. It was necessary to transition
        painlessly existing bash tests.
    """

    perf_supported = True
    flag_checking_supported = True

    modes = {
        "gnatcheck": "gnatcheck",
        "gnatkp": "gnatkp"
    }
    output_formats = set(['brief', 'full', 'short', 'xml'])
    parsers = {
        'full': _parse_full,
        'short': _parse_short_and_brief,
        'brief': _parse_short_and_brief,
        'xml': _parse_xml
    }

    def run(self) -> None:
        gnatcheck_env = dict(os.environ)

        # Here we don't want to pollute the LKQL_RULES_PATH with this
        # repository's LKQL rules: GNATcheck will find those itself by looking
        # next to its executable. If we let this variable, we might end up
        # with duplicate definitions of rules, for example if this repository
        # is a copy of the original LKQL repository (which is actually what
        # happens in production: the checkout used for testing is separate
        # from that used for building).
        gnatcheck_env["LKQL_RULES_PATH"] = getattr(
            self.env, "gnatcheck_rules_path", ""
        )

        # Get the test provided custom GNATcheck worker
        custom_worker = self.test_env.get('worker', None)

        gnatcheck_env["GNATCHECK_WORKER"] = custom_worker or " ".join(
            self.gnatcheck_worker_exe
        )

        globs, locs = {}, {}
        global_python = self.test_env.get("global_python", None)
        if global_python:
            exec(global_python, globs, locs)

        # Prepare the execution flagged lines as an empty object
        flagged_lines = Flags()

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
            output_format = test_data.get('format', 'full')
            assert output_format in GnatcheckDriver.output_formats
            brief = output_format == 'brief'
            exe = GnatcheckDriver.modes[test_data.get('mode', 'gnatcheck')]
            args = [exe, '-q']

            pre_python = test_data.get('pre_python', None)
            post_python = test_data.get('post_python', None)

            # If python code to be executed pre running gnatcheck was passed
            # for this test, run it and capture its output
            if pre_python:
                capture_exec_python(pre_python)

            # Set the "--show-rule" flag according to the test
            if test_data.get('show_rule', False):
                args.append('--show-rule')

            # Use the test's project, if any
            if test_data.get('project', None):
                args += ['-P', test_data['project']]

            if test_data.get('input_sources', None):
                args += test_data['input_sources']

            if output_format == 'brief':
                args.append('--brief')
            elif output_format == 'xml':
                args.append('-xml')
            elif output_format == 'short':
                args.append('-s')

            for k, v in test_data.get('scenario_variables', {}).items():
                args.append(f'-X{k}={v}')

            for rule_dir in test_data.get('rules_dirs', []):
                args.append(f'--rules-dir={rule_dir}')

            for extra_arg in test_data.get('extra_args', []):
                args.append(extra_arg)

            args.append("-rules")
            rule_file = test_data.get('rule_file', None)
            lkql_rule_file = test_data.get('lkql_rule_file', None)
            if rule_file:
                args += ['-from', rule_file]
            if lkql_rule_file:
                args += ['-from-lkql', lkql_rule_file]
            elif test_data.get('rules', None):
                for r in test_data.get('rules', []):
                    args.append(r)

            # Run the interpreter
            # TODO: For the moment, not trying to do anything with the error code,
            # and instead relying solely on the diff. We might want to check that
            # the return code is consistent at some later stage.

            if self.perf_mode:
                self.perf_run(args)
            else:
                label = test_data.get('label', None)
                if label:
                    self.output += label + "\n" + ("=" * len(label)) + "\n\n"

                p = self.shell(args, env=gnatcheck_env, catch_error=False, analyze_output=False)

                # Get the GNATcheck execution output
                exec_output = p.out
                if output_format in ['full', 'short']:
                    with open(
                        P.join(self.test_env["working_dir"], "gnatcheck.out")
                    ) as f:
                        if output_format == 'short':
                            exec_output += f.read()
                        else:
                            # Strip the 10 first lines of the report, which contain
                            # run-specific information that we don't want to
                            # include in the test baseline.
                            exec_output += "".join(f.readlines()[9:])
                elif output_format == 'xml':
                    with open(
                        P.join(self.test_env["working_dir"], "gnatcheck.xml")
                    ) as f:
                        exec_output += f.read()

                # Get the execution flagged lines and add it to the global flags
                if self.flag_checking:
                    flagged_lines.add_other_flags(
                        self.parse_flagged_lines(exec_output, output_format)
                    )

                # Add the execution output to the test output
                self.output += exec_output

                if (not brief and p.status not in [0, 1]) or (brief and p.status != 0):
                    self.output += ">>>program returned status code {}\n".format(p.status)

            # If python code to be executed post running gnatcheck was passed
            # for this test, run it and capture its output
            if post_python:
                capture_exec_python(post_python)

        tests = self.test_env.get("tests")
        if tests:
            for i, test in enumerate(tests):
                env = self.test_env.copy()
                for k, v in test.items():
                    env[k] = v
                run_one_test(env)
                if i < len(tests) - 1:
                    self.output += "\n"
        else:
            run_one_test(self.test_env)

        # Check the execution flagged lines
        if self.flag_checking:
            self.check_flags(flagged_lines)

    @property
    def output_refiners(self) -> list[OutputRefiner]:
        result = super().output_refiners + [ReplacePath(self.working_dir())]
        if self.test_env.get("canonicalize_backslashes", False):
            result.append(Substitute("\\", "/"))
        return result

    def parse_flagged_lines(self, output: str, format: str) -> Flags:
        assert format in self.output_formats
        return self.parsers[format](output)

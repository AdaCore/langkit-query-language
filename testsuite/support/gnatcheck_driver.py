import os
import os.path as P

from e3.testsuite.driver.diff import (
    ReplacePath,
    Substitute,
)

from support.base_driver import BaseDriver


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

    .. NOTE:: In practice, the above allows several different ways to express
        the same test, which is not ideal. It was necessary to transition
        painlessly existing bash tests.
    """

    perf_supported = True

    modes = {
        "gnatcheck": "gnatcheck",
        "gnatkp": "gnatkp"
    }
    output_formats = set(['brief', 'full', 'short', 'xml'])

    def run(self):
        gnatcheck_env = dict(os.environ)

        if self.env.gnatcheck_worker_exe:
            gnatcheck_env["GNATCHECK_WORKER"] = " ".join(
                self.gnatcheck_worker_exe
            )

        def run_one_test(test_data):
            output_format = test_data.get('format', 'full')
            assert output_format in GnatcheckDriver.output_formats

            brief = output_format == 'brief'

            exe = GnatcheckDriver.modes[test_data.get('mode', 'gnatcheck')]
            args = [exe, '-q']

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

            rule_file = test_data.get('rule_file', None)
            if rule_file:
                args += ['-rules', '-from', rule_file]
            elif test_data.get('rules', None):
                args.append("-rules")
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

                p = self.shell(args, catch_error=False, env=gnatcheck_env)

                if output_format in ['full', 'short']:
                    with open(
                        P.join(self.test_env["working_dir"], "gnatcheck.out")
                    ) as f:
                        if output_format == 'short':
                            self.output += f.read()
                        else:
                            # Strip the 10 first lines of the report, which contain
                            # run-specific information that we don't want to
                            # include in the test baseline.
                            self.output += "".join(f.readlines()[9:])
                elif output_format == 'xml':
                    with open(
                        P.join(self.test_env["working_dir"], "gnatcheck.xml")
                    ) as f:
                        self.output += f.read()

                if (not brief and p.status not in [0, 1]) or (brief and p.status != 0):
                    self.output += ">>>program returned status code {}\n".format(p.status)

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

    @property
    def output_refiners(self):
        result = super().output_refiners + [ReplacePath(self.working_dir())]
        if self.test_env.get("canonicalize_backslashes", False):
            result.append(Substitute("\\", "/"))
        return result

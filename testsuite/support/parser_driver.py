from support.base_driver import BaseDriver


class ParserDriver(BaseDriver):
    """
    Compare the output of LKQL's `parse` program on the script in the `input`
    file to the expected output in the `output` text file. Tests pass iff the
    output is the same.

    Note that this compares the output of `parse` passing to it the
    `--hide-slocs` option.

    Test arguments:
        - rule: name of the grammar rule to pass to `parse`
    """
    def run(self):
        rule = self.test_env['rule']

        # Run the `parse` program.
        # The `f` option is used to specify a file name, instead of passing
        # the text to parse as a command-line argument.
        # The `r` option is used to choose a starting rule for the parser.
        self.shell(['lkql_parse', '-f', 'input', '-r', rule, '--hide-slocs'])

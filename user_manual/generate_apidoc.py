"""
APIdoc generation script for LKQL. It can be used to generate a sphinx file
containing API documentation for given LKQL modules.

Limitations:

- For the moment it is only able to document functions and selectors.

- We don't have an LKQL sphinx domain, so this piggybacks on the python domain.
  This implies some limitations (like, for example, selectors are documented as
  functions for now).
"""

import argparse
import liblkqllang as L
from os import path as P, makedirs
import re

from contextlib import contextmanager

SCRIPT_PATH = P.dirname(P.realpath(__file__))

PROF_RE = re.compile(r"(?P<is_builtin>@builtin )?"
                     r"(?P<kind>fun|selector)\s*(?P<profile>.*)")


class App(object):

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.eval_counter = 0
        self.output = []
        self._indent = 0
        self.ctx = L.AnalysisContext()
        self.parser = argparse.ArgumentParser()
        self.parser.add_argument(
            'modules', nargs='*', help="LKQL module names to document"
        )
        self.parser.add_argument(
            '--std', action="store_true",
            help='Generate apidoc for the prelude & builtin functions'
        )
        self.parser.add_argument(
            '-O', '--output-dir', type=str,
            default=".",
            help='Output directory for the generated rst files'
        )

    def write(self, *chunks):
        """
        Write chunks to the current file, taking current indentation into
        account.
        """
        for chunk in chunks:
            if chunk == '':
                self.output.append('')
                continue

            for line in chunk.splitlines():
                self.output.append((' ' * self ._indent) + line)

    def get_output(self):
        """
        Return current output formatted properly.
        """
        return "\n".join(self.output)

    @contextmanager
    def output_file(self, file_name):
        """
        Context manager to set the output file. Writes the content of the
        current buffer to the file and flushes the buffer on exit.
        """
        makedirs(self.args.output_dir, exist_ok=True)
        try:
            self.output_file_name = file_name
            yield
        finally:
            with open(
                P.join(self.args.output_dir, self.output_file_name), "w"
            ) as f:
                f.write(self.get_output())
                self.output = []

    @contextmanager
    def indent(self):
        """
        Context manager to indent sphinx code emitted inside the with block.
        """
        try:
            self._indent += 4
            yield
        finally:
            self._indent -= 4

    def eval(self, cmd):
        """
        Eval given LKQL command and returns the result as a string.
        """
        cmd = self.ctx.get_from_buffer(f"<tmp_{self.eval_counter}", cmd)
        self.eval_counter += 1
        return cmd.root.p_interp_eval

    def process_profile(self, prof):
        """
        Given a string profile such as::

            fun foo(a, b)

        Returns a structured profile as a map::

            {"is_builtin": bool, "kind": "fun"|"selector", profile: str}
        """
        return PROF_RE.match(prof).groupdict()

    def generate_module_doc(self, object_list, module_name=""):
        """
        Generate documentation for the given object list and module name. If no
        module name is given, assume we're generating for the current
        namespace. This mode is used for the --std flag.
        """
        if module_name:
            module_doc = eval(self.eval(f'doc({module_name})'))
            self.write(module_doc)
            self.write('')

        for sym in object_list:
            name = sym if not module_name else f"{module_name}.{sym}"
            prof = eval(self.eval(f"profile({name})"))
            if not prof:
                return
            pprof = self.process_profile(prof)
            self.write(f'.. function:: {pprof["profile"]}')
            with self.indent():
                self.write('')
                doc = eval(self.eval(f"doc({name})"))
                self.write(doc)
            self.write('')

    def main(self):
        self.args = self.parser.parse_args()
        dummy_unit = self.ctx.get_from_buffer('<dummy>', '12')
        dummy_unit.root.p_interp_init_from_project(
            P.join(SCRIPT_PATH, "apidoc_default.gpr")
        )

        if self.args.std:
            local_symbols = eval(self.eval("get_symbols()"))
            with self.output_file('std.rst'):
                self.write('Standard library')
                self.write('----------------')
                self.write('')
                self.generate_module_doc(local_symbols)

        for module_name in self.args.modules:
            self.eval(f"import {module_name}")
            module_symbols = eval(self.eval(
                f"get_symbols({module_name})")
            )
            with self.output_file(f'{module_name}.rst'):
                self.write(f'API doc for module {module_name}')
                self.write('--------------------------------')
                self.write('')
                self.generate_module_doc(module_symbols, module_name)
                print(self.get_output())


if __name__ == '__main__':
    App().main()

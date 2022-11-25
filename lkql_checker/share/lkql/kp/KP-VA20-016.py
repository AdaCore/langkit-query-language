#!/usr/bin/env python3
import sys
import re
import argparse

# This script aims to detect potential occurrences of wrong code generation
# associated with GCC PR 107248.
#   https://gcc.gnu.org/bugzilla/show_bug.cgi?id=107248
#
# Note that this script is free from false negatives but can produce false
# positives, i.e. situations where a pattern is found, but doesn't indicate a
# problem, so the output may require a further manual analysis.
#
# Usage:
#  1) Disassemble your file. It can be any compiled file (shared libraries,
#     executables, object files) or archives (static libraries), e.g:
#     $ leon3-objdump -dr leon-binary > leon-binary.dis
#  2) Launch this script with the generated assembly file:
#     $ <install dir>/share/lkql/kp/KP-VA20-016.py leon-binary.dis
#  3a) If the output is empty, your file is free from GCC PR 107248 wrong code.
#  3b) If the output isn't empty, you'll need to perform a manual analysis
#      on the reported symbol(s).
#
# The script parses the output of "objdump -dr", which needs to be prepared in
# advance, looking for any load/store after a stack increment. We aim to
# detect, in leaf functions, a pattern similar to:
#  (1) add %sp, 0x50, %g1
#  (2) add %sp, 0x50, %sp
#  (3) add %g1, %o0, %o0
#  (4) ld [ %o0 + -8 ], %o0
#
# In (4), the ld will be done in a stack memory "freed" by (2).
# As detected if the (4) is indeed a load or a store targeting a stack
# address is complicated. In the example above, analysing (1), (3) and (4)
# would be required.
# This script simplifies the pattern to detect any load/store after any (2).
# The loads or stores explicilty referencing %sp can be safely ignored as
# GCC ensures that the swap won't happen.

__version__ = "1.0"

# Parsed command line arguments:
args = None


class Symbols:
    """A single symbol object, against which we register lines of
    disassembled instructions found in the objdump output."""

    # A regular expression which, when it matches a objdump output line,
    # designates the start of a symbol disassembly, like:
    #
    # 0000000c <symbol_name>:

    RE = r"^(?P<addr>[0-9A-Fa-f]+) <(?P<symname>.+)>:"

    def __init__(self, remo):
        self.s_name = remo.group("symname")
        self.s_addr = remo.group("addr")

        # List of all the instruction objects registered for this symbol.
        self.s_insns = []
        self.s_relocs = []

    def __str__(self):
        return self.s_name

    def append_insn(self, insn):
        self.s_insns.append(insn)

    def append_reloc(self, reloc):
        self.s_relocs.append(reloc)

    def get_reloc_at_offset(self, off):
        for r in self.s_relocs:
            if r.r_offset == off:
                return r
        return None

    def check(self):
        """
        Check that this symbol doesn't have an occurence of GCC PR 107248
        bug. It looks for any load/store not explicitly using %sp after a
        stack increment in a leaf function.
        """
        # First pass: detect if it's a leaf function
        is_leaf = True
        for insn in self.s_insns:
            if insn.i_mnemonic == "save":
                is_leaf = False
                break

        if not is_leaf:
            return False

        if args.verbose > 2:
            print(f"{self}: Leaf function detected")

        # Second pass: look for load/store after a stack increment.
        stack_incr_detected = False
        last_insn = None
        has_errors = False
        for insn in self.s_insns:
            # Detect if a stack increment happens
            if insn.is_stack_incr():
                # If it's happened after any return instructions, this is a
                # delayed stack increment. Any load/store cannot happen
                # afterwards.
                if last_insn and not last_insn.is_ret():
                    if args.verbose:
                        print(
                            f"{self}(0x{insn.i_offset}):" +
                            " Stack increment detected")
                    stack_incr_detected = True
                    continue

            # A stack increment has been detected.
            if stack_incr_detected:
                if args.verbose > 1:
                    print(f"{self}(0x{insn.i_offset}): Checking {insn}")

                # Detect any load/store.
                if insn.is_load() or insn.is_store():
                    # GCC scheduler will never swap instructions if %sp
                    # is explicitly referenced. Thus, there are safe
                    # to be skipped.
                    if insn.is_sp_related():
                        if args.verbose:
                            print(
                                f"{self}(0x{insn.i_offset}):" +
                                " %sp related store/load detected:" +
                                " safe to ignore"
                            )
                        continue

                    # If there is a relocation on this instruction, this is a
                    # symbol address being targeted. No stack access can occur.
                    r = self.get_reloc_at_offset(insn.i_offset)
                    if r is not None or insn.has_reloc():
                        if args.verbose:
                            print(
                                f"{self}(0x{insn.i_offset}):" +
                                " Symbol access detected: safe to ignore"
                            )
                        continue

                    print(
                        f"{self}(0x{insn.i_offset}):" +
                        " Suspicious store/load detected")
                    has_errors = True

                # If a branch inside the same function happens, the current
                # algorithm cannot detect any wrong load/store.
                # It shouldn't happen but this is a safeguard.
                if insn.is_local_branch(self.s_name):
                    print(
                        f"{self}(0x{insn.i_offset}): Local branch detected." +
                        " Manual verification required."
                    )
                    has_errors = True

                # A ret has been detected and the instruction following it
                # (ie the current insn) have been checked. This means the
                # current stack increment has been handled.
                if last_insn and last_insn.is_ret():
                    stack_incr_detected = False

            # Save if this instruction is a ret to handle delayed instructions.
            last_insn = insn

        return has_errors


class Reloc:
    """Class for instruction objects, constructed from a regexp match object
    found within the disassembly output for a particular symbol."""

    # A regular expression which, when it matches a objdump output line,
    # designates an assembly instruction, like:
    #
    # offset  reloc kind       optional
    #   v     v                v
    #   10:   R_PPC_ADDR16_LO  ...

    RE = (
        # offset
        r"\s*(?P<offset>\w+?):"
        # reloc kind
        r"\s*(?P<kind>R_\w+)"
    )

    def __init__(self, remo):
        self.r_offset = remo.group("offset")
        self.r_kind = remo.group("kind")

    def __str__(self):
        return f"\t{self.r_kind}"


class Insn:
    """Class for instruction objects, constructed from a regexp match object
    found within the disassembly output for a particular symbol."""

    # A regular expression which, when it matches a objdump output line,
    # designates an assembly instruction, like:
    #
    # offset  hex contents    text representation   comments
    #   v     v               v                     v
    #   10:   7c 08 02 a6     mflr    r0            ! ...

    RE = (
        # offset
        r"\s*(?P<offset>\w+?):"
        # hex contents
        r"\s*(?P<hexcode>\w\w \w\w \w\w \w\w)"
        # text representation
        r"\s*(?P<mnemonic>[^ ]+)(?P<ops>[^!]*)"
        # comments
        r"\s*(!\s*(?P<comment>.*))?"
    )

    re_insn = re.compile(
        r"^ *([0-9A-Fa-f]+): *\t(.. .. .. ..) *\t([^ ]+)(.*)$")

    def __init__(self, remo):
        self.i_offset = remo.group("offset")
        self.i_hexcode = remo.group("hexcode")

        # Split mnemonic and ops
        self.i_mnemonic = remo.group("mnemonic")
        self.i_ops = [op.strip() for op in remo.group("ops").split(",")]
        self.i_comment = remo.group("comment")

    def __str__(self):
        res = f"\t{self.i_mnemonic} " + ", ".join(self.i_ops)
        if self.i_comment:
            res += f"\t! {self.i_comment}"

        return res

    def is_ret(self):
        return self.i_mnemonic.startswith("ret") or (
            self.i_mnemonic.startswith("jmp") and "%o7" in self.i_ops[0]
        )

    def is_load(self):
        return (
            self.i_mnemonic.startswith("ld")
            or self.i_mnemonic.startswith("cas")
            or self.i_mnemonic.startswith("swap")
        )

    def is_store(self):
        return (
            self.i_mnemonic.startswith("st")
            or self.i_mnemonic.startswith("cas")
            or self.i_mnemonic.startswith("swap")
            or self.i_mnemonic.startswith("ldst")
        )

    def is_branch(self):
        return self.i_mnemonic.startswith("b") or \
            self.i_mnemonic.startswith("fb")

    def is_local_branch(self, s_name):
        return self.is_branch() and s_name in self.i_ops[0]

    def is_sp_related(self):
        # Return True if any of the operands is targetting %sp.
        for op in self.i_ops:
            if "%sp" in op:
                return True
        return False

    def is_stack_incr(self):
        # A stack increment is following the pattern:
        #   add %sp, <positive number>, %sp
        if self.i_mnemonic != "add":
            return False
        if self.i_ops[0] != "%sp" or self.i_ops[2] != "%sp":
            return False

        # Objdump -d is usually printing positive numbers in hexadecimal and
        # negative numbers in decimal  starting with a "-".
        if self.i_ops[1].startswith("-"):
            return False

        return True

    def has_reloc(self):
        # Within an executable, the relocations are not available.
        # However, objdump can detect some instructions targeting
        # a symbol. It will then print its address and name in the
        # comment part.
        reloc_comment_re = r"^(?P<addr>[0-9A-Fa-f]+) <(?P<symname>.+)>"
        return self.i_comment is not None and \
            re.match(reloc_comment_re, self.i_comment)


def parse_dissas(fname):
    syms = []
    current_sym = None
    has_relocs = False
    if args.verbose:
        print(f"Parsing objdump -dr output in {fname}")

    with open(fname, "r") as f:
        for line in f:
            line = line.rstrip()

            # A new symbol is detected
            m = re.match(Symbols.RE, line)
            if m:
                current_sym = Symbols(m)
                syms.append(current_sym)
                if args.verbose > 2:
                    print(f"New symbol detected: {current_sym}")
                continue

            # A new instruction is detected
            m = re.match(Insn.RE, line)
            if m:
                current_sym.append_insn(Insn(m))
                continue

            m = re.match(Reloc.RE, line)
            if m:
                current_sym.append_reloc(Reloc(m))
                has_relocs = True
                continue

    if not has_relocs:
        print("""
Warning: no relocation have been found. This can lead to various false
positives. Unless you're scanning an executable, please ensure that the
objdump output was generated with -dr flags.
""")

    if args.verbose:
        print("Parsing done")

    return syms


argparser = argparse.ArgumentParser(
    description="Scanning utility for KP-VA20-016 / GCC PR 107248")
argparser.add_argument(
    "-v",
    "--verbose",
    action="count",
    default=0,
    help="dump analysis events of note",
)
argparser.add_argument(
    "--version", action="version", version=f"{__version__}")
argparser.add_argument(
    "fname", metavar="file", help="objdump -dr output to be parsed")


if __name__ == "__main__":
    # Parse options
    args = argparser.parse_args()

    # Parse objdump file
    funcs = parse_dissas(args.fname)

    has_errors = False
    for f in funcs:
        has_errors |= f.check()

    if has_errors:
        sys.exit(1)
    else:
        sys.exit(0)

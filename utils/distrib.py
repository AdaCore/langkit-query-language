#! /usr/bin/env python

import argparse
import os.path
from subprocess import check_call
import sys

from e3.fs import cp, mkdir, sync_tree, rm


parser = argparse.ArgumentParser(
    description="Helper script to build and distribute LKQL in production"
)
parser.add_argument("--build-mode")
parser.add_argument("--build-dir")
parser.add_argument("--package-dir")
parser.add_argument("--library-types")
parser.add_argument("--jobs")
parser.add_argument("--coverage", action="store_true")


args = parser.parse_args()
src_dir = os.path.abspath(os.path.join(os.path.dirname(__file__), ".."))
manage_script = os.path.join(src_dir, "lkql", "manage.py")

lkql_build_dir = os.path.join(args.build_dir, "lkql")
lkql_checker_build_dir = os.path.join(args.build_dir, "lkql_checker")


def run(*argv):
    print("\nRunning:", " ".join(argv))
    sys.stdout.flush()
    check_call(argv, cwd=args.build_dir)

def run_manage(verb, *argv):
    run(
        manage_script,
        verb,
        f"--build-dir={lkql_build_dir}",
        f"--build-mode={args.build_mode}",
        f"--library-types={args.library_types}",
        *argv
    )


# Start from a clean install directory
rm(args.package_dir, recursive=True)
mkdir(args.package_dir)

# Setup build directories
mkdir(lkql_build_dir)
mkdir(lkql_checker_build_dir)

# Build and install the Liblkqllang library
make_args = ["--coverage"] if args.coverage else []
run_manage("make", *make_args)
run_manage("install", args.package_dir)

# Make the Liblkqllang library available to the environment for the next builds
os.environ["GPR_PROJECT_PATH"] = os.path.pathsep.join([
    d for d in [
        os.path.join(args.package_dir, "share", "gpr"),
        os.environ.get("GPR_PROJECT_PATH"),
    ] if d
])

# Build and install the "lkql_checker program. Instrument it first, if coverage
# is requested. Note that we just copy the sources to the build directory since
# "gnatcov instrument" does not support build tree relocation.
sync_tree(
    os.path.join(src_dir, "lkql_checker"),
    lkql_checker_build_dir,
    delete=False,
)
gpr_file = os.path.join(lkql_checker_build_dir, "lkql_checker.gpr")
if args.coverage:
    run(
        "gnatcov",
        "instrument",
        f"-P{gpr_file}",
        "--level=stmt",
        "--no-subprojects",
        "--dump-trigger=atexit",
    )
common_args = [
    "-p",
    f"-P{gpr_file}",
    f"-XBUILD_MODE={args.build_mode}",
]
gprbuild_args = list(common_args)
if args.coverage:
    gprbuild_args += [
        "--src-subdirs=gnatcov-instr",
        "--implicit-with=gnatcov_rts_full",
    ]
run("gprbuild", f"-j{args.jobs}", *gprbuild_args)

# Install lkql_checker.gpr only when needed (i.e. to run coverage analysis)
install_mode = "dev" if args.coverage else "usage"
run(
    "gprinstall",
    f"--prefix={args.package_dir}",
    f"--mode={install_mode}",
    *common_args
)

# Ship the rules
sync_tree(
    os.path.join(src_dir, "lkql_checker", "share"),
    os.path.join(args.package_dir, "share"),
    delete=False,
)

# Install the "lkql_repl.py" script
cp(
    os.path.join(src_dir, "lkql_repl.py"),
    os.path.join(src_dir, "bin"),
)

# Ship coverage data files for both liblkqllang and lkql_checker so that the
# testsuite can use them.
if args.coverage:
    instr_dir = os.path.join(os.path.join(args.package_dir, "instr"))

    def copy_sid_files(filenames, dirnames):
        """
        Copy SID files (``filenames``, a glob pattern) to each of the given
        directories (``dirnames``, a list of directories).
        """
        for d in dirnames:
            cp(filenames, d)

    # Copy SID files for liblkqllang to each library directory
    copy_sid_files(
        os.path.join(lkql_build_dir, "obj", "instr", "sids", "*.sid"),
        [
            os.path.join(args.package_dir, "lib", f"liblkqllang.{lib_type}")
            for lib_type in args.library_types.split(',')
        ],
    )

    # Likewise for lkql_checker
    lib_dir = os.path.join(args.package_dir, "lib", "lkql_checker")
    mkdir(lib_dir)
    copy_sid_files(
        os.path.join(lkql_checker_build_dir, "obj", args.build_mode, "*.sid"),
        [lib_dir],
    )

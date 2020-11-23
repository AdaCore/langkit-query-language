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


args = parser.parse_args()
src_dir = os.path.abspath(os.path.join(os.path.dirname(__file__), ".."))
lkql_build_dir = os.path.join(args.build_dir, "lkql")
lkql_checker_build_dir = os.path.join(args.build_dir, "lkql_checker")
manage_script = os.path.join(src_dir, "lkql", "manage.py")


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
run_manage("make")
run_manage("install", args.package_dir)

# Make the Liblkqllang library available to the environment for the next builds
os.environ["GPR_PROJECT_PATH"] = os.path.pathsep.join([
    d for d in [
        os.path.join(args.package_dir, "share", "gpr"),
        os.environ.get("GPR_PROJECT_PATH"),
    ] if d
])

# Build and install the "lkql_checker program
gpr_file = os.path.join(src_dir, "lkql_checker", "lkql_checker.gpr")
common_args = [
    "-p",
    f"-P{gpr_file}",
    f"--relocate-build-tree={lkql_checker_build_dir}",
    f"-XBUILD_MODE={args.build_mode}",
]
run("gprbuild", f"-j{args.jobs}", *common_args)
run("gprinstall", f"--prefix={args.package_dir}", "--mode=usage", *common_args)

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

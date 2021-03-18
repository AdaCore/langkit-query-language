#! /usr/bin/env python3

"""
Utility script to automatically create lkql_checker tests

Provide the test name, the rule that you want to test, and the Ada files that
you want lkql_checker to be ran on.

TODO: add suport for rule arguments
"""

import argparse
from pathlib import Path
from os import path as P
import shutil
import subprocess
import sys

script_path = P.dirname(P.dirname(P.realpath(__file__)))

a = argparse.ArgumentParser(description=__doc__)
a.add_argument('-r', '--rule-name', type=str, required=True)
a.add_argument('--rule-arg', type=str, nargs='+')
a.add_argument('-t', '--test-name', type=str, required=True)
a.add_argument(
    'ada_files', help='Ada files to add to the test', type=str, nargs='+'
)

YAML_TEMPLATE = """
driver: 'checker'
rule_name: {rule_name}
input_sources: {input_sources}
"""

if __name__ == "__main__":
    args = a.parse_args()
    test_dir_path = Path(
        script_path) / "testsuite" / "tests" / "checks" / args.test_name
    try:
        test_dir_path.mkdir(parents=True)
    except Exception:
        print(f"ERROR: test directory already exists - {test_dir_path}")
        sys.exit(1)

    # Copy the Ada files to the test dir
    for file_path in args.ada_files:
        shutil.copy(file_path, test_dir_path)

    ada_files_names = [P.basename(f) for f in args.ada_files]

    # Write the test yaml
    with open(test_dir_path / "test.yaml", "w") as test_yaml:
        yaml = YAML_TEMPLATE.format(
            input_sources=ada_files_names, rule_name=args.rule_name
        )
        test_yaml.write(yaml)

    # Write the test content
    with open(test_dir_path / "test.out", "w") as f:
        f.write("")

    # Run lkql_checker on the files with given rule
    o = subprocess.check_output([
        "python", "testsuite/testsuite.py",
        "-E", "-r", args.test_name
    ], cwd=script_path, stderr=subprocess.STDOUT)

    # Verify that test is fine:
    subprocess.check_call([
        "python", "testsuite/testsuite.py", "-E", args.test_name
    ], cwd=script_path)

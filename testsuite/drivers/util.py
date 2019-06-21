from e3.fs import mkdir
from e3.os.process import Run
import os
import sys

TESTSUITE_ROOT_DIR = os.path.dirname(
    os.path.dirname(os.path.abspath(__file__))
)

INTERPRETER_PATH = os.path.join(TESTSUITE_ROOT_DIR, 'lkql_ada_interpreter',
                                'obj', 'main')


def make_interpreter(gcov=False):
    print "Compiling the interpreter (gcov=%s)" % gcov
    build_dir = os.path.join(TESTSUITE_ROOT_DIR, 'build')
    mkdir(build_dir)

    # Compute gprbuild invocation
    gprbuild = [
        'gprbuild', os.path.join('..', 'lkql_ada_interpreter',
                                 'lkql_ada_interpreter.gpr'), '-j0'
    ]

    if gcov:
        gprbuild.extend([
            '-cargs', '-fprofile-arcs', '-ftest-coverage', '-gargs',
            '-largs', '-fprofile-arcs', '-ftest-coverage'
        ])

    p = Run(gprbuild, cwd=build_dir, output=sys.stdout)
    assert p.status == 0, ("interperter build failed:\n%s" % p.out)


def read_to_string(path):
    with open(path, 'r') as f:
        text = f.read()
    return text.strip()

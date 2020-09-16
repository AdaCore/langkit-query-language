# langkit-query-language

Query language for Libadalang and Langkit. Allows to run queries on Ada codebase using the query language.

The syntax is not stable for now. Check-out [tests](testsuite/tests) for
examples.

## Architecture

[`lkql/`](lkql): Langkit language definition for the LKQL parser.

`lkql_interpreter.gpr`: Interpreter for the LKQL query language. Library
    project, usable as a library.

[`lkql_ada_interpreter/`](lkql_ada_interpreter): LKQL command line interpreter,
    that can run LKQL scripts on a given Ada codebase.

[`lkql_checker`](lkql_checker): LKQL command line checker. Can run "checks"
    that will flag specific    lines of code in a given Ada codebase.

## Building & using

### Prerequisites

You need to have GNAT, langkit and libadalang all available and in the proper
paths.

### Build steps

- Build LKQL:

```
lkql/manage.py make
```

- Make LKQL available

```
eval `lkql/manage.py setenv`
```

- Build the projects you need:

```
gprbuild -Plkql_interpreter.gpr -p
gprbuild -Plkql_ada_interpreter/lkql_ada_interpreter.gpr -p
gprbuild -Plkql_checker/lkql_checker.gpr -p
```

### Running the testsuite

```
cd testsuite
# Run the testsuite, keep the temp results in tmp
python testsuite.py -Edtmp
```

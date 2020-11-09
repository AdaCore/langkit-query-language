# LKQL

Query language for Libadalang and Langkit. Allows to run queries on Ada
codebase using a custom designed query language called LKQL.

The syntax is not stable for now. Check-out [tests](testsuite/tests) for
examples.

## Architecture

[`lkql/`](lkql): Contains:
* Langkit language definition for the LKQL parser
* Implementation of the LKQL interpreter (in
  [`lkql/extensions/src`](lkql/extensions/src)). Embedding the LKQL interpreter
  only requires using the `lkql` project.
* Command line app for the interpreter

[`lkql_checker`](lkql_checker): LKQL command line checker. Can run "checks"
    that will flag specific    lines of code in a given Ada codebase.

## Building & using

### Prerequisites

You need to have GNAT, langkit and libadalang all available and in the proper
paths.

### Build steps

- Building LKQL is done via

```
lkql/manage.py make
```

- You can then make LKQL available (binaries, Ada libraries, and Python lib)
  via:

```
eval `lkql/manage.py setenv`
```

- LKQL checker (linter based on lkql) is built separately:

```
gprbuild -P lkql_checker/lkql_checker.gpr -p
```

### Running the testsuite

```
cd testsuite
# Run the testsuite, keep the temp results in tmp
python testsuite.py -dtmp
```

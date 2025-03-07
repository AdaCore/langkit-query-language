# LKQL

Query language for Libadalang and Langkit. Allows to run queries on Ada
codebase using a custom designed query language called LKQL.

The syntax is not stable for now.

## High level overview

[`lkql/`](lkql): Langkit language definition for the LKQL parser.

[`lkql_checker/`](lkql_checker): GNATcheck sources and predefined LKQL rules.

[`lkql_jit/`](lkql_jit): LKQL reference implementation using the Truffle Java
   framework.

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

You can run the performance testsuite with the following command:

```
cd testsuite
python testsuite.py --perf-mode result.out [--perf-no-profile]
```

NOTE: You must checkout the libadalang internal testsuite in the `testsuite/ada_projects`
directory to be able to run performances tests.

### Adding a test

To add a test case in the testsuite you must follow the e3-testsuite standard. Moreover
you have to annotate Ada flagged lines when using `checker` and `gnatcheck` drivers.
To annotate an Ada line as flagged you must follow this syntax:

```ada
   My_Function (My_Arg);  -- FLAG This line should be flagged by the test case
```

If a line is flagged multiple times by the test case you can use this syntax:

```ada
   My_Other_Function;  -- FLAG (2) This line should be flagged two times
```

If you want to ensure a line is not flagged by the test case you can use the `NOFLAG`
annotation:

```ada
   Not_Flagged (My_Arg);  -- NOFLAG
```

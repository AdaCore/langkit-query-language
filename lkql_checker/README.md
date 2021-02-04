# LKQL checker

Prototype checker/linter based on LKQL and Libadalang.

The following instructions are to be interpreted in this `lkql_checker`
repo subdirectory.

## How to write checks

Checks are written in the LKQL language, and put in the
[`share/lkql`](share/lkql) directory. Each `.lkql` file represents a distinct
checker. The naming convention of the checkers is
``lowercase_with_underscores``.

Here is a simple checker example, that will just flag every body.

```
@check
fun bodies() = select Body
```

Adding this source in the `body.lkql` file in the `share/lkql` directory will
add a check to LKQL checker dynamically, without need to recompile LKQL
checker.

## Running

Running the checker will by default run all the checks. 

```
bin/lkql_checker [-P project | list of files]
```

If you want to run a specific check, you can add the name of the check after `-r`:

```
bin/lkql_checker [-P project | list of files] -r rule_name
```

There is no way to list checks from the command line for now, just explore the
`share/lkql` directory.

# LKQL checker

Prototype checker/linter based on LKQL and Libadalang.

The following instructions are to be interpreted in this `lkql_checker`
repo subdirectory.

You will find more information about how to write rules in the LKQL user
manual.

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

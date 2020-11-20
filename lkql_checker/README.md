# LKQL checker

Prototype checker/linter based on LKQL and Libadalang.

The following instructions are to be interpreted in this `lkql_checker`
repo subdirectory.

## Architecture

Checks are written in the LKQL language, and put in the
[`share/lkql`](share/lkql) directory. They then need to be registered in the
`share/lkql/rules.json` file. Each entry in the top level JSON array represents
a check.  You can add a new one by adding an entry.  Here is an example:

```json
{
    "name": "Deep_Inheritance",
    "path": "deepInheritance.lkql",
    "params": [
        {
            "name": "depth",
            "type": "int",
            "default": 2
        }
    ]
}
```

## Running

Running the checker will run all checks. For the moment there is no way to
select which checks are ran.

```
bin/lkql_checker [-P project | list of files]
```

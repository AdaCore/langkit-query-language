# LKQL_checker

Prototype checker/linter based on LKQL and Libadalang.

## Architecture

Checks are written in lkql, and put in the [`lkql`](lkql) directory. They then
need to be registered in the `rules.json` file. Each entry in the top level
json array represents a check. You can add a new one by adding an entry. Here
is an example:

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
obj/check [-P project | list of files]
```

Internal dev guide (for AdaCore developers)
-------------------------------------------

## Dev env setup

### Setup

1. Make sure you have a Python 3.7+ distribution (with `pip3`) available in
   your environment. AdaCore's `e3-distrib` package is completely fine.

2. Checkout LKQL

```sh
git clone git.adacore.com:langkit-query-language
cd langkit-query-language
```

3. Install Python dependencies via `pip`:

```sh
pip3 install -r requirements.txt
```

2. Install other dependencies via Anod

```sh
# In your sandbox dir
anod install gnat
anod install langkit_support -Qlalmaster
anod install libadalang -Qlalmaster
```

### Environment

Everytime you want to work with the query language, you need to make tools and
libraries available in your environment. To achieve this, you need to run these
commands:

```sh
# In your sandbox dir
eval `anod printenv gnat`
eval `anod printenv langkit_support -Qlalmaster`
eval `anod printenv libadalang -Qlalmaster`
```

And you also need to define the following environment variables:

```sh
# In your langkit-query-language checkout:
GPR_PROJECT_PATH="$PWD/lkql/build:$GPR_PROJECT_PATH"
export GPR_PROJECT_PATH

LD_LIBRARY_PATH="$PWD/lkql/build/lib/relocatable/dev:$LD_LIBRARY_PATH"
export LD_LIBRARY_PATH

PYTHONPATH="$PWD/lkql/build/python:$PYTHONPATH"
export PYTHONPATH
```

Or create an env script:

```sh
(
    cd /path/to/your/sandbox
    anod printenv gnat
    anod printenv langkit_support -Qlalmaster
    anod printenv libadalang -Qlalmaster

    cd /path/to/your/lkql/checkout
    GPR_PROJECT_PATH="$PWD/lkql/build:$GPR_PROJECT_PATH"
    export GPR_PROJECT_PATH

    LD_LIBRARY_PATH="$PWD/lkql/build/lib/relocatable/dev:$LD_LIBRARY_PATH"
    export LD_LIBRARY_PATH

    PYTHONPATH="$PWD/lkql/build/python:$PYTHONPATH"
    export PYTHONPATH
) >> path/to/env_script.sh
```

and then run `source path/to/env_script.sh`.

### Build `liblkqllang` and `lkql_checker`

Just run `make` in LKQL's checkout.

## Adding checks

To add checks to `lkql_checker`, go [here](lkql_checker/).

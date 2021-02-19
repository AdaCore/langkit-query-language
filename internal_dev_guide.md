Internal dev guide (for AdaCore developers)
-------------------------------------------

## Dev env setup

### Setup

First step, which is one shot, is to setup a development environment. Follow
these steps:

1. Install dependencies via Anod

```sh
anod install gnat
anod install langkit_support -Qlalmaster
anod install langkit -Qlalmaster
anod install libadalang -Qlalmaster
```

2. Checkout LKQL

```sh
git clone git@github.com:AdaCore/langkit-query-language.git
```

3. Make sure you have Python 3.8+ and associated `pip` in your environment,
   either via a `virtualenv`, or via `e3-distrib`.

4. Install Python dependencies:

```sh
pip install prompt_toolkit
pip install railroad-diagrams
pip install pygments
```

### Environment

Second step is to put every tool and library in the path. You can either run
these commands every time:

```sh
# In your sandbox dir
eval `anod printenv gnat`
eval `anod printenv langkit_support -Qlalmaster`
eval `anod printenv langkit -Qlalmaster`
eval `anod printenv libadalang -Qlalmaster`
```

You also need to define the following environment variables:

```sh
# In your langkit-query-language checkout
GPR_PROJECT_PATH="$PWD/lkql/build:$GPR_PROJECT_PATH"
export GPR_PROJECT_PATH

LD_LIBRARY_PATH="$PWD/lkql/build/lib/relocatable/dev:$LD_LIBRARY_PATH"
export LD_LIBRARY_PATH

PYTHONPATH="$PWD/lkql/build/python:$PYTHONPATH"
export PYTHONPATH
``

Or create an env script:

```sh
(
    cd /path/to/your/sandbox
    anod printenv gnat
    anod printenv langkit_support -Qlalmaster
    anod printenv langkit -Qlalmaster
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

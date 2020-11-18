Internal dev guide (for AdaCore developers)
-------------------------------------------

## Dev env setup

### Setup

First step, which is one shot, is to setup a development environment. Follow
those steps:

1. Install dependencies via Anod

```sh
anod install gnat
anod install langkit_support -Qcompiler=bootstrap -Qlalmaster
anod install libadalang -Qcompiler=bootstrap -Qlalmaster
anod install lkql
```

2. Checkout LKQL

```sh
git checkout git@github.com:AdaCore/langkit-query-language.git
```

3. Make sure you have Python 3.8+ and associated `pip` in your environment,
   either via a `virtualenv`, or via `e3-distrib`.

4. Install Python dependencies:

```sh
pip install prompt_toolkit
```

### Environment

Second step is to put every tool and library in the path. You can either run
those commands everytime:

```sh
# In your sandbox dir
eval `anod printenv gnat`
eval `anod printenv langkit_support -Qcompiler=bootstrap -Qlalmaster`
eval `anod printenv libadalang -Qcompiler=bootstrap -Qlalmaster`
eval `anod printenv lkql`
```

Or create an env script:

```sh
# In your sandbox dir
(
    anod printenv gnat
    anod printenv langkit_support -Qcompiler=bootstrap -Qlalmaster
    anod printenv libadalang -Qcompiler=bootstrap -Qlalmaster
    anod printenv lkql
) >> path/to/env_script.sh
```

and then run `source path/to/env_script.sh`.

### Build `lkql_checker`

Just run `make lkql_checker` in LKQL's checkout.

## Adding checks

To add checks to `lkql_checker`, go [here](lkql_checker/).

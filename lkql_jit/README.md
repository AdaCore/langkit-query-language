LKQL JIT
========

The JIT compiler for the LKQL language.

Dependencies
------------

* [GraalVM](https://www.graalvm.org/) : The high performance JVM (version 24.2.1)
* [Maven](https://maven.apache.org/) : Java building tool
* [Python3](https://www.python.org/) : The language (3.9 or later)
* [Langkit](https://github.com/AdaCore/langkit) : The language front-end creation framework
* [Libadalang](https://github.com/AdaCore/libadalang)
  and [Liblkqllang](https://github.com/AdaCore/langkit-query-language) and their Java bindings

How to build and run
--------------------

First, you need to ensure that the `mvn` command is accessible and fairly recent (at least 3.8.5).
Then, make sure you have an accessible Python3 installation, version 3.9 or later.
Also make sure that your GraalVM installation is in the 24.2.1 version.
Finally, make sure that `native-image` tool is available in your environment.

### 1) Install Langkit and make sure it is usable

```sh
$[langkit]> pip3 install -r requirement-pypi.txt
$[langkit]> ./manage.py make
$[langkit]> eval `./manage.py printenv`
$[langkit]> export PATH="/path/to/langkit/scripts:$PATH"
```

If you get a Python error about the Langkit library, add the Langkit directory to your `PYTHONPATH`
environment variable.

*Note: If for some reason you are not running `./manage.py printenv`, make sure that the
`sigsegv_handler` library is reachable from your `LD_LIBRARY_PATH`.*

Install the Java LangkitSupport library:

```sh
$[langkit]> mvn -f langkit/java_support install
```

### 2) Build and install Libadalang

```sh
$[libadalang]> lkm make --enable-java
$[libadalang]> eval `lkm printenv`
```

Make sure the Java bindings are installed locally

```sh
$[libadalang]> mvn -f build/java install
```

### 3) Build and install Liblkqllang

```sh
$[langkit-query-language/lkql]> lkm make --enable-java
$[langkit-query-language/lkql]> eval `lkm printenv`
```

As for Libadalang, make sure Java bindings are installed

```sh
$[langkit-query-language/lkql]> mvn -f build/java install
```

### 4) Export the needed environment variables :

* `JAVA_HOME` should points to your local GraalVM installation
* `GRAAL_HOME` should also points to your GraalVM installation

### 5) Build and run LKQL_JIT

```sh
$[langkit-query-language/lkql_jit]> mvn clean package
```

You can now access LKQL JIT by running the `langkit-query-language/lkql_jit/standalone/target/lkql.py`
script.

This script will launch LKQL JIT with the current accessible Java installation (`java -jar ...`)
so you have to run it in a valid environment.

If you want to build LKQL JIT as a native executable, you can use the `native` Maven profile:

```sh
$[langkit-query-language/lkql_jit]> mvn clean package -P native
```

Then the `lkql` executable will be available under the `langkit-query-language/lkql_jit/standalone/target`
directory.

### 6) Running the code formatter

LKQL JIT project uses `prettier-java` as code formatter, and the `spotless` Maven plugin is configured
to run it on demand. You can run the following command to format all Java sources:

```sh
$[langkit-query-language/lkql_jit]> mvn spotless:apply
```

However, running it is very prone to error, thus you can install pre-commit
hooks to perform this formatting operation on each commit:

```sh
$[langkit-query-language]> pre-commit install
```

Debugging LKQL JIT
------------------

To debug LKQL JIT native-image builds you can use the `debug` maven profile:

```sh
$[langkit-query-language/lkql_jit]> mvn clean package -P native,debug
```

If you need to debug or profile LKQL JIT native-image binaries there is the `dev` profile which
enable support for tools like `valgrind`, `gdb` or `perf` on the produced binaries.

```sh
$[langkit-query-language/lkql_jit]> mvn clean package -P native,dev
```

If you don't need or want any of those, use the `prod` profile to disable all debugging and
profiling information.

```sh
$[langkit-query-language/lkql_jit]> mvn clean package -P native,prod
```

LKQL JIT
========

The JIT compiler for the LKQL language.

Dependencies
------------

* [GraalVM](https://www.graalvm.org/) : The high performance JVM
* [Maven](https://maven.apache.org/) : Java project manager
* [Langkit](https://github.com/AdaCore/langkit) : The language front-end creation framework
* [Libadalang](https://github.com/AdaCore/libadalang) and [Liblkqllang](https://github.com/AdaCore/langkit-query-language) and their Java bindings

How to build and run
--------------------

First, you need to ensure that the `mvn` command is accessible and fairly recent (at least 3.8.5).

You also need to install GraalVM's `native-image` utility, which can be done running `gu install native-image` (where `gu` is in the `bin` directory of the GraalVM installation).

Also make sure that your GraalVM installation is for Java 17, as Java 19 is not supported for now. for information, the tested version of GraalVM is graalvm-ce-java17-22.3.1.

1) Install Langkit and make sure it is usable

```sh
$[langkit]> ./manage.py make
$[langkit]> eval `./manage.py setenv`
```
If you get a Python error about the langkit library, add the langkit directory to your `PYTHONPATH` environment variable.

*Note: If for some reason you are not running `./manage.py setenv`, make sure that the `sigsegv_handler` library is reachable from your `LD_LIBRARY_PATH`.*

2) Build and install Libadalang

```sh
$[libadalang]> ./manage.py make --enable-java
$[libadalang]> eval `./manage.py setenv`
```

Make sure the Java bindings are installed locally

```sh
$[libadalang/build/java]> mvn install
```

3) Build and install Liblkqllang

```sh
$[langkit-query-language/lkql]> ./manage.py make --enable-java
$[langkit-query-language/lkql]> eval `./manage.py setenv`
```

Like Libadalang, make sure Java bindings are installed

```sh
$[langkit-query-language/lkql/build/java]> mvn install
```

4) Export the needed environment variables :

  * `GRAAL_HOME` should points to your GraalVM installation

5) Compile and run LKQL_JIT

```sh
$[langkit-query-language/lkql_jit]> mvn clean install
```

You can now directly run the checker using the `lkql_jit_checker` bash script that has been installed in the `languages/lkql/bin` directory of your GraalVM installation (Linux only for now).

If you want to build the checker driver as a native image, you can use the `native-checker` profile

```sh
$[langkit-query-language/lkql_jit]> mvn clean install -P native-checker
```

*Other supported profiles are `native-launcher` and `native-all` (to build `native-checker` as well as `native-launcher`).*

Native executables are in the `langkit-query-language/lkql_jit/native/bin` directory and installed in your GraalVM guest languages folder
(`$GRAAL_HOME/languages/lkql/bin`).

Debugging LKQL JIT
------------------

To debug LKQL_JIT native-image build you can use the `debug` maven profile.

```sh
$[langkit-query-language/lkql_jit]> mvn clean install -P native-checker,debug
```

If you need to debug or profile LKQL_JIT native-image binaries there is the `dev` profile which enable support for tools like
`valgrind`, `gdb` or `perf` on the produced binaries.

```sh
$[langkit-query-language/lkql_jit]> mvn clean install -P native-checker,dev
```

If you don't need or want any of those, use the `prod` profile to disable all debugging and profiling information.

```sh
$[langkit-query-language/lkql_jit]> mvn clean install -P native-checker,prod
```

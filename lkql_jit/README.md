LKQL JIT
========

The JIT compiler for the LKQL language.

Dependencies
------------

* [GraalVM](https://www.graalvm.org/) : The high performance JVM
* [Maven](https://maven.apache.org/) : Java project manager
* [Langkit](https://github.com/AdaCore/langkit) : The language front-end creation framework
* [Libadalang](https://github.com/AdaCore/libadalang) and [Liblkqllang](https://github.com/AdaCore/langkit-query-language) and their Java bindings

How to build
------------

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

5) Compile the LKQL_JIT

```sh
$[langkit-query-language/lkql_jit]> mvn clean install
```

If you want to build the checker driver you can use the `checker` profile

```sh
$[langkit-query-language/lkql_jit]> mvn clean install -Pchecker
```

*Other supported profiles are `launcher` and `all` (to build `checker` as well as `launcher`).*

Native executables are in the `langkit-query-language/lkql_jit/native/bin` directory and installed in your GraalVM guest languages folder.

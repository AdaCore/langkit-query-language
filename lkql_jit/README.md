LKQL JIT
======

The JIT compiler for the LKQL language.

Dependencies
------

* [GrallVM](https://www.graalvm.org/) : The high performance JVM
* [Langki](https://github.com/AdaCore/langkit) : The language front-end creation framework
* [Libadalang](https://github.com/AdaCore/libadalang) and [Liblkqllang](https://github.com/AdaCore/langkit-query-language) and their Java bindings
  (see DOC for Java bindings generation)

How to build
------

* Export the needed environment variables :
  * Set the environment variables of `libadalang` and `liblkqllang` (`./manage.py setenv` in language directories)
  * `GRAAL_HOME` Environment variable that points to your GraalVM installation root

* `$> mvn clean install -Pchecker` (Development fast build without native binaries)

How to run
------

* Export the needed environment variables :
  * As building phase, set the `libadalang` and `liblkqllang` environment variables
  * Add the `langkit/sigsegv_handler/lib` directory to the `LD_LIBRARY_PATH` environment variable

* `$> native/bin/lkql_jit(_checker) [PARAM]` to run the JIT

How to test
------

* Export the environment variable `LKQL_JIT` as `true` and all variables needed to run lkql_jit
* Run the lkql testsuite

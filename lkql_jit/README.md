LKQL JIT
========

The JIT compiler for the LKQL language.

Dependencies
------------

* [GraalVM](https://www.graalvm.org/) : The high performance JVM
* [Maven](https://maven.apache.org/) : Java project manager
* [Python3](https://www.python.org/) : The language (3.9 or later)
* [Langkit](https://github.com/AdaCore/langkit) : The language front-end creation framework
* [Libadalang](https://github.com/AdaCore/libadalang)
  and [Liblkqllang](https://github.com/AdaCore/langkit-query-language) and their Java bindings

How to build and run
--------------------

First, you need to ensure that the `mvn` command is accessible and fairly recent (at least 3.8.5).

Then, make sure you have an accessible Python3 installation. Python 3.9 or later.

You also need to install GraalVM's `native-image` utility, which can be done
running `gu install native-image` (where `gu` is in the `bin` directory of the GraalVM
installation).

Also make sure that your GraalVM installation is for Java 17, as Java 19 is not supported for now.
for information, the tested version of GraalVM is graalvm-ce-java17-22.3.1.

### 1) Install Langkit and make sure it is usable

```sh
$[langkit]> pip3 install -r requirement-pypi.txt
$[langkit]> ./manage.py make
$[langkit]> eval `./manage.py setenv`
```

If you get a Python error about the langkit library, add the langkit directory to your `PYTHONPATH`
environment variable.

*Note: If for some reason you are not running `./manage.py setenv`, make sure that the
`sigsegv_handler` library is reachable from your `LD_LIBRARY_PATH`.*

### 2) Build and install Libadalang

```sh
$[libadalang]> ./manage.py make --enable-java
$[libadalang]> eval `./manage.py setenv`
```

Make sure the Java bindings are installed locally

```sh
$[libadalang]> mvn -f build/java install
```

### 3) Build and install Liblkqllang

```sh
$[langkit-query-language/lkql]> ./manage.py make --enable-java
$[langkit-query-language/lkql]> eval `./manage.py setenv`
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
$[langkit-query-language/lkql_jit]> mvn clean install
```

You can now access the LKQL JIT launcher using the `lkql_jit` bash script that has been
installed in the `languages/lkql/bin` directory of your GraalVM installation (Linux only for now).

This bash script will launch LKQL JIT with the current accessible Java installation (`java -jar
lkql_jit_launcher.jar ...`) so you have to run it in a valid environment.

If you want to build LKQL JIT as a native executable, you can use the `native-launcher` Maven
profile:

```sh
$[langkit-query-language/lkql_jit]> mvn clean install -P native-launcher
```

Then the `lkql_jit` executable will be available under the
`langkit-query-language/lkql_jit/native/bin` directory and will be installed to your local
GraalVM guest languages folder (`$GRAAL_HOME/languages/lkql/bin`).

LKQL JIT has currently 3 drivers:

* `launcher` : A simple driver to run LKQL JIT with a given LKQL source.
* `checker` : A driver to run LKQL rules on Ada code bases, this is mainly used to test the LKQL
  language implementation.
* `gnatcheck_worker` : A driver which is not designed to be user from the command line. This is
  used by the main GNATcheck executable to run LKQL rules.

When building LKQL JIT you can define which drivers you want to build as native executables with
the Maven profiles `native-launcher`, `native-checker` and `native-gnatcheck_worker`. If you
want to build them all just use the `native-all` profile.

### 6) Running the code formatter

LKQL JIT project uses the `google-java-format` as code formatter. It is run every time you call
the `compile` Maven phase and will raise an error if there is a style violation in the LKQL JIT
source code. To automatically format all Java files you can run the following command:

```sh
$[langkit-query-language/lkql_jit] mvn spotless:apply
```

Debugging LKQL JIT
------------------

To debug LKQL_JIT native-image build you can use the `debug` maven profile.

```sh
$[langkit-query-language/lkql_jit]> mvn clean install -P native-checker,debug
```

If you need to debug or profile LKQL_JIT native-image binaries there is the `dev` profile which
enable support for tools
like
`valgrind`, `gdb` or `perf` on the produced binaries.

```sh
$[langkit-query-language/lkql_jit]> mvn clean install -P native-checker,dev
```

If you don't need or want any of those, use the `prod` profile to disable all debugging and
profiling information.

```sh
$[langkit-query-language/lkql_jit]> mvn clean install -P native-checker,prod
```

Configuring Intellij IDEA
-------------------------

To develop for LKQL JIT you may use Intellij IDEA since the LKQL JIT project use and contains a
configuration for this IDE. You just have to open the LKQL JIT project (`lkql_jit/pom.xml`) with
Intellij to initialize the project.

However, LKQL JIT project use `google-java-format` to format the Java source code, so you may
configure Intellij to use this formatter and applying it when saving Java files. To make this
you have to follow those steps:

### 1) Install Intellij plugin

Go to `File > Settings > Plugins` and search for "google-java-format" in the marketplace then
install it.

### 2) Enable and configure the Intellij plugin

You now have to enable the "google-java-format" plugin: go to `File > Settings > Other Settings >
google-java-format Settings` and check the `Enable google-java-format` option. Then you have to
select the `Android Open Source Project (AOSP) style` in the `Code style` menu.

### 3) Configure the Intellij JRE

The google-java-format plugin uses some internal classes that aren't available without extra
configuration. You have to go to `Help > Edit Custom VM Options...` then past these lines:

```
--add-exports=jdk.compiler/com.sun.tools.javac.api=ALL-UNNAMED
--add-exports=jdk.compiler/com.sun.tools.javac.code=ALL-UNNAMED
--add-exports=jdk.compiler/com.sun.tools.javac.file=ALL-UNNAMED
--add-exports=jdk.compiler/com.sun.tools.javac.parser=ALL-UNNAMED
--add-exports=jdk.compiler/com.sun.tools.javac.tree=ALL-UNNAMED
--add-exports=jdk.compiler/com.sun.tools.javac.util=ALL-UNNAMED
```

Then restart you IDE. Now `google-java-format` should have replaced the `Reformat Code` and
`Optimize Imports` actions.

### 4) Set formatting hook on file saving (optional)

If you want Intellij to automatically reformat the current file when it is saved you can go to
`File > Settings > Tools > Actions on save` then check the `Optimize imports` and `Reformat
code` options.

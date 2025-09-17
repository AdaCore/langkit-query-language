LKQL Driver
===========

Additionally to the ``gnatcheck`` executable, you can access the LKQL language
through the LKQL driver. This is an executable file named ``lkql`` which
defines several sub-commands, each being an entry point to the LKQL engine.

.. attention::

  While being shipped alongside GNATcheck, not all sub-commands are considered
  as stable, some are even experimental or internal and should be used with
  extreme caution.

.. _Sub_commands_List:



Sub-commands List
-----------------

Additionally to their specific switches, each sub-command accepts the
``--help`` flag which triggers the display of its specific help message and
exit.


``lkql refactor``
^^^^^^^^^^^^^^^^^

.. hint::

  This sub-command is considered as stable and is officially supported.

This sub-command is used to perform automatic refactoring operations. It is
mainly used to automatically migrate existing LKQL code-bases when a change is
made in the language syntax or sematic.

``refactor`` defines the following CLI switches:

``-i, --in-place``
  Apply refactoring directly into LKQL source files, modifying them.

``-r, --refactoring=<refactoring>``
  Name of the refactoring to apply to your LKQL files. You can view a list of
  available refactorings in the sub-command help message.

Additionally to those switches, the ``refactor`` sub-command expect a list of
LKQL files to apply the specified refactoring on. Here is an example usage:

.. code-block::

  lkql refactor -i -r=IS_TO_COLON file_1.lkql file_2.lkql


``lkql run``
^^^^^^^^^^^^

.. caution::

  This sub-command is considered as a beta feature: while being pretty stable,
  its interface may change in the future, and relying on it should be
  considered as unsafe.

This is the LKQL interpreter entry point, through it you can access the current
LKQL implementation to run any LKQL script or start a REPL. This is a good
entry point to test the LKQL language and write custom GNATcheck rules in an
iterative way.

``run`` defines the following CLI switches:

``-C, --charset=<charset>``
  Defines the charset to use for source decoding. The default is "utf-8".

``-i, --interactive``
  Start an LKQL REPL (read-eval-print loop). This switch is incompatible with
  the ``-S, --script-path`` one.

``--keep-going-on-missing-file``
  Don't stop the interpreter if an Ada source file is missing, just print a
  warning message instead.

``-P, --project=<project>``
  GPR file to fetch Ada sources from for the interpreter.

``--RTS=<runtime>``
  Ada runtime to use when resolving sources.

``-S, --script-path=<script>``
  Name of the LKQL script to run. This switch is incompatible with the
  ``-i, --interactive`` one.

``--target=<target>``
  Hardware target used to resolved Ada runtime sources.

``-U, --recursive``
  Process all units in the project tree, excluding externally built project.

``-v, --verbose``
  Enable the verbose mode.

Additionally to those switches, you can provide to the ``run`` sub-command a
list of Ada sources to use. Here is an example usage:

.. code-block::

  lkql run --keep-going-on-missing-file -S script.lkql main.adb


``lkql check``
^^^^^^^^^^^^^^

.. danger::

  This sub-command is considered as unstable and is not supported. Use it at
  your own risks.

This sub-command is used to run a set of LKQL rules on provided Ada sources.
This is an internal entry point mainly used to test GNATcheck rules.

``check`` defines the following CLI switches:

``-C, --charset=<charset>``
  Defines the charset to use for source decoding. The default is "utf-8".

``-j, --jobs=<n>``
  Number of jobs to use during analysis. If n is 0, spawn 1 job per CPU.

``-P, --project=<project>``
  GPR file to fetch Ada sources from for the interpreter.

``--RTS=<runtime>``
  Ada runtime to use when resolving sources.

``--target=<target>``
  Hardware target used to resolved Ada runtime sources.

``-U, --recursive``
  Process all units in the project tree, excluding externally built project.

``-v, --verbose``
  Enable the verbose mode.


``-r, --rule=<rule>``
  Enable the given rule for the current run. This option is cumulative.

``--rules-dir=<directory>``
  Additional directory to fetch LKQL rules from. This options is cumulative.

``-a, --rule-arg=<rule>.<arg>=<value>``
  Provide a value for a specific argument of a rule. This option is cumulative.

Additionally to those switches, you can provide to the ``check`` sub-command a
list of Ada sources to use during analysis. Here is an example usage:

.. code-block::

  lkql check main.adb main.ads -r my_rule -a "my_rule.arg=42"


``lkql fix``
^^^^^^^^^^^^

.. danger::

  This sub-command is considered as an experimental feature. Use it at your
  own risks.

Sub-command to run a set of auto-fixing functions on a set of sources. This is
an experimental entry point mainly used for testing purposes, but you can give
it a try (be careful, this process may alter your Ada sources).

``fix`` defines the same switches as ``lkql check`` sub-command, with some
additional ones:

``--auto-fix-mode=<mode>``
  The mode to use when applying auto-fixes. Available modes are:

  * ``DISPLAY``: Only display fixed sources in standard output, doesn't modify
    any source file
  * ``NEW_FILE``: For each source file, if it has some fixes, create a new file
    named ``<filename>.patched`` alongside the original one containing the
    patched source
  * ``PATCH_FILE``: Replace each source file that has fixes in them by their
    patched version

For now, there is no list of rules with an auto-fix function, but you can check
if a rule can be used with this sub-command by reading its source code and
checking for the ``auto_fix`` argument in its related ``@check`` annotation.


``lkql doc-api``
^^^^^^^^^^^^^^^^

.. danger::

  This sub-command is considered as unstable and is not supported. Use it at
  your own risks.

Entry point used to generate API documentation for LKQL modules in the RST
format. Each LKQL file defines a module and all top level symbols are
documented.

``doc-api`` defines the following CLI switches:

``-O, --output-dir=<directory>``
  Directory path to place generated RST files in.

``--std``
  Additionally to other generated files, generate the documentation of the LKQL
  prelude and built-in functions.

Additionally to those switches, the ``doc-api`` sub-command expect a list of
LKQL files to generate documentation for. Here is an example usage:

.. code-block::

  lkql doc-api -O=doc/ --std file_1.lkql file_2.lkql


``lkql doc-rules``
^^^^^^^^^^^^^^^^^^

.. danger::

  This sub-command is considered as unstable and is not supported. Moreover,
  some information are hard-coded in it, so it should be considered as an
  internal. Use it at your own risks.

Entry point used to generate documentation for a set of LKQL rules in the RST
format.

``doc-rules`` defines the following CLI switches:

``-O, --output-dir=<directory>``
  Directory path to place generated RST files in.

``-v, --verbose``
  Enable the verbose mode.

This sub-command also expect a list of directories containing LKQL rules to
generate the documentation for. Here is an example usage:

.. code-block::

  lkql doc-rules -O=rules_doc/ rules/ other_rules/


``lkql gnatcheck_worker``
^^^^^^^^^^^^^^^^^^^^^^^^^

.. danger::

  This sub-command is considered as internal and is not meant to be used from
  the command-line.

This is the entry point of the GNATcheck driver, and it is not meant to be used
outside this context. That's why this entry point won't be documented any
further.

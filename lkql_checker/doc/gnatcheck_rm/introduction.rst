.. _Introduction:

************
Introduction
************

The *gnatcheck* tool is a utility that checks properties
of Ada source files according to a given set of syntactic and semantic rules.

It can be used to enforce coding standards by analyzing Ada source programs
with respect to a set of *rules* supplied at tool invocation.

It can also be used as a static analysis tool to detect potential errors
(problematic or dangerous code patterns) or find areas of code improvement.

A number of rules are predefined in *gnatcheck* and are described
in :ref:`Predefined_Rules`. In addition, it is possible to write new rules
as described in :ref:`Writing_Your_Own_Rules`
using a dedicated pattern matching language called `LKQL`,
used to implement all the predefined rules.

Invoking *gnatcheck* on the command line has the form::

  $ gnatcheck [switches] {filename}
        [-files=arg_list_filename]
        -rules rule_options
        [-cargs gcc_switches] -rules rule_options

where

* `switches` specify the general tool options

* Each `filename` is the name (including the extension) of a source
  file to process, the file name may contain path information.

* `arg_list_filename` is the name (including the extension) of a text
  file containing the names of the source files to process, separated by spaces
  or line breaks.

* `rule_options` is a list of options for controlling a set of
  rules to be checked by *gnatcheck* (:ref:`gnatcheck_Rule_Options`).

* `gcc_switches` is a list of switches for
  *gcc*. They will be passed on to a compiler invocation made by
  *gnatcheck* to collect compiler warnings and to add them to the report
  file. Here you can provide e.g. ``-gnatxx`` switches such as ``-gnat2012``,
  etc.

Either a :file:`filename` or an :file:`arg_list_filename` must be
supplied.

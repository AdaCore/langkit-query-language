.. _Getting_Started:

***************
Getting Started
***************

The ``gnatcheck`` tool is a utility that checks properties
of Ada source files according to a given set of syntactic and semantic rules.

It can be used to enforce coding standards by analyzing Ada source programs
with respect to a set of *rules* supplied at tool invocation.

It can also be used as a static analysis tool to detect potential errors
(problematic or dangerous code patterns) or find areas of code improvement.

A number of rules are predefined in ``gnatcheck`` and are described
in :ref:`Predefined_Rules`. In addition, it is possible to write new rules
as described in :ref:`Writing_Your_Own_Rules`
using a dedicated pattern matching language called `LKQL`,
used to implement all the predefined rules.

Invoking ``gnatcheck`` on the command line has the form::

  $ gnatcheck [switches] {filename}
        [-files=arg_list_filename]
        -rules rule_options
        [-cargs gcc_switches] -rules rule_options

where

* `switches` specify the :ref:`General_gnatcheck_Switches` such as ``-P project``

* Each `filename` is the name (including the extension) of a source
  file to process, the file name may contain path information.

* `arg_list_filename` is the name (including the extension) of a text
  file containing the names of the source files to process, separated by spaces
  or line breaks.

* `rule_options` is a list of options for controlling a set of
  rules to be checked by ``gnatcheck`` (:ref:`gnatcheck_Rule_Options`).

* `gcc_switches` is a list of switches for
  ``gcc``. They will be passed on to a compiler invocation made by
  ``gnatcheck`` to collect compiler warnings and to add them to the report
  file. Here you can provide e.g. ``-gnatxx`` switches such as ``-gnat2012``,
  etc.

Either a :file:`filename` or an :file:`arg_list_filename` must be
supplied.

.. _Example_of_gnatcheck_Usage:

Example of GNATcheck Usage
--------------------------

Here is a complete example. Suppose that in the current directory we have a
project file named :file:`gnatcheck_example.gpr` with the following content:

.. code-block:: ada

  project Gnatcheck_Example is

     for Source_Dirs use ("src");
     for Object_Dir use "obj";
     for Main use ("main.adb");

     package Check is
        for Default_Switches ("ada") use ("-rules", "-from=coding_standard");
     end Check;

  end Gnatcheck_Example;


And the file named :file:`coding_standard` is also located in the current
directory and has the following content::

  -----------------------------------------------------
  -- This is a sample gnatcheck coding standard file --
  -----------------------------------------------------

  --  First, turning on rules, that are directly implemented in gnatcheck
  +RAbstract_Type_Declarations
  +RAnonymous_Arrays
  +RLocal_Packages
  +RFloat_Equality_Checks
  +REXIT_Statements_With_No_Loop_Name

  --  Then, activating compiler checks of interest:
  +RStyle_Checks:e
  --  This style check checks if a unit name is present on END keyword that
  --  is the end of the unit declaration


And the subdirectory :file:`src` contains the following Ada sources:

:file:`pack.ads`:

.. code-block:: ada

  package Pack is
     type T is abstract tagged private;
     procedure P (X : T) is abstract;

     package Inner is
        type My_Float is digits 8;
        function Is_Equal (L, R : My_Float) return Boolean;
     end Inner;
  private
     type T is abstract tagged null record;
  end;

:file:`pack.adb`:

.. code-block:: ada

  package body Pack is
     package body Inner is
        function Is_Equal (L, R : My_Float) return Boolean is
        begin
           return L = R;
        end;
     end Inner;
  end Pack;

and :file:`main.adb`:

.. code-block:: ada

  with Pack; use Pack;
  procedure Main is

     pragma Annotate
       (gnatcheck, Exempt_On, "Anonymous_Arrays", "this one is fine");
     Float_Array : array (1 .. 10) of Inner.My_Float;
     pragma Annotate (gnatcheck, Exempt_Off, "Anonymous_Arrays");

     Another_Float_Array : array (1 .. 10) of Inner.My_Float;

     use Inner;

     B : Boolean := False;

  begin
     for J in Float_Array'Range loop
        if Is_Equal (Float_Array (J), Another_Float_Array (J)) then
           B := True;
           exit;
        end if;
     end loop;
  end Main;

And suppose we call ``gnatcheck`` from the current directory using
the project file as the only parameter of the call::

     gnatcheck -Pgnatcheck_example.gpr


As a result, ``gnatcheck`` is called to check all the files from the
project :file:`gnatcheck_example.gpr` using the coding standard defined by
the file :file:`coding_standard`. The ``gnatcheck``
report file named :file:`gnatcheck.out` will be created in the ``obj``
directory, and it will have the following content::

  GNATCheck report

  date              : YYYY-MM-DD HH:MM
  gnatcheck version : gnatcheck XX.Y
  command line      : gnatcheck -Pgnatcheck_example.gpr
  runtime           : <default>
  coding standard   : coding_standard
  list of sources   : gnatcheck-source-list.out

  1. Summary

     fully compliant sources               : 0
     sources with exempted violations only : 0
     sources with non-exempted violations  : 3
     unverified sources                    : 0
     total sources                         : 3
     ignored sources                       : 0

     non-exempted violations               : 9
     rule exemption warnings               : 0
     compilation errors                    : 0
     exempted violations                   : 0
     internal errors                       : 0

  2. Exempted Coding Standard Violations

     no exempted violations detected

  3. Non-exempted Coding Standard Violations

  main.adb:6:20: anonymous array type
  main.adb:9:28: anonymous array type
  main.adb:19:12: exit statement with no loop name
  pack.adb:5:19: use of equality operation for float values
  pack.adb:6:09: (style) "end Is_Equal" required
  pack.ads:2:16: declaration of abstract type
  pack.ads:5:14: declaration of local package
  pack.ads:10:16: declaration of abstract type
  pack.ads:11:03: (style) "end Pack" required

  4. Rule exemption problems

     no rule exemption problems detected

  5. Language violations

     no language violations detected

  6. Gnatcheck internal errors

     no internal error detected

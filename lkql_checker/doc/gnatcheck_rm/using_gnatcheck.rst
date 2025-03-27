.. _using_gnatcheck:

***************
Using GNATcheck
***************


.. _General_gnatcheck_Switches:

General GNATcheck Switches
==========================

The following switches control the general ``gnatcheck`` behavior


  .. index:: --version


``--version``
  Display Copyright and version, then exit disregarding all other options.

  .. index:: --help


``--help``
  Display usage, then exit disregarding all other options.

  .. index:: -P file


``-P file``
  Indicates the name of the project file that describes the set of sources
  to be processed. The exact set of argument sources depends on other options
  specified, see below.

  .. index:: -U


``-U``
  If a project file is specified and no argument source is provided,
  process all units of the closure of the argument project.
  If explicit argument sources are specified directly through the command-line
  alongside the ``-U`` flag, they will be considered as closure roots for
  source fetching and not as explicit source files.
  If explicit argument sources are passed through `-files`, this option has no
  effect.

  .. index:: --no-subprojects


``--no-subprojects``
  If a project file is specified and no argument source is explicitly
  specified (either directly or by means of ``-files`` option), process
  all the units of the root  argument project. Otherwise this option
  has no effect.

  .. index:: -Xname=value


``-Xname=value``
  Indicates that external variable `name` in the argument project
  has the `value` value. Has no effect if no project is specified as
  tool argument.

  .. index:: --subdirs=dir

``--subdirs=dir``
  Use the specified subdirectory of the project objects file (or of the
  project file directory if the project does not specify an object directory)
  for tool output files. Has no effect if no project is specified as
  tool argument.

  .. index:: --no_objects_dir

``--no_objects_dir``
  Put gnatcheck output files in the current directory instead of using the
  project file's object directory.

  .. index:: -eL

``-eL``
  Follow all symbolic links when processing project files. By default,
  symbolic links are not resolved and kept as is. In some cases, resolving
  the target of symbolic links is needed for proper loading of project files.

  .. index:: --ignore-project-switches

``--ignore-project-switches``
  Ignore gnatcheck switches specified in the package ``Check`` of the main
  project file.

  .. index:: --target

``--target=targetname``
  Specify a target for cross platforms, this is needed to locate the proper
  runtime library.

  .. index:: --RTS

``--RTS=rts-path``
  Specifies the default location of the runtime library.

  .. index:: -h

``-h``
  List all the rules checked by the given ``gnatcheck`` version.

  .. index:: -j

``-j``\ nnnn
  Use *nnnn* processes to analyze the source files.
  On a multi-core machine, this speeds up processing by analyzing subset of
  files separately under multiple processes running in parallel. If ``n`` is 0,
  then the maximum number processes is the number of core processors detected
  on the platform.

  .. attention::

    Please read the :ref:`Performance_and_Memory` section before using this
    flag.

  .. index:: -l

``-l``
  Use full source locations references in the report file.

  .. index:: -log

``-log``
  Duplicate all the output sent to :file:`stderr` into a log file. The log file
  is named :file:`gnatcheck.log`. If a project file is specified as
  ``gnatcheck``
  parameter then it is located in the project objects directory (or in the
  project file directory if no object directory is specified). Otherwise
  it is located in the current directory.

  .. index:: -m

``-m``\ nnnn
  Maximum number of diagnostics to be sent to :file:`stdout`, where *nnnn* is in
  the range 0...1000;
  the default value is 0, which means that there is no limitation on the number of
  diagnostic messages to be output.

  .. index:: -q

``-q``
  Quiet mode. All the diagnostics about rule violations are placed in the
  ``gnatcheck`` report file only, without duplication on :file:`stdout`.

  .. index:: -s

``-s``
  Short format of the report file (no version information, no list of applied
  rules, no list of checked sources is included)

  .. index:: -xml

``-xml``
  Generate the report file in XML format.

  .. index:: -nt

``-nt``
  Do not generate the report file in text format. Enforces  ``-xml``.

  .. index:: -files

``-files=filename``
  Take the argument source files from the specified file. This file should be an
  ordinary text file containing file names separated by spaces or
  line breaks. This switch can be specified only once, but can be combined with
  an explicit list of files. If you want to specify a source file with
  spaces, you need to surround it with double quotes (``"``). If a line in the file
  starts with ``--`` then the whole line is ignored (considered as a comment).

  .. index:: --ignore

``--ignore=filename``
  Do not process the sources listed in a specified file, using the same syntax as
  for the ``-files`` switch.

  .. index:: --rule-file

``--rule-file=filename``
  Load the given file as an LKQL rule options file (see :ref:`LKQL_options_file`
  for more information). If not absolute, the provided path is relative to the
  current working directory.

  .. index:: -r, --rule

``-r, --rule [rule_name]``
  Enable the given ``rule_name`` for the current GNATcheck run; you can pass
  this option multiple times to enable more that one rule. Note that you can
  enable all rules by passing "all" as ``rule_name``.
  You cannot provide parameters to a rule through this command-line option,
  to do so, please use an :ref:`LKQL_options_file`.

  .. index:: --show-rule

``--show-rule``
  Add the corresponding rule name to the diagnosis generated for its
  violation.  If the rule has a user-defined synonym, both gnatcheck and
  user-defined rule names are used as rule annotation:
  ``[user_synonym|gnatcheck_rule_name]``.

  .. index:: --show-instantiation-chain

``--show-instantiation-chain``
  For reported generic instantiation constructs, display a chain of source
  location going from the generic unit to the instantiation.

  .. index:: --brief

``--brief``
  Brief mode, report detections to Stderr. This switch also implies ``-q``
  in terms of verbosity, and ``-s``.

  .. index:: --check-redefinition

``--check-redefinition``
  For a parametrized rule check if a rule parameter is defined more than once
  in the set of rule options specified and issue a warning if parameter redefinition
  is detected

  .. index:: --check-semantic

``--check-semantic``
  Check semantic validity of the source files by running gprbuild with
  the ``-gnatc`` switch, and report any legality error as part of the
  GNATcheck messages. By default, GNATcheck does not check that sources
  are semantically valid and will perform a best effort when encountering
  invalid source files. If you want to ensure and detect that your source
  files are valid as part of running GNATcheck, you should use this switch.

  .. index:: --charset

``--charset=charset``
  Specify the charset of the source files. By default, ``ISO-8859-1`` is
  used if no charset is specified.

  .. index:: --rules-dir

``--rules-dir=dir``
  Specify an alternate directory containing rule files.
  You can specify this switch multiple times. Each of the directories
  specified will be scanned and all files with the extension :file:`.lkql`
  will be loaded by ``GNATcheck`` to provide additional rules.

  .. index:: --include-file=file

``--include-file=file``
  Append the content of the specified text file to the report file

  .. index:: --emit-lkql-rule-file

``--emit-lkql-rule-file``
  Emit a file named ``rules.lkql`` containing the rule configuration of the
  current GNATcheck run. This file is emitted besides the given project file
  if there is one, otherwise, it is generated in the current directory.
  Be careful, if a ``rules.lkql`` file already exists, there will be an error.

  .. index:: -t

``-t``
  Print out execution time.

  .. index:: -v

``-v``
  Verbose mode; ``gnatcheck`` generates version information and then
  a trace of sources being processed.

  .. index:: -W

``-W, --warnings-as-errors``
  Treat warnings raised by GNATcheck as errors, ensuring an erroneous return
  code.

  .. index:: -o

``-o report_file``
  Set name of the text report file to `report_file`.

  .. index:: -ox

``-ox report_file``
  Set name of the XML report file to `report_file`. Enforces  ``-xml``.

  .. index:: -rules

``-rules rules_options``
  Provide rule options for the current GNATcheck run through the command-line.
  All switches and options provided after this flag will be parse as
  :ref:`rule options<gnatcheck_Rule_Options>`.

  .. attention::

    This CLI section is **deprecated**, consider converting your rule
    configuration to the new :ref:`LKQL rule file<LKQL_options_file>` format
    using the ``--emit-lkql-rule-file`` switch.

If a project file is specified and no argument source is explicitly
specified (either directly or by means of ``-files`` option), and no
``-U`` or ``--no-subprojects`` is specified, then the set of processed
sources is determined in the following way.
If root project file has attribute ``Main`` declared and all specified
mains are Ada sources, then combined closure of those mains is processed.
if root project does not have attribute ``Main`` declared, or at least
one of the mains is not an Ada source, then all sources of non-externally
built projects in the project hierarchy are processed.

If the argument project file is an aggregate project, and it aggregates
more than one (non-aggregate) project, gnatcheck runs separately for each
(non-aggregate) project being aggregated by the argument project, and a
separate report file is created for each of these runs. Also such a run
creates an umbrella report file that lists all the (non-aggregate)
projects that are processed separately and for each of these projects
contains the reference for the corresponding report file.

If the argument project file defines an aggregate project that aggregates only
one (non-aggregate) project, the gnatcheck behavior is the same as for the
case of non-aggregate argument project file.


.. _Check_GPR_Package:

The *Check* GPR Package
=======================

In addition to the command-line options, you can use attributes offered by the
``Check`` package to configure a GNATcheck run. In order to do this you may add
the ``Check`` package in the GPR file you're providing to GNATcheck through the
``-P`` command line options, example given:

.. code-block:: ada

  project My_Project is
     package Check is
        ...
     end Check;
  end My_Project;

Inside this package you can define the following attributes to configure
GNATcheck:

``Rules``
  Value is a list of rules to enable when invoking ``gnatcheck`` on this
  project. Values provided in this attribute behave as the ones provided with
  the ``--rule`` switch.

  If the ``--rule`` switch is set when calling ``gnatcheck`` on a project file
  defining this attribute, then, values are concatenated.

``Rule_File``
  Value is a path to a LKQL rule file. If not absolute, the path is relative
  to the project file that defines this attribute.
  See :ref:`LKQL_options_file` for more information.

  If the ``--rule-file`` switch is set when calling ``gnatcheck`` on a project
  file defining this attribute, then, an error is emitted and ``gnatcheck``
  will exit with an error code.

``Switches``
  Index is a language name. Value is a list of additional switches to be used
  when invoking ``gnatcheck``.

  If a switch is provided in both command-line and ``Switches`` attribute,
  then, the value provided through the command-line is used.

  .. attention::

    There are several command-line switches that you cannot pass through the
    ``Switches`` attribute:

    * ``--version``
    * ``--help``
    * ``-P``
    * ``-U``
    * ``-Xname=value``
    * ``-eL``
    * ``-r, --rule [rule_name]`` (use ``Rules`` attribute instead)
    * ``--rule-file=filename`` (use ``Rule_File`` attribute instead)
    * ``--target`` (use the ``Target`` GPR attribute instead)
    * ``--RTS`` (use the ``Runtime`` GPR attribute instead)

    If you're providing one of those switches through the ``Switches`` or the
    ``Default_Switches`` attribute, GNATcheck will emit an error message and
    exit with an error code.

``Default_Switches``
  Same as ``Switches``, but provided additional switches will apply only if
  there is no applicable ``Switches`` attribute.


.. _Source_Preprocessing:

Sources pre-processing
======================

GNATcheck is handling Ada sources pre-processing, meaning that sources lines
that are "excluded" by the Ada pre-processor are also ignored during the
GNATcheck analysis. For example, given the following source:

.. code-block:: ada

  procedure Main is
  begin
     # if Foo = "Bar" then
     goto lbl;
     # else
     null;
     # end if;
  end Main;

Running GNATcheck with the ``Goto_Statements`` rule enabled on this Ada code
will flag the ``goto lbl;`` if, and only if, the preprocessor symbol ``Foo``
is set to ``"Bar"``.

To configure pre-processing, you can use the following GPR attributes:

* ``Builder.Global_Compilation_Switches``
* ``Builder.Default_Switches``
* ``Builder.Switches``
* ``Compiler.Default_Switches``
* ``Compiler.Switches``

.. attention::

  There is a limitation to the GNATcheck's pre-processing handling regarding
  conditioned ``with`` clauses. Meaning that no matter how symbols are defined,
  all ``with`` clauses are going to be analyzed and used by GNATcheck to
  resolve the closure of files to analyze.


.. _LKQL_options_file:

LKQL Rule Files
===============

You can configure GNATcheck rules using an LKQL file, provided through the
``--rule-file`` command-line option or implicitly fetched by GNATcheck (as
described in the following paragraph).

By default, GNATcheck will look for a ``rules.lkql`` file besides the specified
project file if any. If one is found and no other rule configuration has been
provided (either through the LKQL --rule-file option, or by the now deprecated
legacy -rules options), GNATcheck will load the rule configuration file as if
it was provided by the --rule-file option.

.. note::

  You can use the ``--emit-lkql-rule-file`` CLI switch to generate an LKQL rule
  file from a legacy rule configuration provided by the ``-rules`` section.

An LKQL rule file can be any valid LKQL file, the only requirement is that it
must export a ``rules`` top-level symbol. This symbol defines an object value
containing rules configuration; keys are GNATcheck rules to enable; and values
are objects containing the rule parameters. A rule parameter value can be of
the boolean, the integer, the string, or the list of strings type, as shown in
the simple example below:

::

  val rules = @{
    Goto_Statements,
    Forbidden_Attributes: {Forbidden: ["GNAT"], Allowed: ["First", "Last"]}
  }

Please read the :ref:`Predefined_Rules` documentation to view examples on how
to provide parameters to rules through LKQL rule files.

.. attention::

  You cannot provide the same key twice; thus, the following code will result
  in a GNATcheck error.

  ::

    val rules = @{
      Forbidden_Attributes,
      Forbidden_Attributes: {Forbidden: ["GNAT"], Allowed: ["First", "Last"]}
    }

  If you want to create multiple instances of the same rule, you can associate
  a list value to the rule name in the rule configuration object. Elements of
  this list must be parameter objects containing an additional
  ``instance_name`` parameter defining the name of the instance described by
  the enclosing object. If none is provided, the instance is named after the
  rule it is instantiated from, as shown in the following example:

  ::

    val rules = @{
      Goto_Statements,
      Forbidden_Attributes: [
        # "Forbidden_Attributes" instance of the "Forbidden_Attributes" rule, checking for 'First and 'Last
        {Forbidden: ["First", "Last"]},

        # "Length_Attr" instance of the "Forbidden_Attributes" rule, checking for 'Length
        {Forbidden: ["Length"], instance_name: "Length_Attr"}
      ]
    }

  Moreover, each instance must be identifiable through a unique name, thus the
  following configuration is invalid and will lead to a GNATCheck error:

  ::

    val rules = @{
      Forbidden_Attributes: [
        {Forbidden: ["First", "Last"], instance_name: "Instance"},
        {Forbidden: ["Length"], instance_name: "Instance"},
      ]
    }

Additionally to the ``rules`` top-level symbol, an LKQL rule file may export
``ada_rules`` and ``spark_rules`` symbols to enable associated rules,
respectively, only on Ada code or only on SPARK code. Those symbols must also
refer to an object value formatted like the ``rules`` value.

::

  # Rules to run on both Ada and SPARK code
  val rules = @{
    Goto_Statements
  }

  # Rules to run only on Ada code
  val ada_rules = @{
    Forbidden_Attributes: {Forbidden: ["GNAT"]}
  }

  # Rules to run only on SPARK code
  val spark_rules = @{
    Ada_2022_In_Ghost_Code
  }

Please note that compiler based rules (:ref:`Warnings`, :ref:`Restrictions` and
:ref:`Style_Checks`) cannot be restricted to Ada or SPARK code. Consequently,
the following configuration will raise an error:

::

  val spark_rules = @{
    Warnings: {Arg: "a"}
  }

.. attention::

  Instance uniqueness must also be respected between all rule sets, meaning
  that such config is invalid:

  ::

    val rules = @{
      # Clashing with "Goto_Statement" in ada_rules
      Goto_Statements,

      # Clashing with "Forbid_Attr" instance in spark_rules
      Forbidden_Attributes: {Forbidden: ["GNAT"], instance_name: "Forbid_Attr"}
    }

    val ada_rules = @{
      Goto_Statements
    }

    val spark_rules = @{
      Forbidden_Attributes: {Forbidden: ["Length"], instance_name: "Forbid_Attr"}
    }


.. _gnatcheck_Rule_Options:

GNATcheck Rule Options
======================

.. attention::

  Rules options are **deprecated**, consider converting your rule
  configuration to the new :ref:`LKQL rule file<LKQL_options_file>` format
  using the ``--emit-lkql-rule-file`` CLI switch.

The following options control the processing performed by ``gnatcheck``. You
can provide as many rule options as you want after the ``-rules`` switch.

  .. index:: +R (gnatcheck)

``+R[:instance_name:]rule_id[:param{,param}]``
  Create and enable an instance of the specified rule with the specified
  parameter(s), if any.
  `rule_id` must be the identifier of one of the currently implemented
  rules (use ``-h`` for the list of implemented rules). Rule identifiers
  are not case-sensitive.

  Each `param` item must be a non-empty string representing a valid parameter
  for the specified rule. If the part of the rule option that follows the
  colon character contains any space characters then this part must be enclosed
  in quotation marks.

  `instance_name` is a user-defined name for the created rule instance. If this
  is not specified, the instance name is set to the rule name (normalized to
  lower case).
  You can create as much instances as you want for a single rule, as long
  as they have distinct names (names aren't case sensitive either). If an
  instance of the same rule with the same name already exists GNATcheck will
  raise an error.

  For example:

  .. code-block:: ada

    --  Create and enable an instance of "Goto_Statements" named
    --  "goto_statements".
    +RGoto_Statements

    --  Create and enable an second instance of "Goto_Statements" named
    --  "custom_name".
    +R:custom_name:Goto_Statements

    --  Create and enable an instance of "Recursive_Subprograms" named
    --  "other_name".
    +R:other_name:Recursive_Subprograms

    --  This will cause a GNATcheck error because the "goto_statement" instance
    --  already exists.
    +RGoto_Statements

  This feature can be used to map ``gnatcheck`` rules onto a user's coding
  standard.

  .. index:: -R (gnatcheck)


``-R[:instance_name:]rule_id``
  Remove the designated rule instance, disabling it at the same time.

  .. note::

    By removing a rule instance, all previously given instance parameter(s)
    are cleared from the GNATcheck memory.

  .. attention::

    No parameters are allowed for the ``-R`` rule option. Since rule instances
    are immutable, you cannot modify a parameter set once the instance has been
    created by a ``+R`` option.

  .. index:: -from (gnatcheck)


``-from=rule_option_filename``
  Read the rule options from the text file `rule_option_filename`, referred
  to as a 'coding standard file' below.

The default behavior is that all the rule checks are disabled.

If a rule option is given in a rule file, it can contain spaces and line breaks.
Otherwise there should be no spaces between the components of a rule option.

If more than one rule option is specified for the same rule, with the same
instance name, GNATcheck will raise an error and stop its execution.

.. attention::

  Unlike in older versions of GNATcheck, rule instances aren't mutable, so
  you cannot change options for an instance after its instantiation.

A coding standard file is a text file that contains a set of rule options
described above.

.. index:: Coding standard file (for gnatcheck)

The file may contain empty lines and Ada-style comments (comment
lines and end-of-line comments). There can be several rule options on a
single line (separated by a space).

A coding standard file may reference other coding standard files by including
more ``-from=rule_option_filename``
options, each such option being replaced with the content of the
corresponding coding standard file during processing. In case a
cycle is detected (that is, :file:`rule_file_1` reads rule options
from :file:`rule_file_2`, and :file:`rule_file_2` reads
(directly or indirectly) rule options from :file:`rule_file_1`),
processing fails with an error message.

If the name of the coding standard file does not contain a path information in
absolute form, then it is treated as being relative to the current directory if
gnatcheck is called without a project file or as being relative to the project
file directory if gnatcheck is called with a project file as an argument.

.. _Mapping_gnatcheck_Rules_Onto_Coding_Standards:

Mapping GNATcheck Rules Onto Coding Standards
=============================================

If you want to use ``GNATcheck`` to check if your code
follows a given coding standard, you can use the following approach
to simplify mapping your coding standard requirements onto
``GNATcheck`` rules:

* when specifying rule configuration, use instance names that are relevant
  to your coding standard::

    val rules = @{
      Gnatcheck_Rule_1: {instance_name: "My_Coding_Rule_1", param1: "value"},
      ...
      Gnatcheck_Rule_N: {instance_name: "My_Coding_Rule_N"}
    }

  or with the deprecated rule options::

    +R:My_Coding_Rule_1:Gnatcheck_Rule_1:param1
    ...
    +R:My_Coding_Rule_N:Gnatcheck_Rule_N

* call ``gnatcheck`` with the ``--show-rule`` flag that adds the rule names
  the generated diagnoses. If a instance name is defined in the rule configuration,
  then this name will be used to annotate the diagnosis of the rule name::

    foo.adb:2:28: something is wrong here [My_Coding_Rule_1|Gnatcheck_Rule_1]
    ...
    bar.ads:17:3: this is not good [My_Coding_Rule_N|Gnatcheck_Rule_N]

.. attention::

  A custom coding rule name can be any sequence of non-whitespace characters.
  Moreover, the ":" (colon) character is forbidden in those names for parsing
  purposes.

.. _gnatcheck_Exit_Codes:

GNATcheck Exit Codes
====================

.. index:: exit code

``gnatcheck`` returns the following exit codes at the end of its run:

* ``0``: No tool failure, no missing argument source and no rule
  violation was detected.

* ``1``: No tool failure, no missing argument source and at least
  one rule violation was detected.

* ``2``: A tool failure was detected (in this case the results
  of the gnatcheck run cannot be trusted).

* ``3``: No tool failure, no problem with rule specification, but
  there is at least one missing argument source.

* ``4``: Provided rule configuration file doesn't exist.

* ``5``: The name of an unknown rule in a rule option or some problem with
  rule parameters.

* ``6``: Any other problem with specifying the rules to check.

If the exit code corresponds to some problem with defining the rules to check then
the result of the gnatcheck run cannot be fully trusted because the set of rules that
has been actually used may be different from user intent.

If gnatcheck is called with the ``--brief`` option, it will return the exit code
``0`` instead of ``1`` when some violation is detected (and no tool failure).

.. _Format_of_the_Report_File:

Format of the Report File
=========================

.. index:: Format of the Report File

The ``gnatcheck`` tool outputs on :file:`stderr` all messages concerning
rule violations except if running in quiet mode.  By default it also creates a
text file that contains the complete report of the last gnatcheck run, this file
is named :file:`gnatcheck.out`. A user can specify generation of
the XML version of the report file (its default name is :file:`gnatcheck.xml`)
If ``gnatcheck`` is called with a project
file, the report file is located in the object directory defined by the project
file (or in the directory where the argument project file is located if no
object directory is defined), if ``--subdirs`` option is specified, the
file is placed in the subdirectory of this directory specified by this option.
Otherwise it is located in the
current directory; the ``-o`` or ``-ox`` option can be used to
change the name and/or location of the text or XML report file.
This text report contains:


* general details of the ``gnatcheck`` run: date and time of the run,
  the version of the tool that has generated this report, full parameters
  of the  ``gnatcheck`` invocation, reference to the list of checked
  sources and applied rules (coding standard);
* summary of the run (number of checked sources and detected violations);
* list of exempted coding standard violations;
* list of non-exempted coding standard violations;
* list of problems in the definition of exemption sections;
* list of language violations (compile-time errors) detected in processed sources;

The references to the list of checked sources and applied rules are
references to the text files that contain the corresponding information.
These files could be either files supplied as ``gnatcheck`` parameters or
files created by ``gnatcheck``; in the latter case
these files are located in the same directory as the report file.

The content of the XML report is similar to the text report except that
it explores the set of files processed by gnatcheck and the coding standard
used for checking these files.

.. _Rule_exemption:

Rule Exemption
==============

.. index:: Rule exemption

One of the most useful applications of ``gnatcheck`` is to
automate the enforcement of project-specific coding standards,
for example in safety-critical systems where particular features
must be restricted in order to simplify the certification effort.
However, it may sometimes be appropriate to violate a coding standard rule,
and in such cases the rationale for the violation should be provided
in the source program itself so that the individuals
reviewing or maintaining the program can immediately understand the intent.

The ``gnatcheck`` tool supports this practice with the notion of
a 'rule exemption' covering a specific source code section. Normally
rule violation messages are issued both on :file:`stderr`
and in a report file. In contrast, exempted violations are not listed on
:file:`stderr`; thus users invoking ``gnatcheck`` interactively
(e.g. in its GNAT Studio interface) do not need to pay attention to known and
justified violations. However, exempted violations along with their
justification are documented in a special section of the report file that
``gnatcheck`` generates.

.. _Using_pragma_Annotate_to_Control_Rule_Exemption:

Using pragma ``Annotate`` to control rule and instance exemption
----------------------------------------------------------------

.. index:: using pragma Annotate to control rule and instance exemption

Rule and instance exemption is controlled by pragma ``Annotate`` when its first
argument is 'gnatcheck'. The syntax of ``gnatcheck``'s exemption control
annotations is as follows:

::

  <pragma_exemption>  ::= pragma Annotate (gnatcheck, <exemption_control>, <exempted_name> [, <justification>]);

  <exemption_control> ::= Exempt_On | Exempt_Off

  <exempted_name>     ::= <string_literal>

  <justification>     ::= <expression>

An expression used as an exemption justification should be a static string
expression. A string literal is enough in most cases, but you may want to use
concatenation of string literals if you need a long message but you have to
follow line length limitation.

When a ``gnatcheck`` annotation has more than four arguments, ``gnatcheck``
issues a warning and ignores the additional arguments.  If the arguments do not
follow the syntax above, ``gnatcheck`` emits a warning and ignores the
annotation.

The ``exempted_name`` argument should be the name of some existing ``gnatcheck``
rule, or the name of a rule instance.  Otherwise a warning message is generated
and the pragma is ignored. If ``exempted_name`` doesn't denote an activated rule
or a valid instance in the given ``gnatcheck`` call, the pragma is ignored and
no warning is issued. The exception from this rule is that exemption sections
for ``Warnings`` rule are fully processed when ``Restrictions`` rule is
activated.

.. attention::

  Please not that for now it isn't possible to provide an exempted name which
  designates an instance of a compiler-based rule (:ref:`Warnings`,
  :ref:`Style_Checks` and :ref:`Restrictions`) with a custom name.

A source code section where an exemption is active for a given rule is
delimited by an ``exempt_on`` and ``exempt_off`` annotation pair:

.. code-block:: ada

  pragma Annotate (gnatcheck, Exempt_On, "Rule_Name", "justification");
  -- source code section
  pragma Annotate (gnatcheck, Exempt_Off, "Rule_Name");

Using such annotations will exempt all violations of the rule designated by
``Rule_Name`` inside the exempted source section. But you can also provide the
name of a rule instance to only exempt violations raised by this instance.

For some rules it is possible specify rule parameter(s) when defining an
exemption section for a rule or an instance of it. This means that only the
checks corresponding to the given rule parameter(s) are exempted in this
section:

.. code-block:: ada

  pragma Annotate (gnatcheck, Exempt_On, "Rule_Name: Par1, Par2", "justification");
  -- source code section
  pragma Annotate (gnatcheck, Exempt_Off, "Rule_Name: Par1, Par2");

A parametric exemption section can be defined for a rule if a rule has
parameters and these parameters change the scope of the checks performed by a
rule. For example, if you define an exemption section for 'Restriction' rule
with the parameter 'No_Allocators', then in this section only the checks for
``No_Allocators`` will be exempted, and the checks for all the other
restrictions from your coding standard will be performed as usual.

See the description of individual rules to check if parametric exemptions
are available for them and what is the format of the rule parameters to
be used in the corresponding parameters of the ``Annotate`` pragmas.

If a rule has a parameter, but its documentation does not explicitly say that
the parameter can be used when defining exemption sections for the rule,
this means that the parametric exemption cannot be used for this rule.

You may also use pragma ``GNAT_Annotate`` instead of pragma ``Annotate``, this
pragma has exactly the same format. This may be needed if you are using an old
version of the GNAT compiler that does not support the format of
pragma ``Annotate`` given above. Old GNAT versions may issue warning about
unknown pragma when compiling a source that contains pragma ``GNAT_Annotate``.

.. _gnatcheck_Annotations_Rules:

GNATcheck Annotations Rules
---------------------------

.. index:: gnatcheck annotations rules

* An ``Exempt_Off`` annotation can only appear after a corresponding
  'Exempt_On' annotation.

* An ``Exempt_On`` annotation should have a justification. Conversely, an
  ``Exempt_Off`` annotation should *not* have a justification.

* Exempted source code sections are only based on the source location of the
  annotations. Any source construct between the two
  annotations is part of the exempted source code section.

* Exempted source code sections for different rules are independent. They can
  be nested or intersect with one another without limitation.
  Creating nested or intersecting source code sections for the same rule is
  not allowed.

* A matching 'Exempt_Off' annotation pragma for an 'Exempt_On' pragma
  that defines a parametric exemption section is the pragma that contains
  exactly the same set of rule parameters for the same exempted name.

* Parametric exemption sections for the same rule with different parameters
  can intersect or overlap in case if the parameter sets for such sections
  have an empty intersection.

* Malformed exempted source code sections are reported by a warning, and
  the corresponding rule exemptions are ignored.

* When an exempted source code section does not contain at least one violation
  of the exempted name, a warning is emitted on :file:`stderr`.

* If an 'Exempt_On' annotation pragma does not have a matching
  'Exempt_Off' annotation pragma in the same compilation unit, a warning is
  issued and the exemption section is considered to last until the
  end of the compilation unit source.


.. _using_comments_to_control_rule_exemption:

Using comments to control rule and instance exemption
-----------------------------------------------------

.. index:: using comments to control rule and instance exemption

As an alternative to the ``pragma Annotate`` syntax, it is also possible to use
a syntax based on comments, with the following syntax:

::

  <comment_exemption> ::= --## rule (on | off) <exempted_name> [## <exemption_justification>]

Here is an example:

.. code-block:: ada

    --## rule off implicit_in ## Exemption justification
    procedure Bar (A : Integer);
    --## rule on implicit_in

.. attention:: Please note that a comment starting with ``--##`` but not
   respecting the above syntax will not trigger a warning, in order to not emit
   false positives.
   Also note that in its current iteration, this syntax does not support passing
   parameters to rule names

The rules mentioned in :ref:`gnatcheck_Annotations_Rules` are relaxed, in
particular:

* Justifications are not checked and are optional;
* Anything between the exempted name and ``##`` will be ignored;
* Rules regarding parametric exemption do not apply, as per the notice above.

The ``rule on`` marker corresponds to ``Exempt_Off`` and ``rule off`` corresponds
to ``Exempt_On``. Apart from that, you can expect those rule exemptions to work
in a similar fashion as the ones described above.


In addition, a shorthand syntax is available to exempt a rule just for one line::

    <line_comment_exemption> ::= --## rule line off <exempted_name> [## <rule_justification>]

For instance, from the previous example:

.. code-block:: ada

    procedure Bar (A : Integer); --## rule line off implicit_in ## Exemption justification

This will exempt the given rule or instance only for the line on which this
comment is placed, and automatically turn it back on on the next line.

.. _Using_GNATcheck_as_a_KP_Detector:

Using GNATcheck as a Known Problem Detector
===========================================

If you are a GNAT Pro Assurance customer, you have access to a special
packaging of GNATcheck called ``gnatkp`` (GNAT Known Problem detector)
where the ``gnatcheck`` executable is replaced by ``gnatkp``, and the
coding standard rules are replaced by rules designed to detect constructs
affected by known problems in official compiler releases. Note that GNATkp
comes in addition and not as a replacement of GNATcheck.

You can use the command ``gnatkp --help`` to list all the switches
relevant to GNATkp. GNATkp mostly accepts the same command arguments as
GNATcheck and behaves in a similar way, but there are some differences that
are described below.

The easiest way to use GNATkp is by specifying the version of GNAT Pro that
you have and letting ``gnatkp`` run all known problem detectors
registered for this version, via the switch ``--kp-version``. For example:

.. code-block:: none

  gnatkp -Pproject --kp-version=21.2 --target=<my_target> --RTS=<my_runtime>

will run all detectors relevant to GNAT Pro 21.2 on all files in the
project. The list of detectors will be displayed as info messages, and will
also be listed in the file :file:`gnatkp-rule-list.out`. The list of detected
source locations will be generated on standard error, as well as in a file
called :file:`gnatkp.out`.

You can display the list of detectors without running them by specifying
additionally the ``-h`` switch, e.g.:

.. code-block:: none

  gnatkp --kp-version=21.2 -h --target=<my_target> --RTS=<my_runtime>

You can also combine the ``--kp-version`` switch with the ``--target`` switch
to filter out detectors not relevant for your target, e.g:

.. code-block:: none

  gnatkp -Pproject --kp-version=21.2 --target=powerpc-elf --RTS=<my_runtime>

will only enable detectors relevant to GNAT Pro 21.2 and to the ``powerpc-elf``
target.

Note that you need to have the corresponding target GNAT compiler installed
to use this option. By default, detectors for all targets are enabled.

It is also possible to specify the custom list of detectors for GNATkp to run
using the switch ``-r``:

.. code-block:: none

  gnatkp -Pproject --target=<my_target> --RTS=<my_runtime> -r kp_xxxx_xxx [-r kp_xxxx_xxx]

where ``kp_xxxx_xxx`` is the name of a relevant known-problem to detect. You
can get the list of available detectors via the command ``gnatkp -h``. When
combined with the ``--kp-version`` and possibly ``--target`` switches,
``gnatkp -h`` will only list the detectors relevant to the version
(and target) specified.

.. attention::

  You must provide explicit target and runtime (either through the command-line
  or with a provided project file) when running GNATkp to ensure the result
  soundness.

.. note::

  The exemption mechanism is available for GNATkp as well but you have to
  change pragmas and comments a bit to avoid conflict with GNATcheck
  exemptions. Thus, pragmas annotations' first argument must be ``gnatkp``
  instead of ``gnatcheck``:

  .. code-block:: ada

    pragma Annotate (gnatkp, Exempt_On, "kp_19198", "Justification");

  And exemption comments' first word must be ``kp`` instead of ``rule``,
  example:

  .. code-block:: ada

    --## kp off kp_19198 ## Justification

You can check via the GNAT Tracker interface which known problems are
relevant to your version of GNAT and your target before deciding which
known problems may impact you: most known problems are only relevant to a
specific version of GNAT, a specific target, or a specific usage profile. Do
not hesitate to contact the AdaCore support if you need help identifying the
entries that may be relevant to you.

.. _Performance_and_Memory:

Performance and Memory Usage
============================

GNATcheck performances are closely related to rules you're enabling and to the
size of the codebase you're running it on, and sometimes it can take a lot of
time to perform all checks.
You can use the ``-j`` switch to run GNATcheck in multi-core mode and decrease
the checking time. However, you have to be careful about memory usage when
running GNATcheck with this mode enabled:

You should count around 3.5 GB of available memory per million source code
lines, per process. Meaning that for a project with ``l`` source code lines, if
you run GNATcheck while providing ``n`` as parameter of the ``-j`` switch, you
will need ``(l / 1,000,000) * 3.5 * n`` GB of available memory (this formula
isn't valid if ``n = 0``).

.. attention::

  Out-of-memory errors are hard to debug and can lead to system freezes, invalid
  results, or non-deterministic behavior. Thus, make sure you have enough memory
  before running GNATcheck.

.. _Transition_from_ASIS-based_GNATcheck:

Transition from ASIS-based GNATcheck
====================================

Originally ``gnatcheck`` was implemented on top of the ASIS technology and
starting with version 23, it was re-implemented on top of the libadalang
technology. This new implementation has kept most of the old gnatcheck interface
and functionality, so transition from the old ``gnatcheck`` to the current
version should be smooth and transparent, except possibly for a few aspects to
be taken into account by users of the old technology.

.. _Switches_No_Longer_Supported:

Switches No Longer Supported
----------------------------

.. index:: old unsupported switches

The following switches from the old ``gnatcheck`` are no longer supported:

``-a``
  In order to process GNAT Run-Time library units, you need to explicitly
  include them in a project file.

``--incremental``
  GNATcheck no longer makes the distinction between "local" and "global"
  rules, so this switch is no longer supported. You can use the ``-j``
  switch instead which provides a significant speed up compared to the old
  version.

``--write-rules=template_file``
  This switch is no longer supported. You can use the GNAT Studio rule editor
  instead to create a coding standard file.

.. _New_Instance_System:

The new rule instance system
----------------------------

The new ``gnatcheck`` implementation is introducing a new rule instance system
which allows you to instantiate a rule multiple times under different names,
and with potentially different rule parameters.
You can now define more that one "alias" for the same rule to map your coding
standard on the ``gnatcheck`` rules.
However, rules aren't mutable anymore, which means that you cannot modify
parameters of a rule once it has been created (instantiated).
For example:

.. code-block:: ada

  --  The rule "Goto_Statements" is instantiated here
  +RGoto_Statements

  --  We try to create a new instance of the "Goto_Statements", this will fail
  +RGoto_Statements:Only_Unconditional

While with the old system this rule file would just mutate the previously
enabled "Goto_Statements" rule, with the new instance system, this will cause
an error during the ``gnatcheck`` run, telling you that the "goto_statement"
instance already exists.
To correct this error, you have define a custom name for the second
"Goto_Statements" instance:

.. code-block:: ada

  --  The rule "Goto_Statements" is instantiated here
  +RGoto_Statements

  --  The rule "Goto_Statements" is also instantiated here,
  --  under the "Uncond_Goto" name.
  +R:Uncond_Goto:Goto_Statements:Only_Unconditional

The same way, you have to rewrite rule options such as:

.. code-block:: ada

  +RForbidden_Pragmas:GNAT
  +RForbidden_Pragmas:Annotate
  +RForbidden_Pragmas:Assert

into a single rule option using the comma separated notation, like:

.. code-block:: ada

  +RForbidden_Pragmas:GNAT,
                      Annotate,
                      Assert

This new instance system also suppress the possibility to disable a rule (or
an instance) with a parameter. Thus, the ``-R`` rule option doesn't accept
parameters anymore.

.. _Rule_Aliases_No_Longer_Supported:

Rule Aliases No Longer Supported
--------------------------------

.. index:: rule aliases no longer supported

Because of historical reasons the old ``gnatcheck`` allowed aliases for
some rules. These aliases are not documented, but there is some possibility that
they could be used in some legacy rule files. ``GNATcheck`` no longer supports
these aliases. Here is the (alphabetically ordered) list of all the
aliases formerly accepted and their replacement:

====================================== ========================================
Old Rule Alias                         Replacement
====================================== ========================================
Abstr_Types                            Abstract_Type_Declarations
Bool_Relation_Ops                      Boolean_Relational_Operators
Contr_Types                            Controlled_Type_Declarations
Control_Structure_Nesting              Overly_Nested_Control_Structures
Decl_Blocks                            Declarations_In_Blocks
Default_Par                            Default_Parameters
Derived_Types                          Non_Tagged_Derived_Types
Discr_Rec                              Discriminated_Records
Explicit_Discrete_Ranges               Explicit_Full_Discrete_Ranges
Functionlike_Procedures                Function_Style_Procedures
Global_Loop_Exit                       Outer_Loop_Exits
Goto                                   GOTO_Statements
Implicit_IN_Parameter_Mode             Implicit_IN_Mode_Parameters
LL_Subpr                               Library_Level_Subprograms
Local_Pckg                             Local_Packages
Misnamed_Identifiers                   Identifier_Suffixes
Missing_Small_For_Fixed_Point_Type     Implicit_SMALL_For_Fixed_Point_Types
Non_Marked_BEGIN_In_Package_Body       Uncommented_BEGIN_In_Package_Bodies
Non_Named_Blocks_And_Loops             Unnamed_Blocks_And_Loops
One_Entry_In_PO                        Multiple_Entries_In_Protected_Definitions
Parameter_Mode_Ordering                Parameters_Out_Of_Order
Positional_Component_Associations      Positional_Components
Positional_Generic_Associations        Positional_Generic_Parameters
Positional_Parameter_Associations      Positional_Parameters
Pragma_Usage                           Forbidden_Pragmas
Predefined_Exceptions                  Raising_Predefined_Exceptions
Proper_Returns                         Improper_Returns
Qualified_Aggr                         Non_Qualified_Aggregates
Restrict_Name_Space                    Name_Clashes
Simple_Loop_Exit_Names                 Expanded_Loop_Exit_Names
SPARK_Attributes                       Non_SPARK_Attributes
Unconstr_Array_Return                  Unconstrained_Array_Returns
Universl_Ranges                        Universal_Ranges
Unreasonable_Places_For_Instantiations Improperly_Located_Instantiations
Use_Pckg_Clauses                       USE_PACKAGE_Clauses
Use_Of_Non_Short_Circuit               Non_Short_Circuit_Operators
Visible_Exceptions                     Raising_External_Exceptions
Volatile_Requires_Addr_Clause          Volatile_Objects_Without_Address_Clauses
====================================== ========================================

.. _New_Defaults_For_Recursive_Subprograms_Rule:

New Defaults For Recursive_Subprograms Rule
-------------------------------------------

.. index:: new defaults for recursive subprograms rule

The ``Recursive_Subprograms`` rule now defaults to skipping dispatching calls
and a new parameter ``Follow_Dispatching_Calls`` is available (the old
``Skip_Dispatching_Calls`` is still accepted for compatibility and is ignored
since it's the default). In addition, implicit calls made via default
object initialization are not taken into account.

.. _Argument_Sources_Legality_And_Project_Files:

Argument Sources Legality And Project Files
-------------------------------------------

.. index:: argument sources legality and project files

The old ``gnatcheck`` compiled its argument sources to create the
so-called ASIS tree files. This had two important consequences: first,
``gnatcheck`` could analyze only legal Ada sources, and second, for each
legal argument source ``gnatcheck`` had full static semantic information.
The situation with the current ``gnatcheck`` is different.

First, ``gnatcheck`` can now analyze Ada sources that are not legal, and it
is trying to do its best to check the rules specified. This may result in
false negatives caused by the absence of necessary semantic information or
by some other problems in the argument source that impede a full check of
some rules. You can use the ``--check-semantic`` option to check if your
Ada sources are legal sources.

Second, if ``gnatcheck`` is called for some Ada source and it does not have a
project file as a parameter, it will see only the information contained
in the sources specified and will not follow the semantic dependencies on other
sources if any. This is why it is strongly recommended to call ``gnatcheck``
with a project file. When called with a project file, ``gnatcheck`` follows
all the semantic dependencies for sources located in the project file source
directories.

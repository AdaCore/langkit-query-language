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
  If a project file is specified and no argument source is explicitly
  specified (either directly or by means of ``-files`` option), process
  all the units of the closure of the argument project. Otherwise this option
  has no effect.

  .. index:: -U main_unit


``-U main_unit``
  If a project file is specified and no argument source is explicitly
  specified (either directly or by means of ``-files`` option), process
  the closure of units rooted at `main_unit`. Otherwise this option
  has no effect. ``main_unit`` should be the name of a source file that contains
  the main unit of closure. Note that this option is currently ignored (it is
  equivalent to ``-U``) and will be implemented in a future version.

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

  .. index:: --simple-project

``--simple-project``
  Simple project set up where only source directories and optionally the
  ``File_Patterns`` attribute in the ``CodePeer`` package are taken into
  account.

  When using this switch, source files are found using a default set of file
  extensions: :file:`.ada`, :file:`.ads`, :file:`.adb`, :file:`.spc`,
  :file:`.bdy`.

  If you want to override these default file extensions, you can add the
  ``File_Patterns`` attribute in the ``CodePeer`` package, which includes a
  list of file patterns where you can specify the following meta characters:

  ================ ==========================================
  \*               matches any string of 0 or more characters
  ?                matches any character
  [list of chars]  matches any character listed
  [char-char]      matches any character in given range
  [^list of chars] matches any character not listed
  ================ ==========================================

  These patterns are case insensitive.

  For example:

  .. code-block:: gpr

    package CodePeer is
       for File_Patterns use ("*.a", "*.ad[asb]", "[a-z][0-9]*.a?");
    end CodePeer;

  specifies that all sources ending with :file:`.a`, :file:`.ada`,
  :file:`.ads`, :file:`.adb` as well as all sources starting with a
  letter, followed by a digit and ending with :file:`.a` and one last
  character will be analyzed.

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
  On a multi-core machine, this speeds up processing by analyzing subset
  of files separately under multiple processes running in parallel.
  If ``n`` is 0, then the maximum number processes is the number of
  core processors detected on the platform.

  .. index:: -l

``-l``
  Use full source locations references in the report file. For a construct from
  a generic instantiation a full source location is a chain from the location
  of this construct in the generic unit to the place where this unit is
  instantiated.

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
  the default value is 500. Zero means that there is no limitation on
  the number of diagnostic messages to be output.

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

  .. index:: --show-rule

``--show-rule``
  Add the corresponding rule name to the diagnosis generated for its
  violation.  If the rule has a user-defined synonym, both gnatcheck and
  user-defined rule names are used as rule annotation:
  ``[user_synonym|gnatcheck_rule_name]``.

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

  .. index:: -t

``-t``
  Print out execution time.

  .. index:: -v

``-v``
  Verbose mode; ``gnatcheck`` generates version information and then
  a trace of sources being processed.

  .. index:: -o

``-o report_file``
  Set name of the text report file to `report_file`.

  .. index:: -ox

``-ox report_file``
  Set name of the XML report file to `report_file`. Enforces  ``-xml``.

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

.. _gnatcheck_Rule_Options:

GNATcheck Rule Options
======================

The following options control the processing performed by ``gnatcheck``.


  .. index:: +R (gnatcheck)

``+R[:rule_synonym:]rule_id[:param{,param}]``
  Turn on the check for a specified rule with the specified parameter(s), if
  any. `rule_id` must be the identifier of one of the currently implemented
  rules (use ``-h`` for the list of implemented rules). Rule identifiers
  are not case-sensitive. Each `param` item must
  be a non-empty string representing a valid parameter for the specified rule.
  If the part of the rule option that follows the colon character contains any
  space characters then this part must be enclosed in quotation marks.

  `rule_synonym` is a user-defined synonym for a rule name, it can be used
  to map ``gnatcheck`` rules onto a user coding standard.

  .. index:: -R (gnatcheck)


``-Rrule_id[:param]``
  Turn off the check for a specified rule with the specified parameter, if any.

  .. index:: -from (gnatcheck)


``-from=rule_option_filename``
  Read the rule options from the text file `rule_option_filename`, referred
  to as a 'coding standard file' below.


The default behavior is that all the rule checks are disabled.

If a rule option is given in a rule file, it can contain spaces and line breaks.
Otherwise there should be no spaces between the components of a rule option.

If more than one rule option
is specified for the same rule, these options are summed together. If a new option contradicts
the rule settings specified by previous options for this rule, the new option overrides
the previous settings.

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

*
   when specifying rule options, use synonyms for the rule names
   that are relevant to your coding standard::

     +R :My_Coding_Rule_1: Gnatcheck_Rule_1: param1
     ...
     +R :My_Coding_Rule_N: Gnatcheck_Rule_N

*
   call ``gnatcheck`` with the ``--show-rule`` option that adds the rule names
   to the generated diagnoses. If a synonym is used in the rule option that
   enables the rule, then this synonym will be used to annotate the diagnosis
   instead of the rule name::

     foo.adb:2:28: something is wrong here [My_Coding_Rule_1]
     ...
     bar.ads:17:3: this is not good [My_Coding_Rule_N]

Note that this approach currently does not work for compiler-based checks
integrated in ``gnatcheck`` (implemented by ``Restrictions``, ``Style_Checks``
and ``Warnings`` rules.

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

* ``4``: Parameter of the rule ``-from`` option denotes a nonexistent file.

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

.. _Using_pragma_``Annotate``_to_Control_Rule_Exemption:

Using pragma ``Annotate`` to Control Rule Exemption
---------------------------------------------------

.. index:: Using pragma Annotate to control rule exemption

Rule exemption is controlled by pragma ``Annotate`` when its first
argument is 'gnatcheck'. The syntax of ``gnatcheck``'s
exemption control annotations is as follows:


::

  <pragma_exemption>  ::= pragma Annotate (gnatcheck, <exemption_control>, <rule_name> [, <justification>]);

  <exemption_control> ::= Exempt_On | Exempt_Off

  <rule_name>         ::= <string_literal>

  <justification>     ::= <expression>

An expression used as an exemption justification should be a static string
expression. A string literal is enough in most cases, but you may want to use
concatenation of string literals if you need a long message but you have to
follow line length limitation.

When a ``gnatcheck`` annotation has more than four arguments, ``gnatcheck``
issues a warning and ignores the additional arguments.  If the arguments do not
follow the syntax above, ``gnatcheck`` emits a warning and ignores the
annotation.

The ``rule_name`` argument should be the name of some existing ``gnatcheck``
rule, or the name of a synonym for a rule.  Otherwise a warning message is
generated and the pragma is ignored. If ``rule_name`` denotes a rule that is
not activated by the given ``gnatcheck`` call, the pragma is ignored and no
warning is issued. The exception from this rule is that exemption sections for
``Warnings`` rule are fully processed when ``Restrictions`` rule is activated.

A source code section where an exemption is active for a given rule is
delimited by an ``exempt_on`` and ``exempt_off`` annotation pair:

.. code-block:: ada

  pragma Annotate (gnatcheck, Exempt_On, "Rule_Name", "justification");
  -- source code section
  pragma Annotate (gnatcheck, Exempt_Off, "Rule_Name");

For some rules it is possible specify rule parameter(s) when defining
an exemption section for a rule. This means that only the checks
corresponding to the given rule parameter(s) are exempted in this section:

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
  exactly the same set of rule parameters for the same rule.

* Parametric exemption sections for the same rule with different parameters
  can intersect or overlap in case if the parameter sets for such sections
  have an empty intersection.

* Malformed exempted source code sections are reported by a warning, and
  the corresponding rule exemptions are ignored.

* When an exempted source code section does not contain at least one violation
  of the exempted rule, a warning is emitted on :file:`stderr`.

* If an 'Exempt_On' annotation pragma does not have a matching
  'Exempt_Off' annotation pragma in the same compilation unit, a warning is
  issued and the exemption section is considered to last until the
  end of the compilation unit source.


.. _using_comments_to_control_rule_exemption:

Using comments to control rule exemption
----------------------------------------

.. index:: using comments to control rule exemption

As an alternative to the ``pragma Annotate`` syntax, it is also possible to use
a syntax based on comments, with the following syntax:

::

  <comment_exemption> ::= --## rule (on | off) <rule_name> [## <rule_justification>]

.. attention:: Please note that a comment starting with ``--##`` but not
   respecting the above syntax will not trigger a warning, in order to not emit
   false positives.

.. attention:: In its current iteration, this syntax does not support passing
   parameters to rule names

The rules mentioned in :ref:`gnatcheck_Annotations_Rules` are relaxed, in
particular:

* Justifications are not checked and are optional
* Rules regarding parametric exemption do not apply, as per the notice above.

Appart from that, you can expect those rule exemptions to work in a similar
fashion as the ones described above.

.. _Using_GNATcheck_as_a_KP_Detector:

Using GNATcheck as a Known Problem Detector
===========================================

If you are a GNAT Pro Assurance customer, you have access to a special
packaging of GNATcheck called ``gnatkp`` (GNAT Known Problem detector)
where the ``gnatcheck`` executable is replaced by ``gnatkp`` and provides
the following main user interface:

.. code-block:: sh

   gnatkp -Pproject -rules +Rkp_xxxx_xxx [+Rkp_xxxx_xxx]

where ``kp_xxxx_xxx`` is the name of a relevant known-problem to detect. You can
get the list of detectors available via the command ``gnatkp -h``. When
combined with the ``--kp-version`` and possibly ``--target`` switches (see
below), ``gnatkp -h`` will only list the detectors relevant to the version
(and target) specified.

Note that GNATkp comes in addition and not as a replacement of GNATcheck: it
only comes with known problem detectors, and does not include coding standard
rules.

The ``gnatkp`` command above will process all the files in the
given project file and run the listed known problem detectors, generating
a list of occurrences on standard error, as well as in a file called
:file:`gnatkp.out`.

Alternatively you can specify the version of GNAT Pro relevant to your
query and let ``gnatkp`` run all the registered known problem detectors
relevant to this version, via the ``--kp-version`` switch, e.g:

.. code-block:: sh

   gnatkp -Pproject --kp-version=21.2

will run all the detectors relevant to GNAT Pro 21.2. The list of detectors
will be displayed as info messages, and will also be listed in the file
:file:`gnatkp-rule-list.out`. You can also list them without running the
detectors via:

.. code-block:: sh

   gnatkp --kp-version=21.2 -h

You can also combine the ``--kp-version`` switch with the ``--target`` switch
to filter out detectors not relevant for your target, e.g:

.. code-block:: sh

   gnatkp -Pproject --kp-version=21.2 --target=powerpc-elf

will only enable detectors relevant to GNAT Pro 21.2 and to the ``powerpc-elf``
target.

Note that you need to have the corresponding target GNAT compiler installed
to use this option. By default, detectors for all targets are enabled.

You can also use the command ``gnatkp --help`` to list all the switches
relevant to ``gnatkp``.

You can check via the GNAT Tracker interface which known problems are
relevant to your version of GNAT and your target before deciding which
known problems may impact you: most known problems are only relevant to a
specific version of GNAT or a specific target. Do not hesitate to contact the
AdaCore support if needed to identify the relevant entries.

.. _Transition_from_ASIS-based_GNATcheck:

Transition from ASIS-based GNATcheck
====================================

Originally ``gnatcheck`` was implemented on top of the ASIS technology and
starting with version 23, it was re-implemented on top of the libadalang
technology. This reimplementation has kept most of the old gnatcheck interface
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

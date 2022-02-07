.. _gnatcheck_Switches:

******************
GNATcheck Switches
******************

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
  has no effect. `main_unit` should be the name of a source file that contains
  the main unit of closure. This option works properly only if `main_unit`
  has been successfully built (to compute the closure, ``gnatcheck`` needs
  to analyze `ALI` files).

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

  .. index:: -eL

``-eL``
  Follow all symbolic links when processing project files. By default,
  symbolic links are not resolved and kept as is. In some cases, resolving
  the target of symbolic links is needed for proper loading of project files.

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
  an explicit list of files.

  .. index:: --ignore

``--ignore=filename``
  Do not process the sources listed in a specified file.

  .. index:: --show-rule

``--show-rule``
  Add the corresponding rule name to the diagnosis generated for its
  violation.

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
  specified will be scanned and all filed with the extension :file:`.lkql`
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

  .. index:: --write-rules

``--write-rules=template_file``
  Write to `template_file` the template rule file that contains all the rules
  currently implemented in ``gnatcheck`` turned off. A user may edit this
  template file manually to get his own coding standard file.

If a project file is specified and no argument source is explicitly
specified (either directly or by means of ``-files`` option), and no
``-U`` is specified, then the set of processed sources is
all the immediate units of the argument project.

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

* ``0``: No tool failure and no rule violation was detected.

* ``1``: No fatal tool failure and at least one rule violation was detected.

* ``2``: A fatal tool failure was detected, or a non-fatal tool failure was
  detected while no rule violation was detected (in this case the results
  of the gnatcheck run cannot be trusted).

* ``3``: No Ada source file was checked.

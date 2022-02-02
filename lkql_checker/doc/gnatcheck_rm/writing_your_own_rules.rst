.. _Writing_Your_Own_Rules:

**********************
Writing Your Own Rules
**********************

All the predefined rules in GNATcheck are implemented via a pattern matching
language called LKQL (LangKit Query Language) which is a functional,
turing complete language, and provides as an advanced usage the ability to
write an infinite number of custom checks.

The general description of this language can be found in the
:ref:`LKQL_language_reference`.
The APIs available in LKQL are described in the :ref:`LKQL_API_doc`.

This chapter gives some additional information on how to use this language to
create your own rules and checkers.

How to write rules
------------------

Rules are written in the LKQL language, and put either in the predefined
:file:`share/lkql` directory or under any other directory specified
via the ``--rules-dir`` switch. Each :file:`.lkql` file found in these directories
will be loaded by gnatcheck and represents a distinct rule (or a set of helper
functions). The naming convention of the rules is ``lowercase_with_underscores``.

Here is a simple rule example, that will just flag every body:

.. code-block:: lkql

   @check
   fun bodies(node) = node is Body

Adding this source in the ``bodies.lkql`` file in a directory listed via
``--rules-dir`` will add a rule to GNATcheck dynamically, without the need to
modify GNATcheck itself.

Boolean rules
~~~~~~~~~~~~~

Boolean rules are functions that take a node and return a boolean and are
marked with the ``@check`` decorator. They usually contain an ``is`` pattern
match as the main expression:

.. code-block:: lkql

   @check
   fun goto(node) = node is GotoStmt

But are not limited to this, and can contain arbitrary expressions as
long as they return a boolean, e.g:

.. code-block:: lkql

   @check
   fun goto_or_loop(node) =
       match node
       | GotoStmt     => true
       | BaseLoopStmt => true
       | *            => false


Unit rules
~~~~~~~~~~

Unit rules are functions that take an analysis unit and return a list of
objects containing a message and a location. They're meant to be ultimately
flexible, and fullfill the needs that boolean rules can't fullfill, as for example:

- Customizing messages.
- Having a non 1 to 1 relationship between messages and nodes.
- Having token based rules.

The returned objects must have two keys:

- ``message``: Contains the message to be displayed.
- ``loc``: Either a node or a token, used as the source location for the error
  message.

These functions are marked with the ``@unit_check`` decorator:

.. code-block:: lkql

   @unit_check
   fun goto_line(unit) = [
      {message:
         "go to line " &
         img(node.f_label_name.p_referenced_decl().token_start().start_line),
       loc: node.f_label_name}
      for node in (from unit.root select GotoStmt)
   ]

The above rule will find each goto statement and generate a message for
each, listing the line where the target label of the goto is defined.

For example given this code:

.. code-block:: ada
   :linenos:

   procedure Go_To is
   begin
      goto Foo;
      ...
   <<Foo>>
      ...
   end Go_To;

The following gnatcheck call (assuming the file :file:`goto_line.lkql` is found
in the current directory) will output:

.. code-block:: sh

    $ gnatcheck -d go_to.adb --rules-dir=. -rules +Rgoto_line
    go_to.adb:3:09: go to line 5

Rule arguments
~~~~~~~~~~~~~~

Rules can take different optional arguments:

* ``message``: The custom message that is to be shown for a given rule on the
  command line. Defaults to the name of the rule if not specified.

* ``help``: The help message that is to be shown via ``gnatcheck -h``. Defaults to
  message if not specified.

* ``follow_generic_instantiations``: Whether to follow generic instantiations
  during the traversal of given Ada units. If ``true``, generic instantiations
  will be traversed in instantiated form. Defaults to ``false``.

* ``category``, ``subcategory``: The category (and subcategory) associated with this
  rule, used by gnatcheck as part of its ``-hx`` output. Defaults to ``Misc``.

* remediation: A string with the following possible values:

  * EASY
  * MEDIUM
  * MAJOR

  Used by ``gnatcheck -h`` and by the SonarQube integration to compute technical debt.
  Defaults to `MEDIUM`.

Here is an example rule:

.. code-block:: lkql

   @check(message="integer object declaration", follow_generic_instantiations=true)
   fun int_obj_decl(node) =
       |" Will flag object declarations for which the type is the standard
       |" ``Integer`` type
       node is o@ObjectDecl(
           p_type_expression() is SubtypeIndication(
               p_designated_type_decl() is t@* when t == o.p_std_entity("Integer")))

Debugging Your Rules
--------------------

When writing new rules, you should first enable the gnatcheck switch ``-d``
so that any LKQL runtime error (such as type mismatches, wrong nodes or syntax
errors) are reported as part of the gnatcheck output.

You can then use one (or a mix) of the approaches described in the following
sections.

The LKQL REPL
~~~~~~~~~~~~~

LKQL comes with a REPL (Read-Eval-Print-Loop) which allows you to elaborate and
verify all your LKQL expressions line by line, as well as explore the available
properties and functions via the code completion provided by this interactive
environment.

The REPL is a python script called ``lkql_repl.py`` which is found in the
:file:`bin` directory of your LKQL installation. In order to execute it,
you need a Python installation (3.7 or later), along with the ``prompt_toolkit``
module installed:

.. code-block:: sh

   $ pip install prompt_toolkit

You then need to setup the proper environment by adding the directory
:file:`LKQL install root/lib/python` to the ``PYTHONPATH`` environment
variable. For example under a Linux shell, assuming LKQL is installed under
:file:`/opt/lkql`:

.. code-block:: sh

   $ export PYTHONPATH=/opt/lkql/lib/python:$PYTHONPATH

Once done, you should be able to run ``lkql_repl.py``:

.. code-block:: sh

   $ lkql_repl.py -Pprj

where prj is your project file :file:`prj.gpr`. From there you have access to
an interactive shell which provide a history of commands available via e.g. the
up and down keys, as well as automatic completion. To exit this shell, you
can use the :kbd:`Control-D` key combination.

Here is an example session:

.. code-block:: sh

   $ lkql_repl.py -Pprj

   .-.   .-. .-..----. .-.
   | |   | |/ //  {}  \| |        Welcome to LKQL repl
   | `--.| |\ \\      /| `--.     type 'help' for more information
   `----'`-' `-'`-----``----'

    > val root=select first AdaNode
   ()
    > print(root)
   <CompilationUnit loop3.adb:1:1-41:11>
   ()
    > root.dump
   CompilationUnit[1:1-41:11]
   |f_prelude:
   |  AdaNodeList[1:1-1:1]: <empty list>
   |f_body:
   |  LibraryItem[1:1-41:11]
   [...]
    > val ops=select BinOp
   ()
    > print ops
   [<BinOp file1.adb:3:54-3:59>, <RelationOp file1.adb:6:56-6:62>, ...]
    > ops[1].dump
   BinOp[3:54-3:59]
   |f_left:
   |  Id[3:54-3:55]: L
   |f_op:
   |  OpMinus[3:56-3:57]
   |f_right:
   |  Id[3:58-3:59]: R
   ()
    > print ops[1].f_left
   <Id "L" file1.adb:3:54-3:55>
    > print ops[1].f_left.p_referenced_decl()
   <ParamSpec ["L", "R"] file1.adb:3:19-3:33>
    > select ParamSpec
   [<ParamSpec ["L", "R"] file1.adb:2:19-2:33>, <ParamSpec ["L", "R"] file1.adb:3:19-3:33>, ...]
    > select p@ParamSpec when [n for n in p.f_ids.children if n.f_name.p_name_is("Str")]
   [<ParamSpec ["Str"] file1.adb:1:18-1:37>, <ParamSpec ["Str"] file2.adb:1:18-1:37>]
    > ^D
   Do you really want to exit ([y]/n)? y

Print Technique
~~~~~~~~~~~~~~~

Another option to verify at various steps that your rule is doing the right
thing is to insert calls to ``print``, ``dump`` or ``img`` functions by e.g.
inserting variables:

.. code-block:: lkql

   fun do_this(node) = {
       val debug1 = print(node);
       val debug2 = print("parent node is: " & img(node.parent));
       val debug3 = node.parent.dump;
       do_that()
   }

Inside a boolean expression, you can also insert a call to ``print`` which
will always evaluate to ``false``:

.. code-block:: lkql

   node is GotoStmt and (print(node) or real_expression())

Note that print statements will be output immediately on standard output, while
gnatcheck messages are stored internally and dumped at the end. In addition,
the default gnatcheck output may interfere with your print statements, so it is
recommended to use the ``-v`` or ``--brief`` switches to avoid or reduce the
interference.

A Complete Step By Step Example
-------------------------------

In this section, we will implement step by step a rule to detect
integer types that could be replaced by an enumeration type.

To find such types, we first need to define a ``@check`` looking for all
type declarations, with an associated message:

.. code-block:: lkql

   @check(message="integer type may be replaced by an enumeration")
   fun integer_types_as_enum(node) = node is TypeDecl

Then let's refine the rule to only consider integer type declarations,
by using the libadalang ``p_is_int_type`` property:

.. code-block:: lkql

   @check(message="integer type may be replaced by an enumeration")
   fun integer_types_as_enum(node) = node is TypeDecl(p_is_int_type() is true)

Now, we'll add a first criteria to consider: there should be no use
of any arithmetic operator on this type anywhere in the sources. To
achieve that, we need to perform a global query on the whole project,
which is done via a ``select`` query, to find all the references to arithmetic
operators:

.. code-block:: lkql

   select BinOp(f_op is OpDiv or OpMinus or OpMod or OpMult or
                        OpPlus or OpPow or OpRem or OpXor)
       or UnOp(f_op is OpAbs or OpMinus or OpPlus)

we then create a function that will compute all the types associated with
these expressions in a list:

.. code-block:: lkql

   fun arithmetic_ops() =
       |" Return a list of all types referenced in any arithmetic operator
       [op.p_expression_type()
        for op in select
            BinOp(f_op is OpDiv or OpMinus or OpMod or OpMult or
                          OpPlus or OpPow or OpRem or OpXor) or
            UnOp(f_op is OpAbs or OpMinus or OpPlus)].to_list

and we update our rule accordingly to find all integer types for which no
arithmetic operator is found. To achieve that, we use a list comprehension
to iterate over the list returned by ``arithmetic_ops`` and take advantage
of the semantic of list comprehensions when used in a boolean expression:
a list with no element evaluates to ``false``, and a list with at least one
element evaluates to ``true``:

.. code-block:: lkql

   fun integer_types_as_enum(node) =
        node is TypeDecl(p_is_int_type() is true)
        when not [t for t in arithmetic_ops() if t == node]

Running this rule we realize that it finds some interesting matches, but
also too many false positives. In particular types referenced in type
conversions also need to be filtered out. So let's define another helper
function that will list all types referenced as a target of a type conversion.
In the libadalang tree, a type conversion appears as a ``CallExpr`` whose
referenced declaration (``p_referenced_decl`` property) is a type declaration
(``TypeDecl``). We perform another global ``select`` query:

..  code-block:: lkql

    fun types() =
        [c.p_referenced_decl()
         for c in select CallExpr(p_referenced_decl() is TypeDecl)].to_list

And we update our rule accordingly:

.. code-block:: lkql

   fun integer_types_as_enum(node) =
        node is TypeDecl(p_is_int_type() is true)
        when not [t for t in arithmetic_ops() if t == node]
         and not [t for t in types() if t == node]

So we're now filtering target types in type conversions, but that's not enough,
we also need to filter source types in type conversions, so let's refine
our ``types`` function by also using the ``f_suffix`` which is a
``ParamAssocList`` in this context with a single element, where we
compute the type of the expression via the ``p_expression_type`` property:

.. code-block:: lkql

       c.f_suffix[1].f_r_expr.p_expression_type()

We then use the ``concat`` builtin function to concatenate the previous
result with this new one and create a single dimension list of type
declarations with both source and target types of conversions:

.. code-block:: lkql

   fun types() =
       concat([[c.p_referenced_decl(), c.f_suffix[1].f_r_expr.p_expression_type()]
               for c in select CallExpr(p_referenced_decl() is TypeDecl)].to_list)

This gives much better results and much fewer false positives! We then
realize that we need to perform a similar filtering on subtype declarations:
types references in subtype declarations should not be flagged. We use
another global ``select`` on subtype declarations, and list all the
referenced types:

.. code-block:: lkql

   [s.f_subtype.f_name.p_referenced_decl() for s in select SubtypeDecl]

We combine this with the previous results:

.. code-block:: lkql

   fun types() =
       |" Return a list of TypeDecl matching all type conversions (both as source
       |" and target) and subtype declarations in the project.
       concat([[c.p_referenced_decl(), c.f_suffix[1].f_r_expr.p_expression_type()]
               for c in select CallExpr(p_referenced_decl() is TypeDecl)].to_list)
       & [s.f_subtype.f_name.p_referenced_decl() for s in select SubtypeDecl].to_list

We're getting even less false positives now, and quickly realize that we need
to do the same for type derivations:

.. code-block:: lkql

   [c.f_type_def.f_subtype_indication.f_name.p_referenced_decl()
    for c in select TypeDecl(f_type_def is DerivedTypeDef)].to_list

We combine again the results, which gives us our final ``types`` function:

.. code-block:: lkql

   fun types() =
       |" Return a list of TypeDecl matching all type conversions (both as source
       |" and target), subtype declarations and type derivations in the project.
       concat([[c.p_referenced_decl(), c.f_suffix[1].f_r_expr.p_expression_type()]
               for c in select CallExpr(p_referenced_decl() is TypeDecl)].to_list)
       & [s.f_subtype.f_name.p_referenced_decl() for s in select SubtypeDecl].to_list
       & [c.f_type_def.f_subtype_indication.f_name.p_referenced_decl()
          for c in select TypeDecl(f_type_def is DerivedTypeDef)].to_list

Running our rule again, we find a final source of false positives: types
referenced as parameter of generic instantiations also need to be filtered
out, so we define a new function to compute all declarations referenced as
parameters of a generic instantiation, via two ``select``: a global query
returning all generic instantiations:

.. code-block:: lkql

    select GenericInstantiation

and we then inject the result of this query into another select to list all
identifiers referenced by all these instantiations:

.. code-block:: lkql

    from (select GenericInstantiation) select Identifier

which gives us the following function:

.. code-block:: lkql

   fun instantiations() =
       |" Return a list of all declarations referenced in any generic instantiation
       [id.p_referenced_decl()
        for id in from (select GenericInstantiation) select Identifier].to_list

Updating our rule this gives us:

..  code-block:: lkql

    fun integer_types_as_enum(node) =
         node is TypeDecl(p_is_int_type() is true)
         when not [t for t in arithmetic_ops() if t == node]
          and not [t for t in types() if t == node]
          and not [t for t in instantiations() if t == node]

That's good enough in terms of results, but we also realize that running
this rule is very slow, so let's look at how to optimize it.

The first thing to do is to avoid repeated calls to the very costly
global select contained in functions ``arithmetic_ops``, ``types`` and
``instantiations``. We achieve that easily by marking our functions with
the ``@memoize`` decorator, so that these function calls will be cached after
the first evaluation. In addition, to avoid checking multiple times
the same type declarations, we can take advantage of the ``unique`` builtin
in each of our helper function, e.g:

..  code-block:: lkql

   @memoized
   fun instantiations() =
       unique([id.p_referenced_decl()
               for id in from (select GenericInstantiation) select Identifier].to_list)

Finally, we notice that there are many more arithmetic operators to check
in a project than type conversion or generic instantiations, so we swap the
order of the tests:

..  code-block:: lkql

    fun integer_types_as_enum(node) =
         node is TypeDecl(p_is_int_type() is true)
         when not [t for t in types() if t == node]
          and not [t for t in instantiations() if t == node]
          and not [t for t in arithmetic_ops() if t == node]

which gives us this complete rule:

.. code-block:: lkql

   @memoized
   fun arithmetic_ops() =
       |" Return a list of all types referenced in any arithmetic operator
       unique([op.p_expression_type()
               for op in select
                   BinOp(f_op is OpDiv or OpMinus or OpMod or OpMult or
                                 OpPlus or OpPow or OpRem or OpXor) or
                   UnOp(f_op is OpAbs or OpMinus or OpPlus)].to_list)

   @memoized
   fun instantiations() =
       |" Return a list of all declarations referenced in any generic instantiation
       unique([id.p_referenced_decl()
               for id in from (select GenericInstantiation) select Identifier].to_list)

   @memoized
   fun types() =
       |" Return a list of TypeDecl matching all type conversions (both as source
       |" and target), subtype declarations and type derivations in the project.
       unique(concat([[c.p_referenced_decl(),
                       c.f_suffix[1].f_r_expr.p_expression_type()]
                      for c in select CallExpr(p_referenced_decl() is TypeDecl)].to_list) &
              [s.f_subtype.f_name.p_referenced_decl() for s in select SubtypeDecl].to_list &
              [c.f_type_def.f_subtype_indication.f_name.p_referenced_decl()
               for c in select TypeDecl(f_type_def is DerivedTypeDef)].to_list)

   @check(message="integer type may be replaced by an enumeration")
   fun integer_types_as_enum(node) =
        node is TypeDecl(p_is_int_type() is true)
        when not [t for t in types() if t == node]
         and not [t for t in instantiations() if t == node]
         and not [t for t in arithmetic_ops() if t == node]


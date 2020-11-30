LKQL language reference
#######################

LKQL (short for Langkit query language) is a query language enabling users to
run queries on top of source code.

LKQL is based upon the [https://github.com/AdaCore/langkit](langkit)
technology. As such, it is theoretically capable of running queries on any
language with a Langkit frontend. In practice for the moment, LKQL is hardwired
for Ada.
LKQL today is the mixture of two language subsets:

* The first is a general, dynamically typed, functional, small but general
  purpose programming language, including function definitions, common
  expressions, very basic support for numeric types and computations, list
  comprehensions, etc.

* The second is a tree query language, allowing the user to express
  very concisely a predicate over a node and its syntactic and semantic
  relatives. More examples to come.

Those two languages will be documented separately. The general language will be
documented first, because its knowledge is needed for understanding the tree
query language.

General purpose language subset
===============================

This language subset is composed of a reduced set of declarations and
expressions that forms a minimal but turing complete language.

For the time being, *it has no side effects*, which is intended since the
purpose of LKQL is strictly to express queries.

Data types
----------

LKQL has a very limited number of data types for the moment. Here are the
current data types:

* Integers: Very often used as parameters in queries, supports simple
  arithmetic.

* Strings: Built-in string type, that supports concatenation.

* Booleans: Built-in boolean type, that supports the usual expected boolean
  relational operators.

* Nodes: Coming from the queried language (in the common case, Ada), those are
  the only composite data types for the moment. They correspond to the syntax
  nodes of the source files being queried. They can be explored as part of the
  general language subset, through :ref:`Field access`, or via the :ref:`Tree
  query language subset`.

Declarations
------------

Declarations in LKQL only belong at the top level. There is no support
currently for nested declarations, except for
:ref:`Value declarations<Value declaration>` in the :ref:`Block expression`.

Function declaration
^^^^^^^^^^^^^^^^^^^^

.. lkql_doc_class:: FunDecl
.. lkql_doc_class:: ParameterDecl

.. raw:: html
    :file: ../../lkql/build/railroad-diagrams/fun_decl.svg

.. raw:: html
    :file: ../../lkql/build/railroad-diagrams/param.svg

Allows the user to declare an LKQL function that can be used to factor some
computation.

.. code-block:: lkql

    fun add(x, y) = x + y

The syntax is simple, you only declare argument names and an expression after
the ``=``.

If you need to declare temporary named values in the body of your function, you
can use a :ref:`Block expression`.

.. code-block:: lkql

    fun add(x, y) = {
        val ret = x + y;
        ret
    }

Value declaration
^^^^^^^^^^^^^^^^^

.. lkql_doc_class:: ValDecl

.. raw:: html
    :file: ../../lkql/build/railroad-diagrams/val_decl.svg


Declare a named value (often called a variable or constant in other languages).
The value is immutable.

.. code-block:: lkql

    val a = 12 + 15

Expressions
-----------

Block expression
^^^^^^^^^^^^^^^^

.. raw:: html
    :file: ../../lkql/build/railroad-diagrams/block_expr.svg

.. lkql_doc_class:: BlockExpr

The block expression is useful to declare temporary named values. This can be
useful to share the result of a temporary calculation, or to name an
intermediate value to make the code more readable.

.. code-block:: lkql

    {
       val x = 40;
       val y = 2;
       x + y
    }

As you can see in the example above, value declarations are ended by
semicolons. After the last value declaration, you write the block's result
expression, without an ending semicolon.

Field access
^^^^^^^^^^^^

.. lkql_doc_class:: DotAccess

A field access returns the contents of a field. In the following example, we
get the content of the  ``type_expr`` syntax field on a node of type
``ObjectDecl``.

.. code-block:: lkql

    object_decl.type_expr

.. note::

    Ultimately, this construction will be extended to allow access to struct
    fields, but structs are not yet supported.

A regular field access on a nullable variable is illegal, which is why field
access has a variant, which is called a "safe access":

.. code-block:: lkql

    object_decl?.type_expr

The safe access will return null if the left hand side is null. This allows
users to chain accesses without having to checks for nulls at every step.

For a
reference of the existing fields for syntax nodes for Ada, look at the
`Libadalang API doc
<https://docs.adacore.com/live/wave/libadalang/html/libadalang_ug/python_api_ref.html>`_.
The fields are prefixed by ``f_`` in the Python API reference, whereas
they're accessible without the prefix in LKQL.

Property call
^^^^^^^^^^^^^

.. lkql_doc_class:: DotCall
.. lkql_doc_class:: SafeCall

Properties are methods on syntax nodes, returning results of high level
queries, possibly answering semantic questions about the syntax tree. For a
reference of the existing properties for Ada, look at the
`Libadalang API doc
<https://docs.adacore.com/live/wave/libadalang/html/libadalang_ug/python_api_ref.html>`_.
The properties are prefixed by ``p_`` in the Python API reference, whereas
they're callable without the prefix in LKQL.

.. code-block:: lkql

    object_decl.is_static_decl()

Just as for field accesses, property calls have their "safe property calls"
variant that can be used to call a property on a nullable object, and return
null if the object is null.

.. code-block:: lkql

    object_decl?.is_static_decl()

Unwrap expression
^^^^^^^^^^^^^^^^^

.. lkql_doc_class:: Unwrap

When you have a nullable object and you want to make it non nullable, you can
use the unwrap expression. This is useful after a chain of safe accesses/calls,
for example.

.. code-block:: lkql
    object_decl?.type_expr?.designated_type_decl!!

Unwrap will raise an error if the value is null.

Function call
^^^^^^^^^^^^^

.. lkql_doc_class:: FunCall
.. lkql_doc_class:: Arg

.. raw:: html
    :file: ../../lkql/build/railroad-diagrams/fun_call.svg


:ref:`Functions <Function declaration>` defined in LKQL can be called with the
function call expression.

.. code-block:: lkql

    fun add(a, b) = a + b

    val c = add(12, 15)
    val d = add(a=12, b=15)

Parameters can be passed via positional or named associations.

Indexing expression
^^^^^^^^^^^^^^^^^^^

.. lkql_doc_class:: Indexing

Indexing expressions allow the user to access elements of a list, array, or
string.

Here are examples of indexing expressions:

.. code-block:: lkql

    list[0]

    "pouet"[1]

    {
        val x = 2;
        "pouet"[x]
    }

Comparison expression
^^^^^^^^^^^^^^^^^^^^^

.. raw:: html
    :file: ../../lkql/build/railroad-diagrams/comp_expr.svg

Comparison expressions are used to compare an object to another object, or
pattern.

Membership expression
"""""""""""""""""""""

.. lkql_doc_class:: InClause

The membership expression verifies that a collection (list/array/string)
contains the given value.

.. code-block:: lkql

    12 in list

Is expression
"""""""""""""

.. lkql_doc_class:: IsClause


The "is" expression verifies if a node object matches a :ref:`Pattern`.

.. code-block:: lkql

   val a = select AdaNode
   val b = a[0] is ObjectDecl

Comparison operators
""""""""""""""""""""

.. lkql_doc_class:: RelBinOp

The usual comparison operators are available. Order dependent operators
(``<``/``>``/...) are only usable on integers.

.. code-block:: lkql

   12 < 15
   a == b
   b != c

List comprehension
^^^^^^^^^^^^^^^^^^

.. lkql_doc_class:: ListComprehension
.. lkql_doc_class:: ListCompAssoc

.. raw:: html
    :file: ../../lkql/build/railroad-diagrams/listcomp.svg

A list comprehension allows the user to create a new list by iterating on an
existing collection, applying a mapping operation, and eventually a filtering
operation.

.. code-block:: lkql

    # Simple list comprehension that'll double every number in int_list if it
    # is prime

    [a * 2 for a in int_list if is_prime(a)]

    # Complex example interleaving two collections

    val subtypes = select SubtypeIndication
    val objects = select ObjectDecl
    print([o.image & " " & st.image
           for o in objects, st in subtypes
           if (o.image & " " & st.image).length != 64])

A list comprehension is a basic language construct, that, since LKQL is purely
functional, replaces traditional for loops.

If expression
^^^^^^^^^^^^^

.. lkql_doc_class:: IfThenElse

.. raw:: html
    :file: ../../lkql/build/railroad-diagrams/if_then_else.svg

If expressions are traditional conditional expressions composed of a condition,
an expression executed when the condition is true, and and expression executed
when the condition is false.

.. code-block:: lkql

   # No parentheses required
   val a = if b < 12 then c() else d()

Match expression
^^^^^^^^^^^^^^^^

.. lkql_doc_class:: Match
.. lkql_doc_class:: MatchArm

.. raw:: html
    :file: ../../lkql/build/railroad-diagrams/match.svg

Pattern matching expression. Matchers will be evaluated in order against the
match's target expression. The first matcher to match the object will trigger
the evaluation of the associated expression in the match arm.

.. code-block:: lkql

   match nodes[0]
     | ObjectDecl(has_aliased=aliased @ _) => aliased
     | ParamSpec(has_aliased=aliased @ _) => aliased
     | _ => False

.. note:: For the moment, there is no check that the matcher is complete. A
   match expression where no arm has matched will raise an exception at
   runtime.

.. admonition:: todo

   Verify that bindings of names to matched values work correctly

Literals and Operators
^^^^^^^^^^^^^^^^^^^^^^

.. lkql_doc_class:: Literal

.. lkql_doc_class:: ArithBinOp
.. lkql_doc_class:: NotNode

LKQL has literals for booleans, integers, strings, and null values:

.. code-block:: lkql

    val a = 12
    val b = true
    val c = "hello"
    val d = null

LKQL has a few built-in operators available:

- Basic arithmetic operators on integers

.. code-block:: lkql

    a + 2 * 3 / 4 = b
    a <= b
    b >= c

- Basic relational operators on booleans

.. code-block:: lkql

    true and false or (a = b) and (not c)

- String concatenation

.. code-block:: lkql

    "Hello" & name

Tree query language subset
==========================

The tree query language subset is mainly composed of three language constructs:
patterns, queries and selectors.

Patterns allow the user to express filtering logic on trees and graphs, akin to
what regular expressions allow for strings.

A lot of the ideas behind patterns are similar to ideas in
`XPath <https://developer.mozilla.org/fr/docs/Web/XPath>`_,
or even in
`CSS selectors <https://developer.mozilla.org/en-US/docs/Glossary/CSS_Selector>`_

However, unlike in CSS or xpath, a pattern is just the filtering logic, not the
traversal, even though filtering might contain sub traversals via selectors.

The query allows to run the pattern on a tree, traversing its children.

Here is a very simple example of a selector, that will select object
declarations that have the aliased qualifier.

.. code-block:: lkql

   ObjectDecl(has_aliased=true)

And here is its use in a query:

.. code-block:: lkql

   select ObjectDecl(has_aliased=true)

This will query every source file in the LKQL context, and filter according to
the pattern.

.. note:: Queries are expressions, so you can write:

   .. code-block:: lkql

      val a = select ObjectDecl(has_aliased=true)

.. admonition:: todo

   Patterns are not yet expressions, but they certainly could be and
   should be, so we're planning on improving that at a later stage.

Finally, selectors are a way to express "traversal" logic on the node graph.
Syntactic nodes, when explored through their syntactic children, form a tree.
However:

* There are different ways to traverse this tree (for example, you can explore
  the parents starting from a node)

* There are non syntactic ways to explore nodes, for example using semantic
  properties such as going from references to their declarations, or going up
  the tree of base types for a given tagged type.

All those traversals, including the most simple built-in one, use what is
called selectors in LKQL.

Selectors are a way to specify a traversal, which will return a lazy list of
nodes as a result. Here is an example, the selector that will go up the parent
chain.

.. code-block:: lkql

   selector parent
      | AdaNode => rec *it.parent
      | _       => ()

Query expression
----------------

.. raw:: html
    :file: ../../lkql/build/railroad-diagrams/query.svg

.. lkql_doc_class:: Query

The query expression is extremely simple, and most of the complexity lies in
the upcoming sections about patterns.

A query traverses one or several trees, from one or several root nodes,
applying the pattern on every node. It yields all matching nodes.

Pattern
-------

.. lkql_doc_class:: UnfilteredPattern
.. lkql_doc_class:: ValuePattern

.. raw:: html
    :file: ../../lkql/build/railroad-diagrams/pattern.svg

.. raw:: html
    :file: ../../lkql/build/railroad-diagrams/chained_pattern.svg

.. raw:: html
    :file: ../../lkql/build/railroad-diagrams/binding_pattern.svg

Patterns are by far the most complex part of the tree query language subset,
but at its core, the concept of a pattern is very simple:

A pattern is at its core a very simple concept: it's an expression that you
will match against a node. In the context of a query, the pattern will return a
node or collection of nodes for each matched node. In the context of an ``is``
comparison expression, lkql will check that the node matches the pattern, and
produce ``true`` if it does.

High level pattern kinds
^^^^^^^^^^^^^^^^^^^^^^^^

There are two kinds of top-level patterns: chained patterns and nested patterns
(called value_patterns in the grammar), and the way they're different is in how
you use sub-patterns. In the end they'll they differ by which nodes will be
produced by the pattern when used in a query. Let's take an example to
illustrate:

.. code-block:: lkql

   select ObjectDecl(default_expr is IntLiteral)

This query uses a nested pattern, it will return every ``ObjectDecl`` that has
an ``IntLiteral`` node in the default expression.

.. code-block:: lkql

   select ObjectDecl.default_expr is IntLiteral

This query uses a chained pattern, it will return every ``IntLiteral`` that is
the default expression of an ``ObjectDecl``.

Hence, the difference between the two kind of sub-patterns is that in the first
case, the sub-pattern doesn't change what is returned, it only adds a filtering
condition, whereas in the second case, the chained pattern makes the pattern
return a sub object.

Simple value patterns
^^^^^^^^^^^^^^^^^^^^^

.. raw:: html
    :file: ../../lkql/build/railroad-diagrams/value_pattern.svg

A value pattern is the simplest atom for node patterns.

In its simple form, it can be either ``_``, which is the wildcard pattern, and
will match everything, or a node name:

.. code-block:: lkql

   select _ # Will select every node
   select BasicDecl # Will select every basic declaration

In its more complex form, it can have sub-patterns in an optional part between
parentheses:

.. code-block:: lkql

   select BasicDecl(...)

Nested sub patterns
^^^^^^^^^^^^^^^^^^^

.. lkql_doc_class:: NodePatternDetail
.. lkql_doc_class:: DetailValue

.. raw:: html
    :file: ../../lkql/build/railroad-diagrams/pattern_arg.svg

Inside the optional parentheses of value patterns, the user can add
sub-patterns that will help refine the query. Those patterns can be of three
different kind:

Selector predicate
""""""""""""""""""

A selector predicate is a sub-pattern that allows you to run a sub-query and to
match its results:

.. code-block:: lkql

   select Body(any children is ForLoopStmt)

The quantifier part (``any``) can be either ``any`` or ``all``, which will
alter how the sub-pattern matches:

* ``all`` will match only if all nodes returned by the selector match the condition
* ``any`` will match as soon as at least one child matches the condition.

Any of the :ref:`Built-in selectors` can be used, or even custom selectors.

Field predicate
"""""""""""""""

A field predicate is a sub-pattern that allows you to match a sub-pattern
against a specific field in the parent object. We have already seen such a
construct in the introduction, and it's one of the simplest kind of patterns.

.. code-block:: lkql

   select ObjectDecl(default_val is IntLiteral)

Property call predicate
"""""""""""""""""""""""

A property predicate is very similar to a field predicate, except that a
property of the node is called, instead of a field accessed. Syntactically,
this is denoted by the parentheses after the property name.

.. code-block:: lkql

   select BaseId(referenced_decl() is ObjectDecl)

Chained sub patterns
^^^^^^^^^^^^^^^^^^^^

.. lkql_doc_class:: ChainedPatternLink
.. lkql_doc_class:: SelectorCall

Chained sub patterns are roughly similar to nested sub patterns, and come in
similar flavours. The big difference between the two kind of patterns, is which
nodes are yielded when the pattern is used in a query. Chained patterns will
yield the sub-nodes, rather than just filtering and returning the top level
node.

You have the three different kind of chained patterns, corresponding to the
nested ones.

Selector chain
""""""""""""""

A selector chain is a sub-pattern that allows you to recursively yield a
sub-query via a selector call:

.. code-block:: lkql

   select Body any children is ForLoopStmt

The quantifier part (``any``) can be either ``any`` or ``all``, which will
alter how the sub-pattern matches:

Field chain
"""""""""""

A field chain is a sub-pattern that allows you to yield a specific field in the
parent object, given that it satisfies a pattern.

.. code-block:: lkql

   select ObjectDecl.default_val is IntLiteral

This will yield the default values for object decls, given that those default
values are int literals.

Property chain
""""""""""""""

A property chain is very similar to a field chain, except that a property of
the node is called, instead of a field accessed. Syntactically, this is denoted
by the parentheses after the property name.

.. code-block:: lkql

   select BaseId referenced_decl() is ObjectDecl

Filtered patterns and binding patterns
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. lkql_doc_class:: FilteredPattern
.. lkql_doc_class:: BindingPattern

While you can express a lot of things via the regular pattern syntax mentioned
above, sometimes it is necessary to be able to express an arbitrary boolean
condition in patterns. This is done via the `when` clause.

However, in order to be able to express conditions on the currently matched
objects, or arbitrary objects in the query, naming those objects is necessary.
This is done via binding patterns:

.. code-block:: lkql

   select b @ BaseId # Same as "select BaseId", but now every BaseId object
                     # that is matched has a name that can be used in the when
                     # clause

.. code-block:: lkql

   val a = select BasicDecl
   select b @ BaseId when b.referenced_decl() = a

Selector declaration
--------------------

.. lkql_doc_class:: SelectorDecl
.. lkql_doc_class:: SelectorExpr
.. lkql_doc_class:: SelectorExprMode
.. lkql_doc_class:: SelectorArm

.. raw:: html
    :file: ../../lkql/build/railroad-diagrams/selector_decl.svg

.. raw:: html
    :file: ../../lkql/build/railroad-diagrams/selector_arm.svg

.. raw:: html
    :file: ../../lkql/build/railroad-diagrams/selector_expr.svg

Selectors are a special form of functions that return a lazy stream of node
values. They're at the basis of the query DSL of LKQL, allowing the easy
expression of traversal blueprints.

For example, by default, the :ref:`Query expression` explores the tree via the
default ``children`` selector.

You've already seen selectors used in previous sections, and, most of the time,
you might not need to define your own, but in case you need to, here is how
they work.

Defining a selector
^^^^^^^^^^^^^^^^^^^

A selector is a recursive function. It has a single implicit `it` argument that
represents the current node. A selector has an implicit top level :ref:`Match
expression` matching on `it`.

.. note:: The principle of selectors is more general than nodes, but is for the
   moment only usable with an ``it`` argument that is of type node.

In the branch of a selector, you have three choices:

* You can **recurse** via the ``rec`` keyword, on nodes reachable from ``it``.
  The node or nodes you will recurse on via this keyword will both be "yielded"
  by the selector, and explored further (ie. the selector will be called on
  them)

* You can **recurse but skip the node(s)**, via the ``skip`` keyword. This'll have
  the same effect as ``rec``, except that it will not yield the node(s).

* You can **return but not recurse**: This is the default action (requires no
  keyword), and will yield the node(s), but not recurse on them.

.. code-block:: lkql

    selector 

Built-in selectors
^^^^^^^^^^^^^^^^^^

The built-in selectors are:

* ``parent``: parent nodes.
* ``children``: child nodes.
* ``prevSiblings``: sibling nodes that are before the current node.
* ``nextSiblings``: sibling nodes that are after the current node.
* ``superTypes``: if the current node is a type, then all its supertypes.

..
   * Operators need not be documented, since they're documented as part of the
     BinOp hierarchy
   * It's not necessary to document identifiers AFAICT

.. lkql_doc_class:: Op
.. lkql_doc_class:: Identifier

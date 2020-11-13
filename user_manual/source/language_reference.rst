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
    :file: ../../lkql/build/doc/svg/fun_decl.svg

.. raw:: html
    :file: ../../lkql/build/doc/svg/param.svg

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
    :file: ../../lkql/build/doc/svg/val_decl.svg


Declare a named value (often called a variable or constant in other languages).
The value is immutable.

.. code-block:: lkql

    val a = 12 + 15

Expressions
-----------

Block expression
^^^^^^^^^^^^^^^^

.. raw:: html
    :file: ../../lkql/build/doc/svg/block_expr.svg

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

A field access returns the contents of the syntax field ``type_expr``.
.. code-block:: lkql object_decl.type_expr

.. note::

    Ultimately, this construction will be extended to allow access to struct
    fields, but structs are not yet supported.

For a
reference of the existing fields for syntax nodes for Ada, look at the
`Libadalang API doc
<https://docs.adacore.com/live/wave/libadalang/html/libadalang_ug/python_api_ref.html>`_.
The fields are prefixed by ``f_`` in the Python API reference, whereas
they're accessible without the prefix in LKQL.

Property call
^^^^^^^^^^^^^

.. lkql_doc_class:: DotCall

Properties are methods on syntax nodes, returning results of high level
queries, possibly answering semantic questions about the syntax tree. For a
reference of the existing properties for Ada, look at the
`Libadalang API doc
<https://docs.adacore.com/live/wave/libadalang/html/libadalang_ug/python_api_ref.html>`_.
The properties are prefixed by ``p_`` in the Python API reference, whereas
they're callable without the prefix in LKQL.

.. code-block:: lkql

    object_decl.is_static_decl()


Function call
^^^^^^^^^^^^^

.. lkql_doc_class:: FunCall
.. lkql_doc_class:: Arg

.. raw:: html
    :file: ../../lkql/build/doc/svg/fun_call.svg


:ref:`Functions <Function declaration>` defined in LKQL can be called with the
function call expression.

.. code-block:: lkql

    fun add(a, b) = a + b

    val c = add(12, 15)
    val d = add(a=12, b=15)

Parameters can be passed via positional or named associations.

Indexing expression
^^^^^^^^^^^^^^^^^^^

.. lkql_doc_class:: DotCall

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
    :file: ../../lkql/build/doc/svg/comp_expr.svg

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

.. raw:: html
    :file: ../../lkql/build/doc/svg/listcomp.svg

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
    :file: ../../lkql/build/doc/svg/if_then_else.svg

If expressions are traditional conditional expressions composed of a condition,
an expression executed when the condition is true, and and expression executed
when the condition is false.

.. code-block:: lkql

   # No parentheses required
   val a = if b < 12 then c() else d()

Match expression
^^^^^^^^^^^^^^^^

.. lkql_doc_class:: Match

.. raw:: html
    :file: ../../lkql/build/doc/svg/match.svg

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

    true and false or (a = b)

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

   val ods = query ObjectDecl(has_aliased=true)

This will query every source file in the LKQL context, and filter according to
the pattern.

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

Selector declaration
--------------------

.. raw:: html
    :file: ../../lkql/build/doc/svg/selector_decl.svg

.. raw:: html
    :file: ../../lkql/build/doc/svg/selector_arm.svg

.. raw:: html
    :file: ../../lkql/build/doc/svg/selector_expr.svg

Selectors are a special form of functions that return a lazy stream of node
values. They're at the basis of the query DSL of LKQL, allowing the easy
expression of traversal blueprints.

For example, by default, the :ref:`Query expression` explores the tree via the
default ``children`` selector.

.. note:: The principle of selectors is more general than nodes, but is for the
   moment only usable with nodes.

A selector is a recursive function. It has a single implicit `it` argument that
represents the current node. A selector has an implicit top level :ref:`Match
expression` matching on `it`.

Query expression
----------------

Pattern
-------

..
   * Operators need not be documented, since they're documented as part of the
     BinOp hierarchy
   * It's not necessary to document identifiers AFAICT

.. lkql_doc_class:: Op
.. lkql_doc_class:: Identifier

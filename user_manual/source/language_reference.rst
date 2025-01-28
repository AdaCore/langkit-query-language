.. _LKQL_language_reference:

LKQL Language Reference
#######################

LKQL (short for LangKit Query Language) is a query language enabling users to
run queries on top of source code.

LKQL today is the mixture of two language subsets:

* The first is a dynamically typed, functional, small but general purpose
  programming language, including function definitions, common expressions,
  very basic support for numeric types and computations, list comprehensions,
  etc.

* The second is a tree query language, allowing the user to express very
  concisely predicates over a node and its syntactic and semantic relatives,
  and tree traversal logics.

Those two subsets will be documented separately. The general language will be
documented first, because its knowledge is needed for understanding the tree
query language.

LKQL is based upon the `langkit <https://github.com/AdaCore/langkit>`_
technology. As such, it is theoretically capable of running queries on any
language with a Langkit frontend. In practice for the moment, LKQL is hardwired
for Ada (and Libadalang).

.. attention::

  While mostly stable, LKQL is not completely done yet. The
  language will keep being extended with new constructs, and from time to time
  syntax might change to accommodate new language constructs/enhance the
  language ergonomics/fix design mistakes. Read the `Language Changes`_
  section for more information.



General Purpose Language Subset
===============================

This language subset is composed of a reduced set of declarations and
expressions that forms a minimal but turing complete language.

For the time being, *it has no side effects*, which is intended since the
purpose of LKQL is strictly to express queries.



Data Types
----------

LKQL has a very limited number of data types for the moment. Here are the
current data types:


Basic Data Types
^^^^^^^^^^^^^^^^

``Unit``
  A type used to represents empty values.

``Int``
  Basic integer type, supporting arbitrary sized values. Supports simple
  arithmetic.

``Str``
  Built-in string type, that supports concatenation.

``Bool``
  Built-in boolean type, that supports the usual expected boolean
  relational operators.

``Node``
  Coming from the queried language (in the common case, Ada). Nodes
  correspond to the syntax nodes of the source files being queried. They can
  be explored as part of the general language subset, through `Field access`_,
  or via the `Query language subset`_.

``Token``
  Also coming from the queried language. Tokens correspond to lexical
  units of the queried source files.

``Pattern``
  Values of this type are compiled regular expressions that can be used in a
  few contexts to match a string against, notably in the string built-in
  functions ``contains`` and ``find``.

``Function``
  LKQL functions are first class citizens, thus, any expression can has this
  type which represents values that can be called with a `call expression`_.


Composite Data Types
^^^^^^^^^^^^^^^^^^^^

``Tuple``
  Tuples are heterogeneous groups of values with a fixed size. They can be
  indexed to access inner values, a bit like Python tuples.

  .. attention::

    Tuples are indexed starting from ``1``, like in Lua/R/.., unlike in
    Python/Java/..

``List``
  Lists are contiguous immutable sequences of items that can be indexed. Lists
  also support concatenation.

  .. attention::

    Like tuples, Lists are indexed starting from ``1``.

``Object``
  Objects are heterogeneous records that can contain any number of key to value
  mappings, where keys are labels and values are any valid LKQL value.



Declarations
------------

Declarations in LKQL only belong at the top level. There is no support
currently for nested declarations, except in the `block expression`_.


Function Declaration
^^^^^^^^^^^^^^^^^^^^

.. lkql_doc_class:: FunDecl
.. lkql_doc_class:: DeclAnnotation
.. lkql_doc_class:: ParameterDecl

.. raw:: html
  :file: ../../lkql/build/railroad-diagrams/fun_decl.svg

.. raw:: html
  :file: ../../lkql/build/railroad-diagrams/param.svg

Allows the user to declare an LKQL function that can be used to factor some
computation:

.. code-block:: lkql

  fun add(x, y) = x + y

The syntax is simple, you only declare argument names and an expression after
the ``=``.

If you need to declare temporary named values in the body of your function, you
can use a `block expression`_:

.. code-block:: lkql

  fun add(x, y) = {
      |" Add two integers
      val ret = x + y;
      ret
  }


.. note::

  A function can have annotations. For the moment, this is used only in the
  context of LKQL checkers:

  .. code-block:: lkql

    @check(message="Bla detected")
    fun is_bla() = node is Bla

Functions can also be nested in other functions, and closures are allowed, ie.
you can return a function that references the environment in which it was
declared:

.. code-block:: lkql

  fun make_closure(closure_var) = {
      fun use_closure() = closure_var + 1;
      use_closure
  }

  # This will display the functional value "use_closure"
  print(make_closure(12))

.. note::

  Functions can be memoized via the @memoized annotation. In a language
  such as lkql that is purely functional, this will give a way for users to
  express/optimize computationally expensive things. Here is a simple example:

  .. code-block:: lkql

    @memoized
    fun fib(a) =
        if a == 0 then 0
        else (if a == 1 then 1
              else fib(a - 1) + fib (a - 2))

    print(fib(30))


Value Declaration
^^^^^^^^^^^^^^^^^

.. lkql_doc_class:: ValDecl

.. raw:: html
  :file: ../../lkql/build/railroad-diagrams/val_decl.svg

Declare a named value (often called a variable or constant in other languages):

.. code-block:: lkql

  val a = 12 + 15

Note that the value is immutable.

Docstrings
^^^^^^^^^^

Declarations can have assorted docstrings. They're part of the AST and are
directly attached to the declaration:

.. code-block:: lkql

  # Docstrings

  fun make_closure(closure_var) =
  |" Make a function that will capture ``closure_var`` and return the sum of
  |" it plus its first argument
  {
      fun use_closure(x) = closure_var + x;
      use_closure
  }

  |" Function that will add 12 to its first argument
  val adder = make_closure(12)

  print(make_closure(12))



Expressions
-----------


Block Expression
^^^^^^^^^^^^^^^^

.. lkql_doc_class:: BlockExpr
.. lkql_doc_class:: BlockBodyDecl
.. lkql_doc_class:: BlockBodyExpr

.. raw:: html
  :file: ../../lkql/build/railroad-diagrams/block_expr.svg

The block expression is useful to declare temporary named values and execute
intermediate expressions. This can be useful to share the result of a
temporary calculation, to name an intermediate value to make the code more
readable, or to print debug values:

.. code-block:: lkql

  {
      val x = 40;
      val y = 2;
      print("DEBUG : " & (x + y).img);
      x + y
  }

As you can see in the example above, value declarations and intermediate
expressions are ended by semicolons. After the last one, you write the
block's result expression, without an ending semicolon.


Field Access
^^^^^^^^^^^^

.. lkql_doc_class:: DotAccess
.. lkql_doc_class:: Safe
.. lkql_doc_class:: UpperDotAccess

A field access returns the contents of a field. In the following example, we
get the content of the  ``f_type_expr`` syntax field on a node of type
``ObjectDecl``:

.. code-block:: lkql

  object_decl.f_type_expr

A regular field access on a nullable variable is illegal and leads to a runtime
error, which is why field access has a variant, which is called a "safe
access":

.. code-block:: lkql

  object_decl?.f_type_expr

The safe access will return null if the left hand side is null. This allows
users to chain accesses without having to checks for nulls at every step.

In the context of rewriting features usage, you may want to get a reference
to a field of a node.
You can access such references with a dot-access notation on node kinds:

.. code-block::

    val ref_to_f_child = MyNodeKind.f_child

Such values can be used when calling ``RewritingContext``'s methods.


Unwrap Expression
^^^^^^^^^^^^^^^^^

.. lkql_doc_class:: Unwrap

When you have a nullable object and you want to make it non nullable, you can
use the unwrap expression. This is useful after a chain of safe accesses/calls,
for example:

.. code-block:: lkql

  object_decl?.p_type_expr()?.p_designated_type_decl()!!

Unwrap will raise an error if the value is null.


Call Expression
^^^^^^^^^^^^^^^

.. lkql_doc_class:: FunCall
.. lkql_doc_class:: SelectorCall
.. lkql_doc_class:: Arg

.. raw:: html
  :file: ../../lkql/build/railroad-diagrams/fun_call.svg

LKQL values of the ``Function`` type can be invoked with the call expression:

.. code-block:: lkql

  fun add(a, b) = a + b

  val c = add(12, 15)
  val d = add(a=12, b=15)

Parameters can be passed via positional or named associations.

Calls have a "safe" variant, that will return ``null`` if the callee is null:

.. code-block:: lkql

  fun add(a, b) = a + b
  val fn = if true then null else add
  fn?(1, 2) # Returns null

Additionally, you can also call selectors via the call syntax. Selector calls
take only one argument, which is the starting point of the selector call chain:

.. code-block:: lkql

  children(select first AdaNode)

Constructor call
^^^^^^^^^^^^^^^^

.. lkql_doc_class:: ConstructorCall

.. raw:: html
  :file: ../../lkql/build/railroad-diagrams/constructor_call.svg

You can call node constructors to create new nodes possibly used for the
tree rewriting layer of LKQL. The result of a constructor call is a value
of the ``RewritingNode`` type.

.. code-block:: lkql

  val token_node = new BooleanLiteral("Hello!")
  val list_node = new SomeListNode(child_1, child_2)
  val composite_node = new CompositeNode(
      f_child_1=token_node,
      f_child_2=list_node
  )

As function calls, you can pass arguments via positional or named associations
for composite nodes. About token and list nodes, you may only pass arguments
through the positional format.

To know whether a node is a token, list or composite one, you may refer to the
Langkit specification of the language you're querying.


Indexing Expression
^^^^^^^^^^^^^^^^^^^

.. lkql_doc_class:: Indexing

Indexing expression allows the user to access elements of a ``Tuple``,
``List``, ``LazyList``, or ``Node``.

When using the indexing expression on a node value:

* for list nodes, it will access the different elements of the list
* for regular nodes, it will access children in lexical order

Here are some examples of indexing expressions:

.. code-block:: lkql

  # Indexing a tuple
  (1, 2, 3)[1]

  # Indexing a list
  list[1]

  # Indexing a node with an arbitrary index
  {
      val x = 2;
      node[x]
  }

Indexing also has a safe variant, that will return ``unit`` instead of raising
when an out of bound access is done:

.. code-block:: lkql

  val lst = [1, 2, 3]

  # This will display "()"
  print(lst?[5])


Comparison Expression
^^^^^^^^^^^^^^^^^^^^^

.. raw:: html
  :file: ../../lkql/build/railroad-diagrams/comp_expr.svg

Comparison expressions are used to compare an object to another object, or
pattern. All those constructions are evaluated as booleans.

Membership Expression
"""""""""""""""""""""

.. lkql_doc_class:: InClause

The membership expression verifies that a collection (``List``/``LazyList``)
contains the given value:

.. code-block:: lkql

  12 in list

Is Expression
"""""""""""""

.. lkql_doc_class:: IsClause

The ``is`` expression verifies if a value matches a given `pattern`_:

.. code-block:: lkql

  val a = select AdaNode
  val b = a[1] is ObjectDecl

Comparison Operators
""""""""""""""""""""

.. lkql_doc_class:: RelBinOp

The usual comparison operators are available:

.. code-block:: lkql

  12 < 15
  a == b
  b != c

Order dependent operators (``<``/``>``/...) are only usable on integers.


Object Literal
^^^^^^^^^^^^^^

.. lkql_doc_class:: ObjectLiteral
.. lkql_doc_class:: ObjectAssoc
.. lkql_doc_class:: AtObjectLiteral
.. lkql_doc_class:: AtObjectAssoc

.. raw:: html
  :file: ../../lkql/build/railroad-diagrams/objectlit.svg

An object literal is a literal representation of an object value:

.. code-block:: lkql

  # Object literal
  {a: 1, b: "foo", c: null, d: [1, 2, 3, 4]}

.. raw:: html
  :file: ../../lkql/build/railroad-diagrams/at_object_lit.svg

"@" preceded object literals are similar to standard object literal with an
empty list as default value for any key:

.. code-block:: lkql

  # At object literal
  @{a: 1, b, c: null, d}

  # Is similar to
  {a: 1, b: [], c: null, d: []}

Object keys may contain upper-case characters at declaration, but the LKQL
engine will lower them. This means that object keys are case-insensitive:

..  code-block:: lkql

  val o = {lower: "Hello", UPPER: "World"}

  # This will display "Hello World"
  print(o.lower & " " & o.upper)

Please note that objects are immutable.


List Literal
^^^^^^^^^^^^

.. lkql_doc_class:: ListLiteral

.. raw:: html
  :file: ../../lkql/build/railroad-diagrams/listlit.svg

A list literal is simply a literal representation of a list:

.. code-block:: lkql

  # Simple list literal
  [1, 2, 3, 4]

Lists being immutable, lists literals are the primary way to create new lists
from nothing, with `list comprehension`_ being the way to create new lists from
existing lists.


List Comprehension
^^^^^^^^^^^^^^^^^^

.. lkql_doc_class:: ListComprehension
.. lkql_doc_class:: ListCompAssoc

.. raw:: html
  :file: ../../lkql/build/railroad-diagrams/listcomp.svg

A list comprehension allows the user to create a new list by iterating on an
existing collection, applying a mapping operation, and eventually a filtering
logic:

.. code-block:: lkql

  # Simple list comprehension that'll double every number in int_list if it
  # is prime
  [a * 2 for a in int_list if is_prime(a)]

  # Complex example interleaving two collections
  val subtypes = select SubtypeIndication
  val objects = select ObjectDecl
  print(
      [
          o.image & " " & st.image
          for o in objects, st in subtypes
          if (o.image & " " & st.image).length != 64
      ].to_list
  )

A list comprehension is a basic language construct, that, since LKQL is purely
functional, replaces traditional for loops. A list comprehension expression
returns a value of the ``LazyList`` type, meaning that elements in the result
aren't computed until queried:

.. code-block:: lkql

  val lazy = [a * 2 for a in int_list if is_prime(a)]

  # This will display "LazyList"
  print(lazy)

  # To display all elements of a lazy list, you have to convert it to a list
  print(lazy.to_list)


If Expression
^^^^^^^^^^^^^

.. lkql_doc_class:: CondExpr

.. raw:: html
  :file: ../../lkql/build/railroad-diagrams/if_then_else.svg

If expressions are traditional conditional expressions composed of a condition,
an expression executed when the condition is true, and and expression executed
when the condition is false:

.. code-block:: lkql

  # No parentheses required
  val x = if b < 12 then c() else d()

The ``else`` branch is optional and its default value is ``true``, this can
be useful to express an implication logic:

.. code-block:: lkql

  # Without "else" expression
  val y = if b < 12 then a == 0


Match Expression
^^^^^^^^^^^^^^^^

.. lkql_doc_class:: Match
.. lkql_doc_class:: MatchArm

.. raw:: html
  :file: ../../lkql/build/railroad-diagrams/match_expr.svg

This expression is a pattern matching expression, and reuses the same patterns
as the query part of the language. Matchers will be evaluated in order against
the match's target expression. The first matcher to match the object will
trigger the evaluation of the associated expression in the match arm:

.. code-block:: lkql

  match nodes[1]
  | ObjectDecl(p_has_aliased(): aliased @ *) => aliased
  | ParamSpec(p_has_aliased(): aliased @ *) => aliased
  | * => false

.. note::

  For the moment, there is no static check that the matcher is complete. A
  match expression where no arm has matched will raise an exception at runtime.


Tuple Literal
^^^^^^^^^^^^^

.. lkql_doc_class:: Tuple

.. raw:: html
  :file: ../../lkql/build/railroad-diagrams/tuple_expr.svg

The tuple literal is used to create a value of the ``Tuple`` composite type:

.. code-block:: lkql

  val t = (1, 2)
  val tt = ("hello", "world")
  val ttt = (t[1], tt[1])
  print(t)
  print(tt)
  print(ttt)

Tuples are useful as function return values, or to aggregate data, since LKQL
doesn't have structs yet.


Anonymous Function
^^^^^^^^^^^^^^^^^^

.. lkql_doc_class:: BaseFunction

.. raw:: html
  :file: ../../lkql/build/railroad-diagrams/anonymous_function.svg

LKQL supports first class functions, and anonymous functions expressions (or
lambdas). Thus, you can create anonymous functional values:

.. code-block:: lkql

  fun mul_y(y) = (x) => x * y
  val mul_2 = mul_y (2)
  val four = mul_2 (2)


Literals and Operators
^^^^^^^^^^^^^^^^^^^^^^

.. lkql_doc_class:: Literal
.. lkql_doc_class:: SubBlockLiteral
.. lkql_doc_class:: ArithBinOp
.. lkql_doc_class:: UnOp

LKQL has literals for booleans, integers, strings, unit, and null values:

.. code-block:: lkql

  val a = true     # Boolean
  val b = 12       # Integer
  val c = "hello"  # String
  val d = ()       # Unit
  val e = null     # Null

.. note::

  The LKQL ``null`` literal is used to represent a null node value, thus, it is
  different from the ``()`` (``unit``) value.

LKQL has multi-line string literals, called block-strings but they're a bit
different than in Python or other languages:

.. code-block:: lkql

  val a = |" Hello
          |" This is a multi line string
          |" Bue

.. note::

  The first character after the ``"`` should be a whitespace. This is
  not enforced at parse-time but at run-time, so ``|"hello`` is still a
  syntactically valid block-string, but will raise an error when evaluated.

LKQL has a few built-in operators available:

- Basic arithmetic operators on integers

.. code-block:: lkql

  val calc = a + 2 * 3 / 4 == b
  val smaller_or_eq = a <= b
  val greater_or_eq = b >= c

- Basic relational operators on booleans

.. code-block:: lkql

  true and false or (a == b) and (not c)

- String and list concatenation

.. code-block:: lkql

  "Hello " & name

.. code-block:: lkql

  [1, 2, 3] & [4, 5, 6]


Module Importation
^^^^^^^^^^^^^^^^^^

.. lkql_doc_class:: Import

LKQL has a very simple module system. Basically every file in LKQL is a module,
and you can import modules from other files with the ``import`` clause. When
importing a module, you are associating its name to the namespace produced by
the evaluation of its source (all declarations in its top-level):

.. code-block:: lkql

  # foo.lkql
  fun bar() = 12

  # bar.lkql
  import foo

  print(foo.bar())

LKQL will search for files:

1. That are in the same directory as the current file
2. That are in the ``LKQL_PATH`` environment variable

.. note::

  There is no way to create hierarchies of modules for now, only flat modules
  are supported.



Query Language Subset
=====================

The query language subset is mainly composed of three language constructs:
patterns, queries and selectors.

Patterns allow the user to express filtering logic on trees and graphs, akin to
what regular expressions allow for strings.

A lot of the ideas behind patterns are similar to ideas in
`XPath <https://developer.mozilla.org/fr/docs/Web/XPath>`_,
or even in
`CSS selectors <https://developer.mozilla.org/en-US/docs/Glossary/CSS_Selector>`_

However, unlike in CSS or xpath, a pattern is just the filtering logic, not the
traversal, even though filtering might contain sub traversals via selectors.

Here is a very simple example of a `query expression`_, that will select object
declarations that have the aliased qualifier:

.. code-block:: lkql

  # Queries are expressions, so their result can be stored in a named value
  val a = select ObjectDecl(p_has_aliased(): true)

This will query every source file in the LKQL context, filter their nodes
according to the provided `pattern`_, and return the ``List`` containing all
nodes matching the pattern.

Finally, selectors are a way to express "traversal" logic on the node graph.
Syntactic nodes, when explored through their syntactic children, form a tree.
However:

* There are different ways to traverse this tree (for example, you can explore
  the parents starting from a node)

* There are non syntactic ways to explore nodes, for example using semantic
  properties such as going from references to their declarations, or going up
  the tree of base types for a given tagged type.

All those traversals, including the most simple built-in one, use what is
called selectors in LKQL. Those are a way to specify a traversal, which will
return a ``LazyList`` of nodes as a result. Here is an example of a selector
that will go up the parent chain:

.. code-block:: lkql

  selector parent
  | AdaNode => rec(*this.parent, this)
  | *       => ()

Read the `Selector Declaration`_ section for more information about selectors.



Query Expression
----------------

.. lkql_doc_class:: Query
.. lkql_doc_class:: QueryKind

.. raw:: html
  :file: ../../lkql/build/railroad-diagrams/query.svg

The query expression is extremely simple, and most of the complexity lies in
the upcoming sections about patterns.

A query traverses one or several trees, from one or several root nodes,
applying the pattern on every node, and then returns a ``List`` containing all
nodes that matched the pattern:

.. code-block:: lkql

  # Will select all non null nodes
  select AdaNode

By default the query's roots are implicit and set by the context. However, you
can specify them with the ``from`` keyword, followed either by a ``Node``
value, or a ``List`` of nodes:

.. code-block:: lkql

  # Select all non null nodes starting from node a
  from a select AdaNode

  # Select all non null nodes starting from all nodes in list
  from [a, b, c] select AdaNode

You can also run a query that will only select the first element, this can be
useful to avoid visiting all the parsing tree:

.. code-block:: lkql

  # Select first basic declaration
  select first BasicDecl


Specifying the selector
^^^^^^^^^^^^^^^^^^^^^^^

By default, queries traverse the syntactic tree from the root node to leaves.
This behavior is equivalent to going through the nodes returned via the
``children`` built-in selector (read the `Built-in Selectors`_ section for more
information).

But you can also specify which selector you're using to do the traversal, and
even use your custom defined selectors. This is done using the ``through``
keyword:

.. code-block:: lkql

  # Selects the parents of the first basic declaration
  from (select first BasicDecl) through parent select *

.. attention::

  There is a special case for Ada, where you can specify ``follow_generics``
  as a selector name, even though ``follow_generics`` is not a selector. This
  allows traversal of the tree going through instantiated generic trees, but is
  directly hard-coded into the engine for performance reasons.

  .. code-block:: lkql

    # Selects all nodes following generic instantiations
    through follow_generics select *



Pattern
-------

.. lkql_doc_class:: ValuePattern

.. raw:: html
  :file: ../../lkql/build/railroad-diagrams/pattern.svg

.. raw:: html
  :file: ../../lkql/build/railroad-diagrams/value_pattern.svg

Patterns are by far the most complex part of the query language subset, but at
its core, the concept of a pattern is very simple:
it is a construction that you will match against a value. LKQL will check that
the value matches the pattern, and produce ``true`` if it does. In the context
of a query, that will add the value to the result of the query.

.. admonition:: todo

  Patterns are not yet expressions, but they certainly could be and should
  be, so we're planning on improving that at a later stage.


Node patterns
^^^^^^^^^^^^^

Simple Node Patterns
""""""""""""""""""""

.. lkql_doc_class:: NodeKindPattern

Matching one or many node kinds is the simplest atom for node patterns. It can
be either:

* a node kind name, matching all nodes of this kind
* an `or pattern`_, matching on multiple node kinds
* a `wildcard pattern`_, matching on all node kinds

.. code-block:: lkql

  select *                           # Will select every node
  select BasicDecl                   # Will select every basic declaration
  select (ObjectDecl | BaseTypeDecl) # Will select every object and type declaration

In a more complex form, those can have sub-patterns in an optional part between
parentheses, which brings us to the next section.

Nested Sub Patterns
"""""""""""""""""""

.. lkql_doc_class:: NodePatternDetail
.. lkql_doc_class:: PatternDetailDelimiter

.. raw:: html
    :file: ../../lkql/build/railroad-diagrams/pattern_arg.svg

Inside the optional parentheses of node patterns, the user can add sub-patterns
that will help refine the query. Those patterns can be of three different
kinds:

Selector Predicate
""""""""""""""""""

A selector predicate is a sub-pattern that allows you to run a sub-query and to
match its results:

.. code-block:: lkql

  select Body(any children: ForLoopStmt)

The quantifier part (``any`` in the previous example) can be either ``any`` or
``all``, which will alter how the sub-pattern matches:

* ``all`` will match only if all nodes returned by the selector match the
  condition
* ``any`` will match as soon as at least one child matches the condition

Any of the `built-in selectors`_ can be used, or even custom selectors.

.. note::

  All selectors have three optional parameters that allows controlling the
  depth of the traversal, ``depth``, ``max_depth`` and ``min_depth``. Read
  `Selector Declaration`_ section for more information.

Field Predicate
"""""""""""""""

A field predicate is a sub-pattern that allows you to match a sub-pattern
against a specific field in the parent object. We have already seen such a
construct in the introduction, and it's one of the simplest kind of patterns:

.. code-block:: lkql

  select ObjectDecl(f_default_expr: IntLiteral)

Property Call Predicate
"""""""""""""""""""""""

A property predicate is very similar to a field predicate, except that a
property of the node is called, instead of a field accessed. Syntactically,
this is denoted by the parentheses after the property name:

.. code-block:: lkql

  select BaseId(p_referenced_decl(): ObjectDecl)


Regular Values Patterns
^^^^^^^^^^^^^^^^^^^^^^^

Not only nodes can be matched in LKQL: Any value can be matched via a pattern,
including basic and composite data types.

Integer Pattern
"""""""""""""""

.. lkql_doc_class:: IntegerPattern

.. raw:: html
  :file: ../../lkql/build/railroad-diagrams/integer_pattern.svg

You can match simple integer values with this pattern:

.. code-block:: lkql

  v is 12

Bool Pattern
""""""""""""

.. lkql_doc_class:: BoolPattern

.. raw:: html
  :file: ../../lkql/build/railroad-diagrams/bool_pattern.svg

You can match simple boolean values with this pattern:

.. code-block:: lkql

  v is true

Regex Pattern
"""""""""""""

.. lkql_doc_class:: RegexPattern

.. raw:: html
  :file: ../../lkql/build/railroad-diagrams/regex_pattern.svg

You can match simple string values with this pattern, but you can also do more
complicated matching based on regular expressions:

.. code-block:: lkql

  v is "hello"
  v is "hello.*?world"

Tuple Pattern
"""""""""""""

.. lkql_doc_class:: TuplePattern

.. raw:: html
  :file: ../../lkql/build/railroad-diagrams/tuple_pattern.svg

You can match tuple values with this pattern, elements being matched with
component patterns:

.. code-block:: lkql

  match i
  | (1, 2, 3) => print("un, dos, tres")
  | *         => print("un pasito adelante maria")

  match i
  | (1, a@*, b@*, 4) => { print(a); print(b) }

List Pattern
""""""""""""

.. lkql_doc_class:: ListPattern
.. lkql_doc_class:: SplatPattern

.. raw:: html
  :file: ../../lkql/build/railroad-diagrams/list_pattern.svg

You can match list values with this pattern, destructuring them and matching
their elements against arbitrary value patterns:

.. code-block:: lkql

  match lst
  | [1, 2, 3]   => "[1, 2, 3]"
  | [1, a@*, 3] => "[1, a@*, 3], with a = " & img(a)

You can use the `splat pattern`_ at the end of a list pattern to match
remaining elements:

.. code-block:: lkql

  match lst
  | [11, 12, ...] => "[11, 12, ...]"
  | [1, c@...]    => "[1, c@...] with b = " & img(b) & " & c = " & img(c)
  | [...]         => "Any list"

Object Pattern
""""""""""""""

.. lkql_doc_class:: ObjectPattern
.. lkql_doc_class:: ObjectPatternAssoc
.. lkql_doc_class:: SplatPattern

.. raw:: html
  :file: ../../lkql/build/railroad-diagrams/object_pattern.svg

You can match object values with this pattern, associating each object key
with an arbitrary value pattern:

.. code-block:: lkql

  match obj
  | {a: 12}  => "{a: 12}"
  | {a: a@*} => "Any object with an a key. Bind the result to a"

You can use the "splat" pattern anywhere in an object pattern to match
remaining elements:

.. code-block:: lkql

  match obj
  | {a@..., b: "hello"} => "Bind keys that are not b to var a"
  | {a@...}             => "Bind all the object to a"


Special and Composite Patterns
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Null Pattern
""""""""""""

.. lkql_doc_class:: NullPattern

You can match all null nodes with this pattern:

.. code-block:: lkql

  match node
  | BasicDecl => "A BasicDecl node"
  | null      => "Node is null!"

Wildcard Pattern
""""""""""""""""

.. lkql_doc_class:: UniversalPattern

You can match all values with this pattern, it will always return ``true``:

.. code-block:: lkql

  match any_val
  | BasicDecl => "A BasicDecl node"
  | *         => "Any other value"

Splat Pattern
"""""""""""""

.. lkql_doc_class:: SplatPattern

This pattern is used inside `List Pattern`_ and `Object Pattern`_ as a pattern
to match all remaining values, collecting them in a collection of the same type
as it is used in:

.. code-block:: lkql

  match v
  | [1, rem@...]    => "A list with 1 as first element followed by " & img(rem)
  | {a: 1, rem@...} => "An object with a=1 and " & img(rem)

Not Pattern
"""""""""""

.. lkql_doc_class:: NotPattern

You can use this pattern to negate another one:

.. code-block:: lkql

  match v
  | not BasicDecl => "Everything except a BasicDecl node"
  | *             => "A BasicDecl node"

Or Pattern
""""""""""

.. lkql_doc_class:: OrPattern

You can use this pattern to combine any number of other patterns, and match any
value matching one of those:

.. code-block:: lkql

  match v
  | (BasicDecl | 1) => "A BasicDecl node or 1"
  | *               => "Any other value"


Filtered Patterns and Binding Patterns
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. lkql_doc_class:: FilteredPattern
.. lkql_doc_class:: BindingPattern

While you can express a lot of things via the regular pattern syntax mentioned
above, sometimes it is necessary to be able to express an arbitrary boolean
condition in patterns; this is done via the ``when`` clause:

.. code-block:: lkql

  select BasicDecl when bool_condition

However, in order to be able to express conditions on the currently matched
objects, or arbitrary objects in the query, naming those objects is necessary.
This is done via binding patterns:

.. code-block:: lkql

  select b @ BaseId # Same as "select BaseId", but now every BaseId object
                    # that is matched has a name that can be used in the whole
                    # pattern clause.

  # Example usage:
  val a = select first BasicDecl
  select b @ BaseId when b.p_referenced_decl() == a



Selector Declaration
--------------------

.. lkql_doc_class:: SelectorDecl
.. lkql_doc_class:: RecExpr
.. lkql_doc_class:: SelectorArm
.. lkql_doc_class:: Unpack

.. raw:: html
  :file: ../../lkql/build/railroad-diagrams/selector_decl.svg

.. raw:: html
  :file: ../../lkql/build/railroad-diagrams/selector_arm.svg

Selectors are a special form of functions that return a ``LasyList`` of values.
They're widely used in the query subset of LKQL, allowing the easy expression
of traversal blueprints.

For example, by default, a `query expression`_ explores the tree via the
``children`` built-in selector.

While you can't add parameters to the definition of a selector, selector calls
(a `call expression`_ or a `selector predicate`_) can take three optional
arguments that allows the control of depth:

* ``min_depth`` allows you to filter nodes for which the traversal depth is
  lower than a certain value

* ``max_depth`` allows you to filter nodes for which the traversal depth is
  higher than a certain value

* ``depth`` allows you to only receive nodes that are exactly at the given
  traversal depth

Here are some examples of calling selectors with those parameters:

.. code-block:: lkql

  # Calling a selectors directly
  val c = children(node, depth=3)

  # Calling a selector in a nested sub-pattern
  select AdaNode(any children(min_depth=3): BasicDecl)

You've already seen selectors used in previous sections, and, most of the time,
you might not need to define your own, but in case you need to, here is how
they work.


Defining a Selector
^^^^^^^^^^^^^^^^^^^

A selector is a recursive function. In the body of the selector, there is a
binding from ``this`` to the current node. A selector has an implicit top level
`match expression`_ matching on ``this``.

In the branch of a selector, you can express whatever computation you want for
the current node. **There is a high-level requirement though, which is that the
expression returned by a selector branch must be a** ``RecExpr`` **, which can
be created via the call to the** ``rec`` **built-in operation.**

The ``rec`` built-in operation looks like a function call.

.. raw:: html
  :file: ../../lkql/build/railroad-diagrams/selector_expr.svg

It takes one or two expressions, which can be prefixed by the splat operator
``*``.

* The first expression represents what has to be added to the recurse list
  (either an item, or a list of items, if prefixed by `*`). The recurse list is
  the list of items on which the selector will be called next. Items are added
  at the end of the list

* The second expression represents what has to be added to the result list
  (either an item, or a list of items, if prefixed by `*`). The result list is
  the list of items that will be yielded, piece-by-piece, to the user.

* You can pass only one expression, in which case it is used both for the
  result list and for the recurse list.

.. attention::

  Please note that selector call results are ``LasyList``, thus, their elements
  are computed on demand (when accessed).

Here is for example how the ``super_types`` selector is expressed in LKQL:

.. code-block:: lkql

  selector super_types
  | BaseTypeDecl => rec(*this.p_base_types())
  | *            => ()

While selectors are in the vast majority of cases used to express tree
traversals of graph of nodes, you can use selectors to generate or process more
general sequences:

.. code-block:: lkql

  selector infinite_sequence
  |" Infinite sequence generator
  | nb => rec(
      nb + 1, # Recurse with value nb + 1
      nb # Add nb to the result list
  )

  fun my_map(lst, fn) =
  |" User defined map function. Uses an inner selector to return a lazy
  |" iterator result
  {
      selector internal
      | idx => rec(
          idx + 1,     # Recurse with value idx + 1
          fn(lst[idx]) # Add the result of calling fn on list[idx] to the result list
      );

      internal(1)
  }

  val mpd = my_map(infinite_sequence(0), (x) => x * 4)
  print(mpd)
  print(mpd[51])

.. attention::

  The user interface for selectors is not optimal at the moment, so we might
  change it again soon.


Built-in Selectors
^^^^^^^^^^^^^^^^^^

The built-in selectors are:

* ``parent``: parent nodes
* ``children``: child nodes
* ``prev_siblings``: sibling nodes that are before the current node
* ``next_siblings``: sibling nodes that are after the current node
* ``super_types``: if the current node is a type, then all its parent types



..
  * Operators need not be documented, since they're documented as part of the
    BinOp hierarchy
  * It's not necessary to document identifiers AFAICT

.. lkql_doc_class:: Op
.. lkql_doc_class:: Identifier



Language changes
================

Under this section, we'll document language changes chronologically, and
categorize them by AdaCore GNATcheck release.

.. note::

  Changes marked as "**breaking**" indicates that your LKQL code bases need to
  be migrated when moving to the referred GNATcheck version. The LKQL
  executable provides a sub-command named ``refactor`` to help you doing this
  (run ``lkql refactor --help`` for more information).


25.0
----


Conditional expression alternatives are now optional
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Now you can write a conditional expression without providing any alternative
expression. This way, if the condition is evaluated as ``true``, then the
consequence expression is evaluated, else the ``true`` value is returned.
You can use this feature to express logical implication when performing
boolean operation, example:

.. code-block:: lkql

  if node.p_has_something() then node.p_check_something_else()


Syntax of pattern details (breaking)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Pattern details were specified with the syntax ``<left_part> is <pattern>``,
and are now specified with the syntax ``<left_part>: <pattern>``.


Syntax of selectors recursion definition (breaking)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The syntax for defining a recursion in selectors has completely changed. The
old `rec` and `skip` keywords have been replaced by a single `rec` construct
that allows to specify what elements will be recursed upon, and what elements
will be yielded by the selector:

.. code-block:: lkql

  selector parent
  | AdaNode => rec(*this.parent, this)
  #                ^ Add parent to the recurse list
  #                ^             ^ Add this to the return list
  | *       => ()

.. warning::

  This syntax is more general than the previous one, but is still not optimal,
  and might change again in a further release. Please take that into account
  when using selectors in your own code.

More details in the `Selector Declaration`_ section.


Or patterns syntax (breaking)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Or patterns were defined with the ``<pattern> or <pattern>`` syntax, and are
now defined with the ``<pattern> | <pattern>`` syntax.


Binding patterns without value pattern
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Patterns binding any value to a name can simply be expressed with a binding
name now:

.. code-block:: lkql

  match d
  | BasicDecl(p_doc(): doc) => print(doc)


More patterns
^^^^^^^^^^^^^

So far, only node values had corresponding patterns to match them. Now,
patterns can be used to match other values:

.. code-block:: lkql

  v is 12
  v is true
  v is "hello"
  v is "hello.*?world"

  match i
  | (1, 2, 3) => print("un, dos, tres")
  | *         => print("un pasito adelante maria")

  match i
  | (1, a@*, b@*, 4) => { print(a); print(b) }

  match lst
  | [1, 2, 3]   => "[1, 2, 3]"
  | [1, a@*, 3] => "[1, a@*, 3], with a = " & img(a)

  match lst
  | [11, 12, ...] => "[11, 12, ...]"
  | [1, c@...]    => "[1, c@...] with b = " & img(b) & " & c = " & img(c)
  | [...]         => "Any list"

  match obj
  | {a: 12}  => "{a: 12}"
  | {a: a@*} => "Any object with an a key. Bind the result to a"

  match obj
  | {a@..., b: "hello"} => "Bind keys that are not b to var a"
  | {a@...}             => "Bind all the object to a"

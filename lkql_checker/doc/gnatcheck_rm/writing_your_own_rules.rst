.. _Writing_Your_Own_Rules:

**********************
Writing Your Own Rules
**********************

All the predefined rules in GNATcheck are implemented via a pattern matching
language called LQKL (LangKit Query Language) which is a functional,
turing complete language, and provides as an advanced usage the ability to
write an infinite number of custom checks.

The general description of this language can be found in the
:ref:`LKQL_language_reference`.
The APIs available in LKQL are described in the :ref:`LKQL_API_doc`.

This chapter gives some additional information on how to use this language to
create your own rules and checkers.

How to write checks
-------------------

Checks are written in the LKQL language, and put either in the predefined
:file:`share/lkql` directory or under any other directory specified
via the ``--rules-dir`` switch. Each :file:`.lkql` file found in these directories
will be loaded by gnatcheck and represents a distinct checker (or a set of helper
functions). The naming convention of the checkers is ``lowercase_with_underscores``.

Here is a simple checker example, that will just flag every body:

.. code-block:: lkql

   @check
   fun bodies(node) = node is Body

Adding this source in the ``bodies.lkql`` file in a directory listed via
``--rules-dir`` will add a check to GNATcheck dynamically, without the need to
modify GNATcheck itself.

Boolean checks
~~~~~~~~~~~~~~

Boolean checks are functions that take a node and return a boolean and are
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


Unit checks
~~~~~~~~~~~

Unit checks are functions that take an analysis unit and return a list of
objects containing a message and a location. They're meant to be ultimately
flexible, and fullfill the needs that boolean checks can't fullfill, as for example:

- Customizing messages.
- Having a non 1 to 1 relationship between messages and nodes.
- Having token based checkers.

The returned objects must have two keys:

- ``message``: Contains the message to be displayed.
- ``loc``: Either a node or a token, used as the source location for the error
  message.

These functions are marked with the ``@unit_check`` decorator:

.. code-block:: lkql

   @unit_check
   fun goto(unit) = [
      {message: "goto statement at line " &
                img(node.token_start().start_line),
       loc: node}
      for node in (from unit.root select GotoStmt)
   ]

Checks arguments
~~~~~~~~~~~~~~~~

Checks can take different optional arguments:

* `message`: The custom message that is to be shown for a given check on the
  command line. Defaults to the name of the check if not specified.

* `help`: The help message that is to be shown via `gnatcheck -h`. Defaults to
  message if not specified.

* `follow_generic_instantiations`: Whether to follow generic instantiations
  during the traversal of given Ada units. If `true`, generic instantiations
  will be traversed in instantiated form. Defaults to `false`.

* `category`, `subcategory`: The category (and subcategory) associated with this
  check, used by gnatcheck as part of its `-hx` output. Defaults to `Misc`.

* remediation: A string with the following possible values:

  * EASY
  * MEDIUM
  * MAJOR

  Used by `gnatcheck -h` and by the Sonar integration to compute technical debt.
  Defaults to `MEDIUM`.

Here is an example check:

.. code-block:: lkql

    @check(message="integer object declaration", follow_generic_instantiations=true)
    fun int_obj_decl(node) =
        |" Will flag object declarations for which the type is the standard
        |" ``Integer`` type
        node is o@ObjectDecl(
            p_type_expression() is SubtypeIndication(
                p_designated_type_decl() is t@* when t == o.p_std_entity("Integer")))


LKQL checker
============

LKQL checker is a prototype checker/linter based on LKQL and Libadalang. It is
mainly used to test GNATcheck rules/checks and prototypes new features. You can
use it by running the LKQL CLI with the ``check`` subcommand.

How to write checks
-------------------

Checks are written in the LKQL language, and put in the
```share/lkql`` <share/lkql>`__ directory. Each ``.lkql`` file
represents a distinct checker. The naming convention of the checkers is
``lowercase_with_underscores``.

Here is a simple checker example, that will just flag every body.

.. code-block:: lkql

   @check
   fun bodies(node) = node is Body

Adding this source in the ``body.lkql`` file in the ``share/lkql``
directory will add a check to LKQL checker dynamically, without need to
recompile LKQL checker.

Boolean checks
~~~~~~~~~~~~~~

Boolean checks are functions that take a node and return a boolean. They
usually contain an ``is`` pattern match as the main expression:

.. code-block:: lkql

   @check
   fun goto(node) = node is GotoStmt

But are not limited to this, and can contain arbitrary expressions as
long as they return a boolean.

Unit checks
~~~~~~~~~~~

Unit checks are functions that take an analysis unit and return a list of
objects containing a message and a location. They're meant to be ultimately
flexible, and fulfill the needs that boolean checks can't fulfill, as for example:

- Customizing messages.
- Having a non 1 to 1 relationship between messages and nodes.
- Having token based checkers.

The returned objects must have two keys:

- ``message``: Contains the message to be displayed.
- ``loc``: Either a node or a token, used as the source location for the error
           message.

``@unit_check``:

.. code-block:: lkql

   @unit_check
   fun goto(unit) = [
      {message: "goto statement", loc: node}
      for node in (from unit.root select GotoStmt)
   ]

Checks arguments
~~~~~~~~~~~~~~~~

Checks can take different optional arguments:

* `message`: The custom message that is to be shown for a given check on the
  command line.

* `follow_generic_instantiations`: Whether to follow generic instantiations
  during the traversal of given Ada units. If `true`, generic instantiations
  will be traversed in instantiated form.

* `auto_fix`: A function value which is applied on all node that violates the
  associated checker function. This function takes 2 parameters: a ``Node`` and
  a ``RewritingContext``, the execution of the function should provide a fix
  for the violated rule on the given node using the rewriting context.
  This argument is only available on **Boolean checks**.

Here is an example check:

.. code-block:: lkql

   fun replace_by_null(node, rewriting_ctx) =
      rewriting_ctx.replace(node, new NullStmt())

   @check(
      message="integer object declaration",
      follow_generic_instantiations=true,
      auto_fix=replace_by_null
   )
   fun int_obj_decl(node) =
      |" Will flag object declarations for which the type is the standard
      |" ``Integer`` type.
      |" This check will also replace the object declaration by a "null;"
      |" statement as a fix.
      node is o @ ObjectDecl(
         p_type_expression(): SubtypeIndication(
            p_designated_type_decl(): t @ * when t == o.p_std_entity("Integer")
         )
      )

Running
-------

Running the checker will by default run all the checks.

.. code-block:: sh

   lkql check [-P project | list of files]

If you want to run a specific check, you can add the name of the check
after ``-r``:

.. code-block:: sh

   lkql check [-P project | list of files] -r rule_name

There is no way to list checks from the command line for now, just
explore the ``share/lkql`` directory.

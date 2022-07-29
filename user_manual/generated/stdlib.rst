API Doc For Module stdlib
--------------------------------

LKQL stdlib module

This module contains functions that are shared accross lkql_checker rules.
These functions may be moved in the future in Libadalang or LKQL's builtin
library.

.. function:: children_no_nested

    Return all children nodes starting from a base subprogram body, but not
    entering in nested bodies.

.. function:: enclosing_block(n)

    Return the first DeclBlock enclosing n if any, null otherwise

.. function:: enclosing_body(n)

    Return the first BodyNode enclosing n if any, null otherwise

.. function:: enclosing_package(n)

    Return the first BasePackageDecl or PackageBody enclosing n if any,
    null otherwise

.. function:: find_comment(token, name)

    Return true if a comment token immediately following the previous
    "begin" keyword is found and containing only the package name.

.. function:: first_non_blank(s, ind=1)

    Return the index of the first non blank character of s, starting at ind

.. function:: get_parameter(params, actual)

    Given a List[ParamActual], return the parameter corresponding to
    actual, null if actual is not found.

.. function:: get_subp_body(node)

    Return the SubpBody or TaskBody corresponding to node, if any, null
    otherwise.

.. function:: has_interfaces(n)

    Return true if ``n`` is an interface or implements some interfaces

.. function:: has_local_scope(n)

    Return ``true`` if ``n`` is enclosed in a local scope

.. function:: in_generic_template(n)

    Return true if ``n`` is declared as part of a generic template (spec
    or body). Return false otherwise, including inside a generic
    instantiation.

.. function:: is_assert_aspect(s)

    Return ``true`` if the string ``s`` is the name of an assert aspect

.. function:: is_assert_pragma(s)

    Return ``true`` if the string ``s`` is the name of an assert pragma

.. function:: is_classwide_type(t)

    Return true if t is a classwide TypeDecl.

.. function:: is_constant_object(node)

    Return true is node represents a constant object, false otherwise

.. function:: is_constructor(spec)

    Return true if spec is a subprogram spec of a constructor, that is, has
    a controlling result and no controlling parameter.

.. function:: is_controlling_param_type(t, spec)

    Return true if `t` is a TypeExpr corresponding to a controlling
    parameter of the subprogram spec `spec`.

.. function:: is_in_library_unit_body(o)

    Return ``true`` if ``o`` is located in a library unit body

.. function:: is_in_package_scope(o)

    Return ``true`` if ``o`` is immediately in the scope of a package spec,
    body or generic package.

.. function:: is_limited_type(type)

    Return `true` if type is a limited type

.. function:: is_local_object(o)

    Return ``true`` if ``o`` represents a local ``ObjectDecl`` or ``ParamSpec``

.. function:: is_predefined_op(op)

    Return true if op is a predefined operator

.. function:: is_predefined_type(n)

    Return true if n is the name of a type declared in a predefined package
    spec.

.. function:: is_program_unit(n)

    Return ``true`` if ``n`` is a program unit spec, body or stub

.. function:: is_standard_boolean(n)

    Return true if the root type of n is Standard.Boolean

.. function:: is_standard_numeric(n)

    Return ``true`` if ``n`` is the name of a numeric type or subtype in Standard

.. function:: list_of_units()

    Return a (cached) list of all known units

.. function:: max(x, y)

    Return the max value between x and y

.. function:: next_non_blank_token_line(token)

    Return the start line of the next non blank token, or the next line for
    a comment, or 0 if none.

.. function:: number_of_values(type)

    Return the number of values covered by a given BaseTypeDecl, -1 if
    this value cannot be determined.

.. function:: param_pos(n, pos: int = 0)

    Return the position of node ``n`` in its current list of siblings

.. function:: previous_non_blank_token_line(token)

    Return the end line of the previous non blank token, or the previous
    line for a comment, or 0 if none.

.. function:: range_values(left, right)

    Return the number of values covered between left and right expressions,
    -1 if it cannot be determined.

.. function:: semantic_parent

    Return all semantic parent nodes starting from a given node.

.. function:: sloc_image(node)

    Return a string with basename:line corresponding to node's sloc

.. function:: strip_conversions(node)

    Strip ParenExpr, QualExpr and type conversions

.. function:: ultimate_alias(name, all_nodes=true, strip_component=false)

    Return the ultimately designated `ObjectDecl`, going through renamings
    This will not go through generic instantiations. If all_nodes is true,
    consider all kinds of nodes, otherwise consider only BaseId and
    DottedName. If strip_component is true, go to the prefix when
    encountering a component, otherwise stop at the ComponentDecl.

.. function:: ultimate_exception_alias(name)

    Return the ultimately designated ``ExceptionDecl``, going through renamings

.. function:: ultimate_prefix(n)

    Return n.f_prefix as long as n is a DottedName and designates a
    ComponentDecl, n otherwise.

.. function:: ultimate_subprogram_alias(name)

    Return the ultimately designated ``BasicSubpDecl``, going through renamings

.. function:: within_assert(node)

    Return ``true`` if ``node`` is part of an assertion-related pragma or
    aspect.

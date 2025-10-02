stdlib's API doc
----------------

Functions
^^^^^^^^^
.. function:: all(iterable)

    Return whether all elements in the given iterable are truthy

.. function:: any(iterable)

    Return whether at least one element in the given iterable is truthy

.. function:: children_no_nested(node)

    Return all children nodes starting from a base subprogram body, but not
    entering in nested bodies.

.. function:: closest_enclosing_generic(n)

    If ``n`` is part of a generic package or subprogram, whether it is
    instantiated or not, then return it.

.. function:: default_bit_order()

    Return the value of ``System.Default_Bit_Order`` if any ``with System``
    clause is found, null otherwise.

.. function:: depends_on_mutable_discriminant(component_decl)

    Given a `ComponentDecl`, return whether it depends on a mutable
    discriminant value coming from its parent record declaration.
    The component depends on a discriminant if it uses it in its subtype
    constraint or if it is a variant.

.. function:: enclosing_block(n)

    Return the first ``DeclBlock`` enclosing ``n`` if any, ``null``
    otherwise.

.. function:: enclosing_body(n)

    Return the first BodyNode enclosing n if any, null otherwise

.. function:: enclosing_package(n)

    Return the first BasePackageDecl or PackageBody enclosing n if any,
    null otherwise

.. function:: find_comment(token, name)

    Return true if a comment token immediately following the previous
    "begin" keyword is found and contains only the provided name.

.. function:: first_non_blank(s, ind=1)

    Return the index of the first non blank character of s, starting at ind

.. function:: full_root_type(t)

    Return the full view of the root type of ``t``, traversing subtypes,
    derivations and privacy.

.. function:: get_parameter(params, actual)

    Given a ``List[ParamActual]``, return the parameter corresponding to
    actual, null if actual is not found.

.. function:: get_subp_body(node)

    Return the SubpBody, TaskBody or ExprFunction corresponding to node,
    if any, null otherwise.

.. function:: has_interfaces(n)

    Return true if ``n`` is an interface or implements some interfaces

.. function:: has_local_scope(n)

    Return ``true`` if ``n`` is enclosed in a local scope

.. function:: has_non_default_sso(decl)

    Return true if ``decl`` has a ``Scalar_Storage_Order`` aspect whose
    value cannot be determined to be equal to
    ``System.Default_Storage_Order``.

.. function:: in_generic_instance(n)

    Return true if ``n`` is part of a generic instantiation.

.. function:: in_generic_template(n)

    Return true if ``n`` is declared as part of a generic template (spec
    or body). Return false otherwise, including inside a generic
    instantiation.

.. function:: is_assert_aspect(s)

    Return ``true`` if the string ``s`` is the name of an assert aspect

.. function:: is_assert_pragma(s)

    Return ``true`` if the string ``s`` is the name of an assert pragma

.. function:: is_by_copy(param)

    Return true if ``param`` (a ``ParamActual``) has a non aliased by-copy
    type

.. function:: is_by_ref(param)

    Get whether the provided parameter (a ``ParamActual``) type is a
    by-reference" type as defined in the reference manual at 6.2(4-9).

.. function:: is_classwide_type(t)

    Return true if t is a classwide TypeDecl.

.. function:: is_composite_type(decl)

    Given a BaseTypeDecl, returns whether the declared type is a composite
    Ada type (record, array, task or protected).

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

.. function:: is_negated_op(node)

    Return whether ``node`` is a "not" unary operation, returning a standard
    boolean, and having as operand a predefined RelationOp or UnOp with
    OpNeq as operator.

.. function:: is_predefined_op(op, follow_renamings=false)

    Return true if ``op`` is a predefined operator; ``op`` can be an Op or
    a CallExpr.

.. function:: is_predefined_type(n)

    Return true if ``n`` is the name of a type declared in a predefined
    package spec.

.. function:: is_program_unit(n)

    Return ``true`` if ``n`` is a program unit spec, body or stub

.. function:: is_standard_boolean(n)

    Return true if the root type of ``n`` is ``Standard.Boolean``.

.. function:: is_standard_false(node)

    Get whether the given node is a Name representing the standard False
    value.

.. function:: is_standard_numeric(n)

    Return ``true`` if ``n`` is the name of a numeric type or subtype in Standard

.. function:: is_standard_true(node)

    Get whether the given node is a Name representing the standard True
    literal.

.. function:: is_subject_to_predicate(decl)

    Return whether the provided declaration is subject to a dynamic or
    static predicate.

.. function:: is_tasking_construct(node)

    Returns whether the given node is a construct related to Ada tasking,
    in other words: All constructs described in the section 9 of Ada RM.

.. function:: is_unchecked_conversion(node)

    Return true if node represents an instantiation of the
    `Ada.Unchecked_Conversion` subprogram

.. function:: is_unchecked_deallocation(node)

    Return true if node represents an instantiation of the
    `Ada.Unchecked_Deallocation` subprogram

.. function:: list_of_units()

    Return a (cached) list of all known units

.. function:: max(x, y)

    Return the max value between x and y

.. function:: negate_op(node)

    Assumes that ``node`` is either a RelationOp or UnOp with the OpNot
    as operator. Returns the negated form of the operation as a rewriting
    node.
    Examples:
    ``negate_op("A = B") -> "A /= B"``
    ``negate_op("A > B") -> "A <= B"``
    ``negate_op("not A") -> "A"``

.. function:: next_non_blank_token_line(token)

    Return the start line of the next non blank token, or the next line for
    a comment, or 0 if none.

.. function:: number_of_values(type)

    Return the number of values covered by a given BaseTypeDecl, -1 if
    this value cannot be determined.

.. function:: param_pos(n, pos=0)

    Return the position of node ``n`` in its current list of siblings

.. function:: previous_non_blank_token_line(token)

    Return the end line of the previous non blank token, or the previous
    line for a comment, or 0 if none.

.. function:: propagate_exceptions(body)

    Return true if the given body may propagate an exception, namely if:
    - it has no exception handler with a ``when others`` choice;
    - or it has an exception handler containing a raise statement, or a call
    to ``Ada.Exception.Raise_Exception`` or
    ``Ada.Exception.Reraise_Occurrence``.

.. function:: range_values(left, right)

    Return the number of values covered between left and right expressions,
    -1 if it cannot be determined.

.. function:: sloc_image(node)

    Return a string with basename:line corresponding to node's sloc

.. function:: strip_conversions(node)

    Strip ``ParenExpr``, ``QualExpr`` and type conversions

.. function:: ultimate_alias(name, all_nodes=true, strip_component=false)

    Return the ultimately designated ``ObjectDecl``, going through renamings
    This will not go through generic instantiations. If all_nodes is true,
    consider all kinds of nodes, otherwise consider only ``BaseId`` and
    ``DottedName``. If ``strip_component`` is true, go to the prefix when
    encountering a component, otherwise stop at the ``ComponentDecl``.

.. function:: ultimate_designated_generic_subp(subp_inst)

    Given a node representing an instantiation of a generic subprogram,
    return that non-instantiated subprogram after resolving all renamings.

.. function:: ultimate_exception_alias(name)

    Return the ultimately designated ``ExceptionDecl``, going through renamings

.. function:: ultimate_generic_alias(name)

    Return the ultimately designated ``GenericDecl``, going through renamings

.. function:: ultimate_prefix(n)

    Return ``n.f_prefix`` as long as ``n`` is a ``DottedName`` and
    designates a ``ComponentDecl``, ``n`` otherwise.

.. function:: ultimate_subprogram_alias(name)

    Return the ultimately designated ``BasicSubpDecl``, going through renamings

.. function:: within_assert(node)

    Return ``true`` if ``node`` is part of an assertion-related pragma or
    aspect.

Selectors
^^^^^^^^^
.. function:: complete_super_types()

    Yields the chain of super types of the given type in their most complete
    view. Hence, for a type T which public view derives from a type A but
    private view derives from a type B (which itself derives from A),
    invoking this selector on the public view of T will yield B and then A.

.. function:: component_types()

    Return all the ``BaseTypeDecl`` corresponding to all fields of a given
    type, including their full views, base types and subtypes.

.. function:: full_parent_types()

    Return all base (sub)types full views

.. function:: parent_decl_chain()

    Return all parent basic decl nodes starting from a given node, using
    semantic parent.
    When on a subprogram or package body, go to the declaration
    This allows us to, if in a generic template, always find back the
    generic formal.

.. function:: semantic_parent()

    Return all semantic parent nodes starting from a given node.

.. function:: super_types()

    Yields the chain of super types of the given type, as viewed from that
    type. Hence, for a type T which public view derives from a type A but
    private view derives from a type B (which itself derives from A),
    invoking this selector on the public view of T will yield A.


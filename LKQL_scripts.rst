===========
LKQL Checks
===========

This document presents a *florilège* of LKQL-based checks inspired on **GNATCheck** and **kp-toolkit**.



1. Deep inheritance hierarchies
===============================

LKQL Script::
    
    selector supertypes
        | TypeDecl          => skip it.type_def
        | InterfaceTypeDef  => skip it.interfaces
        | RecordTypeDef     => ()
        | DerivedTypeDef    => skip it.subtype_indication <> skip it.interfaces
        | SubtypeIndication => skip it.name
        | Name              => rec it.referenced_decl(true)
        | ParentList        => skip *it.children
        | _                 => ()

    let result = query TypeDecl(any supertypes(depth=3): _)

    print(result)


Lists the type declarations which depth is greater or equal to 3.

`Deep inheritance hierarchyies test <https://github.com/geoffreycopin/langkit-query-language/tree/checker/testsuite/tests/checks/deep_inheritance>`_


2. Deep Library
===============

LKQL Script::

    let targetDepth = 5

    let result =
        query PackageDecl (package_name: DefiningName(any children(depth=targetDepth): Name))

    print(result)


Lists the package declations that have more than `targetDepth` parents.

`Deep library test <https://github.com/geoffreycopin/langkit-query-language/tree/checker/testsuite/tests/checks/deep_library>`_


3. Implicit Small
=================

LKQL Script::

    let result =
        query TypeDecl(any children: OrdinaryFixedPointDef, get_attribute("Small"): null)
    
    print(result)


Lists the ordinary fixed point type declarations that lack an explicit representation clause
defining it's `'Small` value.  


`Implicit small test <https://github.com/geoffreycopin/langkit-query-language/tree/checker/testsuite/tests/checks/implicit_small>`_


4. KP R517_023: Wrong value returned for unconstrained packed array formal
==========================================================================

LKQL Script::

    let result = query CallExpr(
                            is_call(): true,
                            any children(depth=2): p@ParamAssoc when isFlagArg(p)
                       )
    print(result)

    fun isFlagArg(actual) =
        val formal = getFormal(actual);
        if formal == null
        then false
        else isInOutOrOut(formal) &&
             isUnConstrainedArrayWithPacked(typeOfParamSpec(formal)) &&
             isReprRecordComponent(argumentDecl(actual.r_expr))

    fun getFormal(actual) = 
        val params = actual.get_params();
        if params.length == 0 then null else params[1]?.parent?.parent

    fun isReprRecordComponent(decl) =
        decl is ComponentDecl(any parent:
                    TypeDecl(get_record_representation_clause(): RecordRepClause)
                )

    fun argumentDecl(argExpr) =
        match argExpr
            | DottedName => it.referenced_decl()
            | _          => null

    fun typeOfParamSpec(spec) =
        spec?.type_expr?.name?.referenced_decl()

    fun isInOutOrOut(spec) =
        spec?.mode is ModeOut || spec?.mode is ModeInOut

    fun isUnConstrainedArrayWithPacked(decl) =
        decl is TypeDecl(
                    type_def: ArrayTypeDef(any children(depth=6): BoxExpr),
                    get_attribute("Pack"): AdaNode
                )

The compiler generates wrong code for the call to a subprogram with an In Out or Out 
formal parameter of an unconstrained packed array type, when the actual parameter 
is a component of a record subject to a representation clause. 

This LKQL script lists the occurrences of the aforementioned issue.

`KP R517_023 test <https://github.com/geoffreycopin/langkit-query-language/tree/checker/testsuite/tests/checks/kp_R517_023>`_


5. Multiple entries in protected definitions
============================================

LKQL Script::

    let result = query ProtectedDef any children EntryDecl(any prevSiblings: EntryDecl)
    print(result)

Lists the entries that belong to a protected definition containing multiple entries.
The first entry of the definition will not be flagged.

`Multiple entries test <https://github.com/geoffreycopin/langkit-query-language/tree/checker/testsuite/tests/checks/multiple_entries>`_


6. No explicit real range
=========================

LKQL Script::

    let result = query t @ _
                    when isRealWithoutRange(t) ||
                        t is SubtypeDecl(any superTypes: s@_ when isRealWithoutRange(s))
    print(result)

    fun isRealWithoutRange(decl) =
        decl is TypeDecl(type_def: RealTypeDef(no children: RangeSpec))


Lists the floating point type definitions that do not include an explicit range.

`No real range test <https://github.com/geoffreycopin/langkit-query-language/tree/checker/testsuite/tests/checks/no_real_range>`_

7. Parameters out of order
==========================

LKQL Script::

    let result =
        query p@ParamSpec(any nextSiblings: sib@ParamSpec when priority(sib) > priority(p))

    print(result)

    fun priority(paramSpec) =
        if paramSpec.default_expr != null then 0
        else match paramSpec.mode
                | ModeOut     => 1
                | ModeInOut   => 2
                | ModeIn      => 3
                | ModeDefault => 3

Flag each subprogram and entry declaration whose formal parameters are not ordered according to the following scheme:
    * in and access parameters first, then in out parameters, and then out parameters;
    * for in mode, parameters with default initialization expressions occur last

`Parameters out of order test <https://github.com/geoffreycopin/langkit-query-language/tree/checker/testsuite/tests/checks/paramters_order>`_

8. Raise predefined exception
=============================

LKQL Script::

    let result = query r@RaiseStmt when isPredefinedName(r.exception_name)

    print(result)

    fun isPredefinedName(id) =
        val name = id?.text;
        name == "Program_Error" || name == "Constraint_Error" ||
        name == "Numeric_Error" || name == "Storage_Error"    ||
        name == "Tasking_Error"

List `raise` statements that raise a predefined exception.

`Raise predefined exception test <https://github.com/geoffreycopin/langkit-query-language/tree/checker/testsuite/tests/checks/raise_builtin>`_

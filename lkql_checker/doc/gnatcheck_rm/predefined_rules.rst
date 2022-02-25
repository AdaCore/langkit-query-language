.. _Predefined_Rules:

****************
Predefined Rules
****************

.. index:: Predefined Rules

The description of the rules currently implemented in ``gnatcheck`` is
given in this chapter.
The rule identifier is
used as a parameter of ``gnatcheck``'s ``+R`` or ``-R``
switches.

Be aware that most of these rules apply to specialized coding
requirements developed by individual users and may well not make sense in
other environments. In particular, there are many rules that conflict
with one another. Proper usage of gnatcheck involves selecting the rules
you wish to apply by looking at your independently developed coding
standards and finding the corresponding gnatcheck rules.

If not otherwise specified, a rule does not do any check for the
results of generic instantiations.

Style-Related Rules
===================

.. index:: Style-related rules

The rules in this section may be used to enforce various feature usages
consistent with good software engineering, for example
as described in Ada 95 Quality and Style.

.. _Tasking:

Tasking
-------

.. index:: Tasking-related rules

The rules in this subsection may be used to enforce various
feature usages related to concurrency.

.. _Multiple_Entries_In_Protected_Definitions:

``Multiple_Entries_In_Protected_Definitions``
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: Multiple_Entries_In_Protected_Definitions

Flag each protected definition (i.e., each protected object/type declaration)
that declares more than one entry.
Diagnostic messages are generated for all the entry declarations
except the first one. An entry family is counted as one entry. Entries from
the private part of the protected definition are also checked.

This rule has no parameters.

.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 3

   protected PO is
      entry Get (I :     Integer);
      entry Put (I : out Integer);    --  FLAG
      procedure Reset;
      function Check return Boolean;
   private
      Val : Integer := 0;
   end PO;

.. _Volatile_Objects_Without_Address_Clauses:

``Volatile_Objects_Without_Address_Clauses``
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: Volatile_Objects_Without_Address_Clauses

Flag each volatile object that does not have an address specification.
Only variable declarations are checked.

An object is considered as being volatile if a pragma or aspect Volatile
is applied to the object or to its type, if the object is atomic or
if the GNAT compiler considers this object as volatile because of some
code generation reasons.

This rule has no parameters.

.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 6, 11

   with Interfaces, System, System.Storage_Elements;
   package Foo is
      Variable: Interfaces.Unsigned_8
         with Address => System.Storage_Elements.To_Address (0), Volatile;

      Variable1: Interfaces.Unsigned_8                                --  FLAG
         with Volatile;

      type My_Int is range 1 .. 32 with Volatile;

      Variable3 : My_Int;                                             --  FLAG

      Variable4 : My_Int
        with Address => Variable3'Address;
   end Foo;

.. _Object_Orientation:

Object Orientation
------------------

.. index:: Object-Orientation related rules

The rules in this subsection may be used to enforce various
feature usages related to Object-Oriented Programming.


.. _Constructors:

``Constructors``
^^^^^^^^^^^^^^^^

.. index:: Constructors

Flag any declaration of a primitive function of a tagged type that has a
controlling result and no controlling parameter. If a declaration is a
completion of another declaration then it is not flagged.

This rule has no parameters.

.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 5-7

   type T is tagged record
      I : Integer;
   end record;

   function Fun (I : Integer) return T;                -- FLAG
   function Bar (J : Integer) return T renames Fun;    -- FLAG
   function Foo (K : Integer) return T is ((I => K));  -- FLAG


.. _Deep_Inheritance_Hierarchies:

``Deep_Inheritance_Hierarchies``
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: Deep_Inheritance_Hierarchies

Flags a tagged derived type declaration or an interface type declaration if
its depth (in its inheritance hierarchy) exceeds the value specified by the
*N* rule parameter. Types in generic instantiations which violate this
rule are also flagged; generic formal types are not flagged. This rule also
does not flag private extension declarations. In the case of a private
extension, the corresponding full declaration is checked.

In most cases, the inheritance depth of a tagged type or interface type is
defined as 0 for a type with no parent and no progenitor, and otherwise as 1 +
max of the depths of the immediate parent and immediate progenitors. If the
declaration of a formal derived type has no progenitor, or if the declaration
of a formal interface type has exactly one progenitor, then the inheritance
depth of such a formal derived/interface type is equal to the inheritance
depth of its parent/progenitor type, otherwise the general rule is applied.

If the rule flags a type declaration inside the generic unit, this means that
this type declaration will be flagged in any instantiation of the generic
unit. But if a type is derived from a format type or has a formal progenitor
and it is not flagged at the place where it is defined in a generic unit, it
may or may not be flagged in instantiation, this depends of the inheritance
depth of the actual parameters.

This rule has the following (mandatory) parameter for the ``+R`` option:



*N*
  Integer not less than -1 specifying the maximal allowed depth of any
  inheritance hierarchy. If the rule parameter is set to -1, the rule
  flags all the declarations of tagged and interface types.

.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 8

   type I0 is interface;
   type I1 is interface and I0;
   type I2 is interface and I1;

   type T0 is tagged null record;
   type T1 is new T0 and I0 with null record;
   type T2 is new T0 and I1 with null record;
   type T3 is new T0 and I2 with null record; -- FLAG (if rule parameter is 2)


.. _Direct_Calls_To_Primitives:

``Direct_Calls_To_Primitives``
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: Direct_Calls_To_Primitives

Flag any non-dispatching call to a dispatching primitive operation, except for:


*
  a call to the corresponding primitive of the parent type.  (This
  occurs in the common idiom where a primitive subprogram for a tagged type
  directly calls the same primitive subprogram of the parent type.)

*
  a call to a primitive of an untagged private type, even though the full type
  may be tagged, when the call is made at a place where the view of the type is
  untagged.


This rule has the following (optional) parameters for the ``+R`` option:



*Except_Constructors*
  Do not flag non-dispatching calls to functions if the function has a
  controlling result and no controlling parameters (in a traditional OO sense
  such functions may be considered as constructors).

.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 28, 29

   package Root is
      type T_Root is tagged private;

      procedure Primitive_1 (X : in out T_Root);
      procedure Primitive_2 (X : in out T_Root);
   private
      type T_Root is tagged record
         Comp : Integer;
      end record;
   end Root;

   package Root.Child is
      type T_Child is new T_Root with private;

      procedure Primitive_1 (X : in out T_Child);
      procedure Primitive_2 (X : in out T_Child);
   private
      type T_Child is new T_Root with record
         B : Boolean;
      end record;
   end Root.Child;

   package body Root.Child is

      procedure Primitive_1 (X : in out T_Child) is
      begin
         Primitive_1 (T_Root (X));      --  NO FLAG
         Primitive_2 (T_Root (X));      --  FLAG
         Primitive_2 (X);               --  FLAG
      end Primitive_1;

      procedure Primitive_2 (X : in out T_Child) is
      begin
         X.Comp  := X.Comp + 1;
      end Primitive_2;

   end Root.Child;


.. _Downward_View_Conversions:

``Downward_View_Conversions``
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: Downward_View_Conversions

Flag downward view conversions.

This rule has no parameters.

.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 19, 21

   package Foo is
      type T1 is tagged private;
      procedure Proc1 (X : in out T1'Class);

      type T2 is new T1 with private;
      procedure Proc2 (X : in out T2'Class);

   private
      type T1 is tagged record
         C : Integer := 0;
      end record;

      type T2 is new T1 with null record;
   end Foo;

   package body Foo is

      procedure Proc1 (X : in out T1'Class) is
         Var : T2 := T2 (X);                   --  FLAG
      begin
         Proc2 (T2'Class (X));                 --  FLAG
      end Proc1;

      procedure Proc2 (X : in out T2'Class) is
      begin
         X.C := X.C + 1;
      end Proc2;

   end Foo;


.. _No_Inherited_Classwide_Pre:

``No_Inherited_Classwide_Pre``
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: No_Inherited_Classwide_Pre

Flag a declaration of an overriding primitive operation of a tagged type
if at least one of the operations it overrides or implements does not
have (explicitly defined or inherited) Pre'Class aspect defined for
it.

This rule has no parameters.

.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 13, 17

   package Foo is

      type Int is interface;
      function Test (X : Int) return Boolean is abstract;
      procedure Proc (I : in out Int) is abstract with Pre'Class => Test (I);

      type Int1 is interface;
      procedure Proc (I : in out Int1) is abstract;

      type T is tagged private;

       type NT1 is new T and Int with private;
       function Test (X : NT1) return Boolean;        --  FLAG
       procedure Proc (X : in out NT1);

       type NT2 is new T and Int1 with private;
       procedure Proc (X : in out NT2);               --  FLAG

      private
      type T is tagged record
         I : Integer;
      end record;

      type NT1 is new T and Int with null record;
      type NT2 is new T and Int1 with null record;

   end Foo;

.. _Specific_Pre_Post:

``Specific_Pre_Post``
^^^^^^^^^^^^^^^^^^^^^

.. index:: Specific_Pre_Post

Flag a declaration of a primitive operation of a tagged type if this
declaration contains specification of Pre or/and Post aspect.

This rule has no parameters.

.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 5, 8, 11, 19

   type T is tagged private;
   function Check1 (X : T) return Boolean;
   function Check2 (X : T) return Boolean;

   procedure Proc1 (X : in out T)           --  FLAG
      with Pre => Check1 (X);

   procedure Proc2 (X : in out T)           --  FLAG
      with Post => Check2 (X);

   function Fun1 (X : T) return Integer     --  FLAG
      with Pre  => Check1 (X),
           Post => Check2 (X);

   function Fun2 (X : T) return Integer
      with Pre'Class  => Check1 (X),
           Post'Class => Check2 (X);

   function Fun3 (X : T) return Integer     --  FLAG
      with Pre'Class  => Check1 (X),
           Post'Class => Check2 (X),
           Pre        => Check1 (X),
           Post       => Check2 (X);

.. _Specific_Parent_Type_Invariant:

``Specific_Parent_Type_Invariant``
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: Specific_Parent_Type_Invariant

Flag any record extension definition or private extension definition if
a parent type has a Type_Invariant aspect defined for it. A record
extension definition is not flagged if it is a part of a completion of a
private extension declaration.

This rule has no parameters.

.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 18, 23

   package Pack1 is
      type PT1 is tagged private;
      type PT2 is tagged private
        with Type_Invariant => Invariant_2 (PT2);

      function Invariant_2   (X : PT2) return Boolean;

   private
      type PT1 is tagged record
         I : Integer;
      end record;

      type PT2 is tagged record
         I : Integer;
      end record;

      type PT1_N is new PT1 with null record;
      type PT2_N is new PT2 with null record;    --  FLAG
   end Pack1;

   package Pack2 is
      type N_PT1 is new Pack1.PT1 with private;
      type N_PT2 is new Pack1.PT2 with private;  --  FLAG
   private
      type N_PT1 is new Pack1.PT1 with null record;
      type N_PT2 is new Pack1.PT2 with null record;
   end Pack2;


.. _Specific_Type_Invariants:

``Specific_Type_Invariants``
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: Specific_Type_Invariants

Flag any definition of (non-class-wide) Type_Invariant aspect that is
a part of a declaration of a tagged type or a tagged extension. Definitions
of Type_Invariant'Class aspects are not flagged. Definitions of (non-class-wide)
Type_Invariant aspect that are parts of declarations of non-tagged types
are not flagged.

This rule has no parameters.

.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 6

   type PT is private
      with Type_Invariant => Test_PT (PT);
   function Test_PT (X : PT) return Boolean;

   type TPT1 is tagged private
      with Type_Invariant => Test_TPT1 (TPT1);        --  FLAG
   function Test_TPT1 (X : TPT1) return Boolean;

   type TPT2 is tagged private
      with Type_Invariant'Class => Test_TPT2 (TPT2);
   function Test_TPT2 (X : TPT2) return Boolean;

.. _Too_Many_Parents:

``Too_Many_Parents``
^^^^^^^^^^^^^^^^^^^^

.. index:: Too_Many_Parents

Flag any tagged type declaration, interface type declaration, single task
declaration or single protected declaration that has more than *N*
*parents*, where *N* is a parameter of the rule.
A *parent* here is either a (sub)type denoted by the subtype mark from the
parent_subtype_indication (in case of a derived type declaration), or
any of the progenitors from the interface list (if any).

This rule has the following (mandatory) parameters for the ``+R`` option:



*N*
  Positive integer specifying the maximal allowed number of parents/progenitors.

.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 11

   type I1 is interface;
   type I2 is interface;
   type I3 is interface;
   type I4 is interface;

   type T_Root is tagged private;

   type T_1 is new T_Root with private;
   type T_2 is new T_Root and I1 with private;
   type T_3 is new T_Root and I1 and I2 with private;
   type T_4 is new T_Root and I1 and I2 and I3 with private; -- FLAG (if rule parameter is 3 or less)


.. _Too_Many_Primitives:

``Too_Many_Primitives``
^^^^^^^^^^^^^^^^^^^^^^^

.. index:: Too_Many_Primitives

Flag any tagged type declaration that has more than N user-defined
primitive operations (counting both inherited and not overridden and
explicitly declared, not counting predefined operators). Only types
declared in visible parts of packages, generic packages and package
instantiations are flagged.

This rule has the following (mandatory) parameters for the ``+R`` option:



*N*
  Positive integer specifying the maximal number of primitives when
  the type is not flagged.


.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 2, 14

   package Foo is
      type PT is tagged private;     --  FLAG (if rule parameter is 3 or less)

      procedure P1 (X : in out PT);
      procedure P2 (X : in out PT) is null;
      function F1 (X : PT) return Integer;
      function F2 (X : PT) return Integer is (F1 (X) + 1);

      type I1 is interface;

      procedure P1 (X : in out I1) is abstract;
      procedure P2 (X : in out I1) is null;

      type I2 is interface and I1;   --  FLAG (if rule parameter is 3 or less)
      function F1 (X : I2) return Integer is abstract;
      function F2 (X : I2) return Integer is abstract;

   private
      type PT is tagged record
         I : Integer;
      end record;
   end Foo;

.. _Visible_Components:

``Visible_Components``
^^^^^^^^^^^^^^^^^^^^^^

.. index:: Visible_Components

Flag all the type declarations located in the visible part of a library
package or a library generic package that can declare a visible component.
A visible component can be declared in a *record definition* which appears
on its own or as part of a record extension.  The *record definition* is
flagged even if it contains no components.

*Record definitions* located in private parts of library (generic) packages
or in local (generic) packages are not flagged. *Record definitions* in
private packages, in package bodies, and in the main subprogram body are not
flagged.

This rule has the following (optional) parameters for the ``+R`` option:


*Tagged_Only*
  Only declarations of tagged types are flagged.

.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 3, 5, 10, 17

   with Types;
   package Foo is
      type Null_Record is null record;                                    --  FLAG

      type Not_Null_Record is record                                      --  FLAG
         I : Integer;
         B : Boolean;
      end record;

      type Tagged_Not_Null_Record is tagged record                        --  FLAG
         I : Integer;
         B : Boolean;
      end record;

      type Private_Extension is new Types.Tagged_Private with private;

      type NoN_Private_Extension is new Types.Tagged_Private with record  --  FLAG
         B : Boolean;
      end record;

   private
      type Rec is tagged record
         I : Integer;
      end record;

      type Private_Extension is new Types.Tagged_Private with record
         C : Rec;
      end record;
   end Foo;


.. _Portability:

Portability
-----------

.. index:: Portability-related rules

The rules in this subsection may be used to enforce various
feature usages that support program portability.

.. _Bit_Records_Without_Layout_Definition:

``Bit_Records_Without_Layout_Definition``
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: Bit_Records_Without_Layout_Definition

Flag record type declarations if a record has a component of a modular
type and the record type is packed but does not have a record representation clause
applied to it.

This rule has no parameters.

.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 4

   package Pack is
      type My_Mod is mod 8;

      type My_Rec is record   --  FLAG
         I : My_Mod;
      end record;
      pragma Pack (My_Rec);
   end Pack;

.. _Forbidden_Attributes:

``Forbidden_Attributes``
^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: Forbidden_Attributes

Flag each use of the specified attributes. The attributes to be detected are
named in the rule's parameters.

This rule has the following parameters:

* For the ``+R`` option



*Attribute_Designator*
    Adds the specified attribute to the set of attributes to be detected and sets
    the detection checks for all the specified attributes ON.
    If *Attribute_Designator*
    does not denote any attribute defined in the Ada standard
    or in the GNAT Reference Manual,
    it is treated as the name of unknown attribute.


``GNAT``
    All the GNAT-specific attributes are detected; this sets
    the detection checks for all the specified attributes ON.


``ALL``
    All attributes are detected; this sets the rule ON.

* For the ``-R`` option


*Attribute_Designator*
    Removes the specified attribute from the set of attributes to be
    detected without affecting detection checks for
    other attributes. If *Attribute_Designator* does not correspond to any
    attribute defined in the Ada standard
    or in the GNAT Reference Manual,
    this option is treated as turning OFF detection of all unknown attributes.


GNAT
    Turn OFF detection of all GNAT-specific attributes


ALL
    Clear the list of the attributes to be detected and
    turn the rule OFF.

Parameters are not case sensitive. If *Attribute_Designator* does not
have the syntax of an Ada identifier and therefore can not be considered as a
(part of an) attribute designator, a diagnostic message is generated and the
corresponding parameter is ignored. (If an attribute allows a static
expression to be a part of the attribute designator, this expression is
ignored by this rule.)

When more than one parameter is given in the same rule option, the parameters
must be separated by commas.

If more than one option for this rule is specified for the gnatcheck call, a
new option overrides the previous one(s).

The ``+R`` option with no parameters turns the rule ON, with the set of
attributes to be detected defined by the previous rule options.
(By default this set is empty, so if the only option specified for the rule is
``+RForbidden_Attributes`` (with
no parameter), then the rule is enabled, but it does not detect anything).
The ``-R`` option with no parameter turns the rule OFF, but it does not
affect the set of attributes to be detected.

The rule allows parametric exemption, the parameters that are allowed in the
definition of exemption sections are *Attribute_Designators*. Each
*Attribute_Designator* used as a rule exemption parameter should denote
a predefined or GNAT-specific attribute.

.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 6, 9

   --  if the rule is activated as +RForbidden_Attributes:Range,First,Last
   procedure Foo is
      type Arr is array (1 .. 10) of Integer;
      Arr_Var : Arr;

      subtype Ind is Integer range Arr'First .. Arr'Last; --  FLAG (twice)
   begin

      for J in Arr'Range loop                             --  FLAG
         Arr_Var (J) := Integer'Succ (J);



.. _Forbidden_Pragmas:

``Forbidden_Pragmas``
^^^^^^^^^^^^^^^^^^^^^

.. index:: Forbidden_Pragmas

Flag each use of the specified pragmas.  The pragmas to be detected
are named in the rule's  parameters.

This rule has the following parameters:

* For the ``+R`` option



*Pragma_Name*
    Adds the specified pragma to the set of pragmas to be
    checked and sets the checks for all the specified pragmas
    ON. *Pragma_Name* is treated as a name of a pragma. If it
    does not correspond to any pragma name defined in the Ada
    standard or to the name of a GNAT-specific pragma defined
    in the GNAT Reference Manual,
    it is treated as the name of unknown pragma.


``GNAT``
    All the GNAT-specific pragmas are detected; this sets
    the checks for all the specified pragmas ON.


``ALL``
    All pragmas are detected; this sets the rule ON.

* For the ``-R`` option


*Pragma_Name*
    Removes the specified pragma from the set of pragmas to be
    checked without affecting checks for
    other pragmas. *Pragma_Name* is treated as a name
    of a pragma. If it does not correspond to any pragma
    defined in the Ada standard or to any name defined
    in the GNAT Reference Manual,
    this option is treated as turning OFF detection of all unknown pragmas.


GNAT
    Turn OFF detection of all GNAT-specific pragmas


ALL
    Clear the list of the pragmas to be detected and
    turn the rule OFF.

Parameters are not case sensitive. If *Pragma_Name* does not have
the syntax of an Ada identifier and therefore can not be considered
as a pragma name, a diagnostic message is generated and the corresponding
parameter is ignored.

When more than one parameter is given in the same rule option, the parameters
must be separated by a comma.

If more than one option for this rule is specified for the ``gnatcheck``
call, a new option overrides the previous one(s).

The ``+R`` option with no parameters turns the rule ON with the set of
pragmas to be detected defined by the previous rule options.
(By default this set is empty, so if the only option specified for the rule is
``+RForbidden_Pragmas`` (with
no parameter), then the rule is enabled, but it does not detect anything).
The ``-R`` option with no parameter turns the rule OFF, but it does not
affect the set of pragmas to be detected.

Note that in case when the rule is enabled with *ALL* parameter, then
the rule will flag also pragmas ``Annotate`` used to exempt rules, see
:ref:`Rule_exemption`. Even if you exempt this *Forbidden_Pragmas* rule
then the pragma ``Annotate`` that closes the exemption section will be
flagged as non-exempted. To avoid this, turn off the check for pragma
``Annotate`` by using ``-RForbidden_Pragmas:Annotate`` rule option.

The rule allows parametric exemption, the parameters that are allowed in the
definition of exemption sections are pragma names. Each
name used as a rule exemption parameter should denote
a predefined or GNAT-specific pragma.

.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 5

   --  if the rule is activated as +RForbidden_Pragmas:Pack
   package Foo is

      type Arr is array (1 .. 8) of Boolean;
      pragma Pack (Arr);                      --  FLAG

      I : Integer;
      pragma Atomic (I);

   end Foo;



.. _Implicit_SMALL_For_Fixed_Point_Types:

``Implicit_SMALL_For_Fixed_Point_Types``
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: Implicit_SMALL_For_Fixed_Point_Types

Flag each fixed point type declaration that lacks an explicit
representation  clause to define its ``'Small`` value.
Since ``'Small`` can be  defined only for ordinary fixed point types,
decimal fixed point type declarations are not checked.

This rule has no parameters.

.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 3

   package Foo is
      type Fraction is delta 0.01 range -1.0 .. 1.0;
      type Fraction1 is delta 0.01 range -1.0 .. 1.0; --  FLAG

      type Money is delta 0.01 digits 15;

      for Fraction'Small use 0.01;
   end Foo;



.. _Incomplete_Representation_Specifications:

``Incomplete_Representation_Specifications``
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: Incomplete_Representation_Specifications

Flag all record types that have a layout representation specification
but without Size and Pack representation specifications.

This rule has no parameters.

.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 2

   package Pack is
      type Rec is record  --  FLAG
         I : Integer;
         B : Boolean;
      end record;

      for Rec use record
         I at 0 range 0 ..31;
         B at 4 range 0 .. 7;
      end record;
   end Pack;


.. _Membership_For_Validity:

``Membership_For_Validity``
^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: Membership_For_Validity

Flag membership tests that can be replaced by a ``'Valid`` attribute.
Two forms of membership tests are flagged:

*
  X in Subtype_Of_X

*
  X in Subtype_Of_X'First .. Subtype_Of_X'Last

where X is a data object except for a loop parameter, and ``Subtype_Of_X``
is the subtype of the object as given by the corresponding declaration.

This rule has no parameters.

.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 5

      subtype My_Int is Integer range 1 .. 10;
      X : My_Int;
      Y : Integer;
   begin
      if X in My_Int then                           --  FLAG


.. _No_Explicit_Real_Range:

``No_Explicit_Real_Range``
^^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: No_Explicit_Real_Range

Flag a declaration of a floating point type or a decimal fixed point
type, including types derived from them if no explicit range
specification is provided for the type.

This rule has no parameters.

.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 1, 2

   type F1 is digits 8;                           --  FLAG
   type F2 is delta 0.01 digits 8;                --  FLAG

.. _No_Scalar_Storage_Order_Specified:

``No_Scalar_Storage_Order_Specified``
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: No_Scalar_Storage_Order_Specified

Flag each record type declaration, record extension declaration, and
untagged derived record type declaration if a
record_representation_clause that has at least one component clause
applies to it (or an ancestor), but neither the type nor any of its
ancestors has an explicitly specified Scalar_Storage_Order aspect.

This rule has no parameters.

.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 4

   with System;
   package Foo is

      type Rec1 is record     --  FLAG
         I : Integer;
      end record;

      for Rec1 use record
         I at 0 range 0 .. 31;
      end record;

      type Rec2 is record     --  NO FLAG
         I : Integer;
      end record
        with Scalar_Storage_Order => System.Low_Order_First;

      for Rec2 use record
         I at 0 range 0 .. 31;
      end record;

   end Foo;


.. _Predefined_Numeric_Types:

``Predefined_Numeric_Types``
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: Predefined_Numeric_Types

Flag each explicit use of the name of any numeric type or subtype declared
in package ``Standard``.

The rationale for this rule is to detect when the
program may depend on platform-specific characteristics of the implementation
of the predefined numeric types. Note that this rule is overly pessimistic;
for example, a program that uses ``String`` indexing
likely needs a variable of type ``Integer``.
Another example is the flagging of predefined numeric types with explicit
constraints:

.. code-block:: ada

      subtype My_Integer is Integer range Left .. Right;
      Vy_Var : My_Integer;


This rule detects only numeric types and subtypes declared in package
``Standard``. The use of numeric types and subtypes declared in other
predefined packages (such as ``System.Any_Priority`` or
``Ada.Text_IO.Count``) is not flagged

This rule has no parameters.

.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 2, 3, 6, 9

   package Foo is
      I : Integer;                               -- FLAG
      F : Float;                                 -- FLAG
      B : Boolean;

      type Arr is array (1 .. 5) of Short_Float; -- FLAG

      type Res is record
         C1 : Long_Integer;                      -- FLAG
         C2 : Character;
      end record;

   end Foo;


.. _Printable_ASCII:

``Printable_ASCII``
^^^^^^^^^^^^^^^^^^^

.. index:: Printable_ASCII

Flag source code text characters that are not part of the printable
ASCII character set, a line feed, or a carriage return character (i.e.
values 10, 13 and 32 .. 126 of the ASCII Character set).

This rule has no parameters.

.. _Separate_Numeric_Error_Handlers:

``Separate_Numeric_Error_Handlers``
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: Separate_Numeric_Error_Handlers

Flags each exception handler that contains a choice for
the predefined ``Constraint_Error`` exception, but does not contain
the choice for the predefined ``Numeric_Error`` exception, or
that contains the choice for ``Numeric_Error``, but does not contain the
choice for ``Constraint_Error``.

This rule has no parameters.

.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 2

   exception
      when Constraint_Error =>  --  FLAG
         Clean_Up;
   end;

.. _Size_Attribute_For_Types:

``Size_Attribute_For_Types``
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: Size_Attribute_For_Types

Flag any 'Size attribute reference if its prefix denotes a type or a subtype.
Attribute references that are subcomponents of attribute definition clauses of
aspect specifications are not flagged.

This rule has no parameters.

.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 6

   type T is record
      I : Integer;
      B : Boolean;
   end record;

   Size_Of_T : constant Integer := T'Size  --  FLAG

.. _Program_Structure:

Program Structure
-----------------

.. index:: Program Structure related rules

The rules in this subsection may be used to enforce feature usages
related to program structure.




.. _Deep_Library_Hierarchy:

``Deep_Library_Hierarchy``
^^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: Deep_Library_Hierarchy

Flag any library package declaration, library generic package
declaration or library package instantiation that has more than N
parents and grandparents (that is, the name of such a library unit
contains more than N dots). Child subprograms, generic subprograms
subprogram instantiations and package bodies are not flagged.

This rule has the following (mandatory) parameters for the ``+R`` option:

*N*
  Positive integer specifying the maximal number of ancestors when
  the unit is not flagged.

.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 1

   package Parent.Child1.Child2 is  -- FLAG  (if rule parameter is 1)
      I : Integer;
   end;


.. _Deeply_Nested_Generics:

``Deeply_Nested_Generics``
^^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: Deeply_Nested_Generics

Flag a generic declaration nested in another generic declaration if
the nesting level of the inner generic exceeds
the value specified by the *N* rule parameter.
The nesting level is the number of generic declarations that enclose the given
(generic) declaration. Formal packages are not flagged by this rule.

This rule has the following (mandatory) parameters for the ``+R`` option:



*N*
  Non-negative integer specifying the maximum nesting level for a
  generic declaration.

.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 7

   package Foo is

      generic
      package P_G_0 is
         generic
         package P_G_1 is
            generic              --  FLAG (if rule parameter is 1)
            package P_G_2 is
               I  : Integer;
            end;
         end;
      end;

   end Foo;


.. _Deeply_Nested_Instantiations:

``Deeply_Nested_Instantiations``
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: Deeply_Nested_Instantiations

Flag a generic package instantiation if it contains another instantiation
in its specification and this nested instantiation also contains another
instantiation in its specification and so on, and the length of these
nested instantiations is more than N where N is a rule parameter.

This rule has the following (mandatory) parameters for the ``+R`` option:

*N*
  Non-negative integer specifying the maximum nesting level for instantiations.

.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 27

   procedure Proc is

      generic
      procedure D;

      procedure D is
      begin
         null;
      end D;

      generic
      package C is
         procedure Inst is new D;
      end C;

      generic
      package B is
         package Inst is new C;
      end B;

      generic
      package A is
          package Inst is new B;
      end A;

      package P is
         package Inst is new A;   --  FLAG
      end P;


.. _Local_Packages:

``Local_Packages``
^^^^^^^^^^^^^^^^^^

.. index:: Local_Packages

Flag all local packages declared in package and generic package
specs.
Local packages in bodies are not flagged.

This rule has no parameters.

.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 2

   package Foo is
      package Inner is    --  FLAG
         I : Integer;
      end Inner;
   end Foo;

.. _Non_Visible_Exceptions:

``Non_Visible_Exceptions``
^^^^^^^^^^^^^^^^^^^^^^^^^^


.. _Maximum_Expression_Complexity:

``Maximum_Expression_Complexity``
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: Maximum_Expression_Complexity

Flag any expression that is not directly a part of another expression
which contains more than N expressions of the following kinds (each count for 1)
as its subcomponents, N is a rule parameter:

*
  Identifiers;

*
  Numeric, string or character literals;

*
  Conditional expressions;

*
  Quantified expressions;

*
  Aggregates;

*
  @ symbols (target names).

This rule has the following (mandatory) parameter for the ``+R`` option:

*N*
  Positive integer specifying the maximum allowed number of expression
  subcomponents.

.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 1-3

   I := 1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9 + 10;  --  FLAG if N < 10
   I := F (I);   --  FLAG if N < 2
   I := F5 (1 + 2 + 3 + 4 + 5, 2, 3, 4, 5);   --  FLAG (twice) if N < 5


.. _Maximum_Subprogram_Lines:

``Maximum_Subprogram_Lines``
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: Maximum_Subprogram_Lines

Flag handled sequences of statements of subprogram bodies exceeding *N* textual
lines (*N* is the rule parameter). Lines are counted from the beginning of the
first to the end of the last statement, including blank and comment lines

This rule has the following (mandatory) parameter for the ``+R`` option:

*N*
  Positive integer specifying the maximum allowed number of lines in the
  subprogram statement sequence.



.. index:: Non_Visible_Exceptions

Flag constructs leading to the possibility of propagating an exception
out of the scope in which the exception is declared.
Two cases are detected:

*
  An exception declaration in a subprogram body, task body or block
  statement is flagged if the body or statement does not contain a handler for
  that exception or a handler with an ``others`` choice.

*
  A ``raise`` statement in an exception handler of a subprogram body,
  task body or block statement is flagged if it (re)raises a locally
  declared exception.  This may occur under the following circumstances:

  *
    it explicitly raises a locally declared exception, or
  *
    it does not specify an exception name (i.e., it is simply ``raise;``)
    and the enclosing handler contains a locally declared exception in its
    exception choices.

Renamings of local exceptions are not flagged.

This rule has no parameters.

.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 5, 18

   procedure Bar is
      Var : Integer :=- 13;

      procedure Inner (I : in out Integer) is
         Inner_Exception_1 : exception;          --  FLAG
         Inner_Exception_2 : exception;
      begin
         if I = 0 then
            raise Inner_Exception_1;
         elsif I = 1 then
            raise Inner_Exception_2;
         else
            I := I - 1;
         end if;
      exception
         when Inner_Exception_2 =>
            I := 0;
            raise;                               --  FLAG
      end Inner;

   begin
      Inner (Var);
   end Bar;


.. _One_Tagged_Type_Per_Package:

``One_Tagged_Type_Per_Package``
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: One_Tagged_Type_Per_Package

Flag all package declarations with more than one tagged type declaration
in the visible part.

This rule has no parameters.

.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 1

   package P is  --  FLAG

      type T is tagged null record;
      type T2 is tagged null record;

   end P;


.. _Raising_External_Exceptions:

``Raising_External_Exceptions``
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: Raising_External_Exceptions

Flag any ``raise`` statement, in a program unit declared in a library
package or in a generic library package, for an exception that is
neither a predefined exception nor an exception that is also declared (or
renamed) in the visible part of the package.

This rule has no parameters.

.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 12

   package Exception_Declarations is
      Ex : exception;
   end Exception_Declarations;
   package Foo is
      procedure Proc (I : in out Integer);
   end Foo;
   with Exception_Declarations;
   package body Foo is
      procedure Proc (I : in out Integer) is
      begin
         if I < 0 then
            raise Exception_Declarations.Ex;   --  FLAG
         else
            I := I - 1;
         end if;
      end Proc;
   end Foo;

.. _Same_Instantiations:

``Same_Instantiations``
^^^^^^^^^^^^^^^^^^^^^^^

.. index:: Same_Instantiations

Flag each generic package instantiation when it can be determined that
a set of the ``gnatcheck`` argument sources contains an instantiation
of the same generic with the same actual parameters. This determination
is conservative, it checks only for the following matching parameters:

*
  integer, character and string literals;

*
  Ada names that denote the same entity.

If some instantiation if marked by the rule, additional investigation
is required to decide if one of the duplicated instantiations can be
removed to simplify the code. In particular, the rule does not check if
these instantiations declare any global variable or perform some
non-trivial actions as a part of their elaboration.

This rule has no parameters.

.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 10, 17

   generic
      type T is private;
      X : Integer;
   package Gen is
   end Gen;

   with Gen;

   package Inst1 is
      package Inst_1 is new Gen (Integer, 2);  --  FLAG
      package Inst_2 is new Gen (Integer, 3);  --  NO FLAG
   end Inst1;

   with Gen;

   package Inst2 is
      package Inst_3 is new Gen (Integer, 2);  --  FLAG
   end Inst2;



.. _Too_Many_Generic_Dependencies:

``Too_Many_Generic_Dependencies``
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: Too_Many_Generic_Dependencies

Flags a ``WITH`` clause that mentions a
generic unit that in turn directly depends (mentions in its ``WITH``
clause) on another generic unit, and so on, and the length of the
chain of these dependencies on generics is more than N where N is
a rule parameter.

This rule has the following (mandatory) parameter for the ``+R`` option:



*N*
  Non-negative integer specifying the maximal allowed length of the
  chain of dependencies on generic units.

.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 20

   generic
   package D is
   end D;

   with D;
   generic
   package C is
   end C;

   with C;
   generic
   package B is
   end B;

   with B;
   generic
   package A is
   end A;

   with A;        --  FLAG (if N <= 3)
   package P is
      procedure Proc;
   end P;



.. _Programming_Practice:

Programming Practice
--------------------

.. index:: Programming Practice related rules

The rules in this subsection may be used to enforce feature usages that
relate to program maintainability.

.. _Access_To_Local_Objects:

``Access_To_Local_Objects``
^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: Access_To_Local_Objects

Flag any ``'Access`` attribute reference if its prefix denotes an identifier
defined by a local object declaration or a subcomponent thereof. An object
declaration is considered as local if it is located anywhere except library-level
packages or bodies of library-level packages (including packages nested
in those). Here both package declarations and package instantiations are
considered as packages. If the attribute prefix is a dereference or
a subcomponent thereof, the attribute reference is not flagged.

This rule has no parameters.

.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 5

   package body Pack
      procedure Proc is
         type Int_A is access all Integer;
         Var1 : aliased Integer;
         Var2 :         Int_A := Var1'Access;  --  FLAG

.. _Address_Attribute_For_Non_Volatile_Objects:

``Address_Attribute_For_Non_Volatile_Objects``
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: Address_Attribute_For_Non_Volatile_Objects

Flag any 'Address attribute reference if its prefix denotes a data
object defined by a variable object declaration and this object is not
marked as Volatile. An entity is considered as being marked volatile
if it has an aspect Volatile, Atomic or Shared declared for it.

This rule has no parameters.

.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 5

   Var1 : Integer with Volatile;
   Var2 : Integer;

   X : Integer with Address => Var1'Address;
   Y : Integer with Address => Var2'Address;   --  FLAG


.. _Address_Specifications_For_Initialized_Objects:

``Address_Specifications_For_Initialized_Objects``
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: Address_Specifications_For_Initialized_Objects

Flag address clauses and address aspect definitions if they are applied
to object declarations with explicit initializations.

This rule has no parameters.

.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 5

   I : Integer := 0;
   Var0 : Integer with Address => I'Address;

   Var1 : Integer := 10;
   for Var1'Address use Var0'Address;             --  FLAG

.. _Address_Specifications_For_Local_Objects:

``Address_Specifications_For_Local_Objects``
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: Address_Specifications_For_Local_Objects

Flag address clauses and address aspect definitions if they are applied
to data objects declared in local subprogram bodies. Data objects
declared in library subprogram bodies are not flagged.

This rule has no parameters.

.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 7

   package Pack is
      Var : Integer;
      procedure Proc (I : in out Integer);
   end Pack;
   package body Pack is
      procedure Proc (I : in out Integer) is
         Tmp : Integer with Address => Pack.Var'Address;   --  FLAG
      begin
         I := Tmp;
      end Proc;
   end Pack;


.. _Anonymous_Arrays:

``Anonymous_Arrays``
^^^^^^^^^^^^^^^^^^^^

.. index:: Anonymous_Arrays

Flag all anonymous array type definitions (by Ada semantics these can only
occur in object declarations).

This rule has no parameters.

.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 3

   type Arr is array (1 .. 10) of Integer;
   Var1 : Arr;
   Var2 : array (1 .. 10) of Integer;      --  FLAG


.. _Binary_Case_Statements:

``Binary_Case_Statements``
^^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: Binary_Case_Statements

Flag a case statement if this statement has only two alternatives, one
containing exactly one choice, the other containing exactly one choice
or the ``OTHERS`` choice.

This rule has no parameters.

.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 1

   case Var is                   --  FLAG
      when 1 =>
         Var := Var + 1;
      when others =>
         null;
   end case;

.. _Boolean_Negations:

``Boolean_Negations``
^^^^^^^^^^^^^^^^^^^^^

.. index:: Boolean_Negations

Flag any infix call to the predefined ``NOT`` operator for the predefined
Boolean type if its argument is an infix call to a predefined relation
operator or another call to the predefined ``NOT`` operator. Such expressions
can be simplified by excluding the outer call to the predefined ``NOT``
operator. Calls to ``NOT`` operators for the types derived from
Standard.Boolean are not flagged.

This rule has no parameters.

.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 1

   Is_Data_Available := not (Buffer_Length = 0);   --  FLAG


.. _Constant_Overlays:

``Constant_Overlays``
^^^^^^^^^^^^^^^^^^^^^

.. index:: Constant_Overlays

Flag an overlay definition that has a form of an attribute definition
clause ``for Overlaying'Address use Overlaid'Address;`` or a form of aspect definition
``Address => Overlaid'Address``, and ``Overlaid`` is a data object defined by a constant
declaration  or a formal or generic formal parameter of mode ``IN`` if
at least one of the following is true:

*
   the overlaying object is not a constant object;

*
   overlaying object or overlaid object is marked as Volatile;

This rule has no parameters.

.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 3

   C : constant Integer := 1;
   V : Integer;
   for V'Address use C'Address;    --  FLAG

.. _Default_Values_For_Record_Components:

``Default_Values_For_Record_Components``
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: Default_Values_For_Record_Components

Flag a record component declaration if it contains a default expression.
Do not flag record component declarations in protected definitions.
Do not flag discriminant specifications.

This rule has no parameters.

.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 2, 7

   type Rec (D : Natural := 0) is record
      I : Integer := 0;                    -- FLAG
      B : Boolean;

      case D is
         when 0 =>
            C : Character := 'A';          -- FLAG
         when others =>
            F : Float;
      end case;
   end record;


.. _Deriving_From_Predefined_Type:

``Deriving_From_Predefined_Type``
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: Deriving_From_Predefined_Type

Flag derived type declaration if the ultimate ancestor type is a
predefined Ada type. Do not flag record extensions and private
extensions. The rule is checked inside expanded generics.

This rule has no parameters.

.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 3, 5

   package Foo is
      type T is private;
      type My_String is new String;  --  FLAG
   private
      type T is new Integer;         --  FLAG
   end Foo;


.. _Duplicate_Branches:

``Duplicate_Branches``
^^^^^^^^^^^^^^^^^^^^^^

.. index:: Duplicate_Branches

Flag a sequence of statements that is a component of an ``IF`` statement
or of a ``CASE`` statement alternative, if the same ``IF`` or ``CASE``
statement contains another sequence of statements as its component
(or a component of its ``CASE`` statement alternative) that is
syntactically equivalent to the sequence of statements in question.
The check for syntactical equivalence of operands is case-sensitive,
it does not take into account line breaks, white spaces and
comments.

Small sequences of statements are not flagged by this rule. The rule has
two optional parameters that allow to specify the maximal size of statement
sequences that are not flagged:

* min_stmt=N

  N is an integer literal. All statement sequences that
  contain more than N statements (`Stmt` as defined in Libadalang) as
  subcomponents are flagged;

* min_size=N

  N is an integer literal. All statement sequences that
  contain more than N lexical elements (`SingleTokNode` in Libadalang terms)
  are flagged.

If at least one of the two thresholds specified by the rule parameters is
exceeded, a statement sequence is flagged. The following defaults are used:
``min_stmt=4,min_size=14``.


.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 2, 11

   if X > 0 then
      declare       --  FLAG: code duplicated at line 11
         A : Integer := X;
         B : Integer := A + 1;
         C : Integer := B + 1;
         D : Integer := C + 1;
      begin
         return D;
      end;
   else
      declare
         A : Integer := X;
         B : Integer := A + 1;
         C : Integer := B + 1;
         D : Integer := C + 1;
      begin
         return D;
      end;
   end if;


.. _Enumeration_Ranges_In_CASE_Statements:

``Enumeration_Ranges_In_CASE_Statements``
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: Enumeration_Ranges_In_CASE_Statements

Flag each use of a range of enumeration literals as a choice in a
``case`` statement.
All forms for specifying a range (explicit ranges
such as ``A .. B``, subtype marks and ``'Range`` attributes) are flagged.
An enumeration range is
flagged even if contains exactly one enumeration value or no values at all. A
type derived from an enumeration type is considered as an enumeration type.

This rule helps prevent maintenance problems arising from adding an
enumeration value to a type and having it implicitly handled by an existing
``case`` statement with an enumeration range that includes the new literal.

This rule has no parameters.

.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 8, 10

   procedure Bar (I : in out Integer) is
      type Enum is (A, B, C, D, E);
      type Arr is array (A .. C) of Integer;

      function F (J : Integer) return Enum is separate;
   begin
      case F (I) is
         when Arr'Range  =>  --  FLAG
            I := I + 1;
         when D .. E =>      --  FLAG
            null;
      end case;
   end Bar;


.. _Enumeration_Representation_Clauses:

``Enumeration_Representation_Clauses``
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: Enumeration_Representation_Clauses

Flag enumeration representation clauses.

This rule has no parameters.

.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 2

   type Enum1 is (A1, B1, C1);
   for Enum1 use (A1 => 1, B1 => 11, C1 => 111);     --  FLAG


.. _Exceptions_As_Control_Flow:

``Exceptions_As_Control_Flow``
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: Exceptions_As_Control_Flow

Flag each place where an exception is explicitly raised and handled in the
same subprogram body. A ``raise`` statement in an exception handler,
package body, task body or entry body is not flagged.

The rule has no parameters.

.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 5

   procedure Bar (I : in out Integer) is

   begin
      if I = Integer'Last then
         raise Constraint_Error;    --  FLAG
      else
        I := I - 1;
      end if;
   exception
      when Constraint_Error =>
         I := Integer'First;
   end Bar;


.. _Exits_From_Conditional_Loops:

``Exits_From_Conditional_Loops``
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: Exits_From_Conditional_Loops

Flag any exit statement if it transfers the control out of a ``for`` loop
or a ``while`` loop. This includes cases when the ``exit`` statement
applies to a ``FOR`` or ``while`` loop, and cases when it is enclosed
in some ``for`` or ``while`` loop, but transfers the control from some
outer (unconditional) ``loop`` statement.

The rule has no parameters.

.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 5

   function Bar (S : String) return Natural is
      Result : Natural := 0;
   begin
      for J in S'Range loop
         exit when S (J) = '@';  --  FLAG
         Result := Result + J;
      end loop;

      return 0;
   end Bar;


.. _EXIT_Statements_With_No_Loop_Name:

``EXIT_Statements_With_No_Loop_Name``
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: EXIT_Statements_With_No_Loop_Name

Flag each ``exit`` statement that does not specify the name of the loop
being exited.

This rule has the following (optional) parameter for the +R option:

*Nested_Only*
  Flag only those exit statements with no loop name that exit from nested loops.

.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 4

   procedure Bar (I, J : in out Integer) is
   begin
      loop
         exit when I < J;  --  FLAG
         I := I - 1;
         J := J + 1;
      end loop;
   end Bar;


.. _Global_Variables:

``Global_Variables``
^^^^^^^^^^^^^^^^^^^^

.. index:: Global_Variables

Flag any variable declaration that appears immediately within the
specification of a library package or library generic package. Variable
declarations in nested packages and inside package instantiations are
not flagged.

This rule has the following (optional) parameters for the ``+R`` option:

*Only_Public*
  Do not flag variable declarations in private library (generic) packages and
  in package private parts.

.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 2, 5

   package Foo is
       Var1 : Integer;    --  FLAG
       procedure Proc;
   private
       Var2 : Boolean;    --  FLAG
   end Foo;



.. _GOTO_Statements:

``GOTO_Statements``
^^^^^^^^^^^^^^^^^^^

.. index:: GOTO_Statements

Flag each occurrence of a ``goto`` statement.

This rule has no parameters.

.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 3

   for K in 1 .. 10 loop
      if K = 6 then
         goto Quit; -- FLAG
      end if;
      null;
   end loop;
   <<Quit>>
   return;



.. _Integer_Types_As_Enum:

``Integer_Types_As_Enum``
^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: Integer_Types_As_Enum

Flag each integer type declaration (including types derived from
integer types) if this integer type may benefit from
being replaced by an enumeration type. An integer type is considered
as being potentially replaceable by an enumeration type if all the
following conditions are true:

*
  there is no infix calls to any arithmetic operator for objects
  of this type;

*
  this type is not referenced in an actual parameter of a generics
  instantiation;

*
  there is no type conversion from or to this type;

*
  no type is derived from this type;

*
  no subtype is declared for this type.

This rule has no parameters.

.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 2

   procedure Proc is
      type Enum is range 1 .. 3;    --  FLAG
      type Int is range 1 .. 3;     --  NO FLAG

      X : Enum := 1;
      Y : Int := 1;
   begin
      X := 2;
      Y := Y + 1;
   end Proc;


.. _Improper_Returns:

``Improper_Returns``
^^^^^^^^^^^^^^^^^^^^

.. index:: Improper_Returns

Flag each explicit ``return`` statement in procedures, and
multiple ``return`` statements in functions.
Diagnostic messages are generated for all ``return`` statements
in a procedure (thus each procedure must be written so that it
returns implicitly at the end of its statement part),
and for all ``return`` statements in a function after the first one.
This rule supports the stylistic convention that each subprogram
should have no more than one point of normal return.

This rule has no parameters.

.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 4, 15, 19

   procedure Proc (I : in out Integer) is
   begin
      if I = 0 then
         return;                          --  FLAG
      end if;

      I := I * (I + 1);
   end Proc;

   function Factorial (I : Natural) return Positive is
   begin
      if I = 0 then
         return 1;
      else
         return I * Factorial (I - 1);    --  FLAG
      end if;
   exception
      when Constraint_Error =>
         return Natural'Last;             --  FLAG
   end Factorial;


.. _Local_USE_Clauses:

``Local_USE_Clauses``
^^^^^^^^^^^^^^^^^^^^^

.. index:: Local_USE_Clauses

Use clauses that are not parts of compilation unit context clause are
flagged. The rule has an optional parameter for +R option:

*Except_USE_TYPE_Clauses*
  Do not flag local use type clauses.

.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 4, 7

   with Pack1;
   with Pack2;
   procedure Proc is
      use Pack1;               --  FLAG

      procedure Inner is
         use type Pack2.T;     --  FLAG (if Except_USE_TYPE_Clauses is not set)
      ...


.. _Maximum_Lines:

``Maximum_Lines``
^^^^^^^^^^^^^^^^^

.. index:: Maximum_Lines

Flags the file containing the source text of a compilation unit if this
file contains more than N lines where N is a rule parameter

This rule has the following (mandatory) parameters for the ``+R`` option:

*N*
  Positive integer specifying the maximum allowed number of lines in
  the compilation unit source text.


.. _Maximum_OUT_Parameters:

``Maximum_OUT_Parameters``
^^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: Maximum_OUT_Parameters

Flag any subprogram declaration, subprogram body declaration, expression
function declaration, null procedure declaration, subprogram
body stub or generic subprogram declaration if the corresponding
subprogram has more than *N* formal parameters of mode ``IN`` or
``IN OUT``, where *N* is a parameter of the rule.

A subprogram body, an expression function, a null procedure or
a subprogram body stub is flagged only if there is
no separate declaration for this subprogram. Subprogram renaming
declarations and subprogram instantiations, as well as declarations
inside expanded generic instantiations are never flagged.

This rule has the following (mandatory) parameters for the ``+R`` option:



*N*
  Positive integer specifying the maximum allowed total number of
  subprogram formal parameters of modes ``IN`` and ``IN OUT``.

.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 4

   procedure Proc_1 (I : in out Integer);          --  NO FLAG
   procedure Proc_2 (I, J : in out Integer);       --  NO FLAG
   procedure Proc_3 (I, J, K : in out Integer);    --  NO FLAG
   procedure Proc_4 (I, J, K, L : in out Integer); --  FLAG (if rule parameter is 3)


.. _Maximum_Parameters:

``Maximum_Parameters``
^^^^^^^^^^^^^^^^^^^^^^

.. index:: Maximum_Parameters

Flag any subprogram declaration, subprogram body declaration, expression
function declaration, null procedure declaration, subprogram
body stub or generic subprogram declaration if the corresponding
subprogram has more than *N* formal parameters, where *N* is a
parameter of the rule.

A subprogram body, an expression function, a null procedure or
a subprogram body stub is flagged only if there is
no separate declaration for this subprogram. Subprogram renaming
declarations and subprogram instantiations, as well as declarations
inside expanded generic instantiations are never flagged.

This rule has the following (mandatory) parameters for the ``+R`` option:



*N*
  Positive integer specifying the maximum allowed total number of
  subprogram formal parameters.

.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 6, 8

   package Foo is

      procedure Proc_1 (I : in out Integer);
      procedure Proc_2 (I, J : in out Integer);
      procedure Proc_3 (I, J, K : in out Integer);
      procedure Proc_4 (I, J, K, L : in out Integer); --  FLAG (if rule parameter is 3)

      function Fun_4                                  --  FLAG (if rule parameter is 3)
        (I : Integer;
         J : Integer;
         K : Integer;
         L : Integer) return Integer is (I + J * K - L);

   end Foo;

.. _Misplaced_Representation_Items:

``Misplaced_Representation_Items``
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: Misplaced_Representation_Items

Flag a representation item if there is any Ada construct except
another representation item for the same entity between this clause
and the declaration of the entity it applies to. A representation item
in the context of this rule is either a representation clause or one of
the following representation pragmas:

*
  Atomic   J.15.8(9/3)

*
  Atomic_Components   J.15.8(9/3)

*
  Independent   J.15.8(9/3)

*
  Independent_Components   J.15.8(9/3)

*
  Pack   J.15.3(1/3)

*
  Unchecked_Union   J.15.6(1/3)

*
  Volatile   J.15.8(9/3)

*
  Volatile_Components   J.15.8(9/3)

This rule has no parameters.

.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 5

   type Int1 is range 0 .. 1024;
   type Int2 is range 0 .. 1024;

   for Int2'Size use 16;         --  NO FLAG
   for Int1'Size use 16;         --  FLAG



.. _Nested_Subprograms:

``Nested_Subprograms``
^^^^^^^^^^^^^^^^^^^^^^

.. index:: Nested_Subprograms

Flag any subprogram declaration, subprogram body declaration, subprogram
instantiation, expression function declaration or subprogram body stub
that is not a completion of another subprogram declaration and that is
declared within subprogram body (including bodies of generic
subprograms), task body or entry body directly or indirectly (that is -
inside a local nested package). Protected subprograms are not flagged.
Null procedure declarations are not flagged. Procedure declarations
completed by null procedure declarations are not flagged.

This rule has no parameters.

.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 4, 6

   procedure Bar (I, J : in out Integer) is

      procedure Foo (K : Integer) is null;
      procedure Proc1;                    --  FLAG

      procedure Proc2 is separate;        --  FLAG

      procedure Proc1 is
      begin
         I := I + J;
      end Proc1;

   begin



.. _No_Others_In_Exception_Handlers:

``No_Others_In_Exception_Handlers``
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: No_Others_In_Exception_Handlers

Flag handled sequences of statements that do not contain exception
handler with ``OTHERS``, depending on the rule parameter(s)
specified.

This rule has three parameters for +R option:

*
  Subprogram

    Flag a subprogram body if the handled sequence of statements
    of this body does not contain an exception handler with
    ``OTHERS`` choice. This includes the case when the body does
    not contain any exception handler at all. The diagnostic
    message points to the beginning of the subprogram body.

*
  Task

    Flag a task body if the handled sequence of statements
    of this body does not contain an exception handler with
    ``OTHERS`` choice. This includes the case when the body does
    not contain any exception handler at all. The diagnostic
    message points to the beginning of the task body.

*
  All_Handlers

   Flag a handled sequence of statements if it does contain
   at least one exception handler, but it does not contain an
   exception handler with ``OTHERS`` choice. If a handled sequence
   of statements does not have any exception handler, nothing is
   flagged for it. The diagnostic  message points to the
   ``EXCEPTION`` keyword in the handled sequence of statements.

At least one parameter should be specified for the rule. If
more than one parameter is specified, each of the specified
parameters has its effect.

.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 5

   procedure Other (I, J : in out Integer) is
   begin
      begin
         I := I + 1;
      exception                --  FLAG (if All_Handlers parameter is set)
         when Constraint_Error => null;
      end;

   exception                    --  NO FLAG
      when Constraint_Error =>
         I := Integer'Last;
      when others =>
         I := J;
         raise;
   end Other;


.. _Non_Component_In_Barriers:

``Non_Component_In_Barriers``
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: Non_Component_In_Barriers

Flag a barrier condition expression in an entry body declaration
if this expression contains a reference to a data object that is
not a (sub)component of the enclosing record the entry belongs to.

This rule has no parameters.

.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 21

   protected Obj is
      entry E1;
      entry E2;
   private
      Value  : Integer;
      Is_Set : Boolean := False;
   end Obj;

   Global_Bool : Boolean := False;

   protected body Obj is

      entry E1
         when Is_Set and then Value > 0 is  --  NO FLAG
      begin
         Value  := Value - 1;
         Is_Set := False;
      end E1;

      entry E2
         when Global_Bool is                --  FLAG
      begin
         Is_Set := True;
      end E2;

   end Obj;





.. _Non_Constant_Overlays:

``Non_Constant_Overlays``
^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: Non_Constant_Overlays

Flag an overlay definition that has a form of an attribute definition
clause ``for Overlaying'Address use Overlaid'Address;`` or a form of
aspect definition ``Address => Overlaid'Address``, and ``Overlaid``
is a data object defined by a variable declaration , a formal parameter
of mode ``IN OUT`` or ``OUT`` or a generic formal parameter of mode ``IN OUT``
if at least one of the following is true:

*
  the overlaying object is a constant object;

*
  overlaying object is not marked as Volatile;

*
  if overlaid object is not a parameter, it is not marked as Volatile;

This rule has no parameters.

.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 3

   V : Integer with Volatile;
   C : constant Integer := 1;
   for C'Address use V'Address;    --  FLAG


.. _Nonoverlay_Address_Specifications:

``Nonoverlay_Address_Specifications``
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: Nonoverlay_Address_Specifications

Flag an attribute definition clause that defines ``'Address`` attribute if
it does not have the form ``for Overlaying'Address use Overlaid'Address;``
where ``Overlaying`` is an identifier defined by an object declaration
and ``Overlaid`` is an identifier defined either by an object declaration
or a parameter specification. Flag an Address aspect specification if
this aspect specification is not a part of an object declaration and
if the aspect value does not have the form ``Overlaid'Address``
where ``Overlaid`` is an identifier defined either by an object
declaration or a parameter specification.

Address specifications given for program units are not flagged.

This rule has no parameters.

.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 9

   type Rec is record
      C : Integer;
   end record;

   Var_Rec : Rec;
   Var_Int : Integer;

   Var1 : Integer with Address => Var_Int'Address;
   Var2 : Integer with Address => Var_Rec.C'Address;  --  FLAG


.. _Non_Short_Circuit_Operators:

``Non_Short_Circuit_Operators``
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: Non_Short_Circuit_Operators

Flag all calls to predefined ``and`` and ``or`` operators for
any boolean type. Calls to
user-defined ``and`` and ``or`` and to operators defined by renaming
declarations are not flagged. Calls to predefined ``and`` and ``or``
operators for modular types or boolean array types are not flagged.

The rule has an optional parameter for +R option:

*Except_Assertions*
  Do not flag the use of non-short-circuit_operators inside
  assertion-related pragmas or aspect specifications.

A pragma or an aspect is considered as assertion-related if its name
is from the following list:

*
  ``Assert``
*
  ``Assert_And_Cut``
*
  ``Assume``
*
  ``Contract_Cases``
*
  ``Debug``
*
  ``Default_Initial_Condition``
*
  ``Dynamic_Predicate``
*
  ``Invariant``
*
  ``Loop_Invariant``
*
  ``Loop_Variant``
*
  ``Post``
*
  ``Postcondition``
*
  ``Pre``
*
  ``Precondition``
*
  ``Predicate``
*
  ``Predicate_Failure``
*
  ``Refined_Post``
*
  ``Static_Predicate``
*
  ``Type_Invariant``


.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 1, 3

   B1 := I > 0 and J > 0;       --  FLAG
   B2 := I < 0 and then J < 0;
   B3 := I > J or J > 0;        --  FLAG
   B4 := I < J or else I < 0;


.. _Not_Imported_Overlays:

``Not_Imported_Overlays``
^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: Not_Imported_Overlays

Flag an attribute definition clause that defines 'Address attribute and
has the form ``for Overlaying'Address use Overlaid'Address;`` where
``Overlaying`` and ``Overlaid`` are identifiers
both defined by object declarations if ``Overlaying`` is not marked as
imported. Flag an Address aspect specification if this aspect specification
is a part of an object declaration of the object ``Overlaying`` and
if the aspect value has the form ``Overlaid'Address`` where ``Overlaid``
is an identifier defined by an object declaration if the object ``Overlaying``
is not marked as imported.

This rule has no parameters.

.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 4

   package Pack is
      I : Integer;

      J : Integer with Address => I'Address;            --  FLAG

      L : Integer;
      for L'Address use I'Address;                      --  NO FLAG
      pragma Import (C, L);
   end Pack;




.. _Null_Paths:

``Null_Paths``
^^^^^^^^^^^^^^

.. index:: Null_Paths

Flag a statement sequence that is a component of an IF, CASE or LOOP
statement if this sequences consists of NULL statements only.

This rule has no parameters.

.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 4, 13, 17

   if I > 10 then
      J := 5;
   elsif I > 0 then
      null;                 --  FLAG
   else
     J := J + 1;
   end if;

   case J is
      when 1 =>
         I := I + 1;
      when 2 =>
         null;              --  FLAG
      when 3 =>
         J := J + 1;
      when others =>
         null;              --  FLAG
   end case;


.. _Objects_Of_Anonymous_Types:

``Objects_Of_Anonymous_Types``
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: Objects_Of_Anonymous_Types

Flag any object declaration located immediately within a package
declaration or a package body (including generic packages) if it uses
anonymous access or array type definition. Record component definitions
and parameter specifications are not flagged. Formal object declarations
defined with anonymous access definitions are flagged.

This rule has no parameters.

.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 5, 8, 12

   package Foo is
      type Arr is array (1 .. 10) of Integer;
      type Acc is access Integer;

      A : array (1 .. 10) of Integer;  --  FLAG
      B : Arr;

      C : access Integer;              --  FLAG
      D : Acc;

      generic
         F1 : access Integer;          --  FLAG
         F2 : Acc;
      procedure Proc_G
        (P1 : access Integer;
         P2 : Acc);
   end Foo;


.. _Operator_Renamings:

``Operator_Renamings``
^^^^^^^^^^^^^^^^^^^^^^

.. index:: Operator_Renamings

Flag subprogram renaming declarations that have an operator symbol as
the name of renamed subprogram.

This rule has no parameters.

.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 1

   function Foo (I, J : Integer)           --  FLAG
     return Integer renames Standard."+";
   function "-" (I, J : Integer)           --  NO FLAG
     return Integer renames Bar;

.. _OTHERS_In_Aggregates:

``OTHERS_In_Aggregates``
^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: OTHERS_In_Aggregates

Flag each use of an ``others`` choice in extension aggregates.
In record and array aggregates, an ``others`` choice is flagged unless
it is used to refer to all components, or to all but one component.

If, in case of a named array aggregate, there are two associations, one
with an ``others`` choice and another with a discrete range, the
``others`` choice is flagged even if the discrete range specifies
exactly one component; for example, ``(1..1 => 0, others => 1)``.

This rule has no parameters.

.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 22, 25, 29

   package Foo is
      type Arr is array (1 .. 10) of Integer;

      type Rec is record
         C1 : Integer;
         C2 : Integer;
         C3 : Integer;
         C4 : Integer;
      end record;

      type Tagged_Rec is tagged record
         C1 : Integer;
      end record;

      type New_Tagged_Rec is new Tagged_Rec with record
         C2 : Integer;
         C3 : Integer;
         C4 : Integer;
      end record;

      Arr_Var1 : Arr := (others => 1);
      Arr_Var2 : Arr := (1 => 1, 2=> 2, others => 0);  --  FLAG

      Rec_Var1 : Rec := (C1 => 1, others => 0);
      Rec_Var2 : Rec := (1, 2, others => 3);           --  FLAG

      Tagged_Rec_Var : Tagged_Rec := (C1 => 1);

      New_Tagged_Rec_Var : New_Tagged_Rec := (Tagged_Rec_Var with others => 0); -- FLAG
   end Foo;


.. _OTHERS_In_CASE_Statements:

``OTHERS_In_CASE_Statements``
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: OTHERS_In_CASE_Statements

Flag any use of an ``others`` choice in a ``case`` statement.

This rule has no parameters.

.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 6

   case J is
      when 1 =>
         I := I + 1;
      when 3 =>
         J := J + 1;
      when others =>        --  FLAG
         null;
   end case;


.. _OTHERS_In_Exception_Handlers:

``OTHERS_In_Exception_Handlers``
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: OTHERS_In_Exception_Handlers

Flag any use of an ``others`` choice in an exception handler.

This rule has no parameters.

.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 4

   exception
      when Constraint_Error =>
         I:= Integer'Last;
      when others =>                   --  FLAG
         I := I_Old;
         raise;


.. _Outbound_Protected_Assignments:

``Outbound_Protected_Assignments``
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: Outbound_Protected_Assignments

Flag an assignment statement located in a protected body if the
variable name in the left part of the statement denotes an object
declared outside this protected type or object.

This rule has no parameters.

.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 17

   package Pack is
      Var : Integer;

      protected P is
         entry E (I : in out Integer);
         procedure P (I : Integer);
      private
         Flag : Boolean;
      end P;

   end Pack;
   package body Pack is
      protected body P is
         entry E (I : in out Integer) when Flag is
         begin
            I   := Var + I;
            Var := I;           --  FLAG
         end E;

         procedure P (I : Integer) is
         begin
            Flag := I > 0;
         end P;
      end P;
   end Pack;

.. _Outside_References_From_Subprograms:

``Outside_References_From_Subprograms``
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: Outside_References_From_Subprograms

Within a subprogram body or an expression function flag any identifier
that denotes a data object declared outside this body. In case of bodies
of generic subprograms or subprograms declared in generic packages, this
also includes references to generic formal objects. If a referenced
object is global, such a reference is not flagged.

Any reference to a formal object (it can be only in a body of a formal
subprogram or a subprogram declared in a generic body) is considered as
outside reference (and therefore is flagged) because in any
instantiation it will be replaced by the reference to an entity that in
no case can be declared in the subprogram body being checked.

This rule has no parameters.

.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 4

   Var : Integer;
   procedure Proc (I : in out Integer) is
   begin
      I := I + Var;      --  FLAG

.. _Overly_Nested_Control_Structures:

``Overly_Nested_Control_Structures``
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: Overly_Nested_Control_Structures

Flag each control structure whose nesting level exceeds the value provided
in the rule parameter.

The control structures checked are the following:

* ``if`` statement
* ``case`` statement
* ``loop`` statement
* selective accept statement
* timed entry call statement
* conditional entry call statement
* asynchronous select statement

The rule has the following parameter for the ``+R`` option:



*N*
  Positive integer specifying the maximal control structure nesting
  level that is not flagged

If the parameter for the ``+R`` option is not specified or
if it is not a positive integer, ``+R`` option is ignored.

If more than one  option is specified for the gnatcheck call,
the later option and new parameter override the previous one(s).

.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 6

   if I > 0 then
       for Idx in I .. J loop
          if J < 0 then
             case I is
                when 1 =>
                   if Idx /= 0 then  --  FLAG (if rule parameter is 3)
                      J := J / Idx;
                   end if;
                when others =>
                   J := J + Idx;
             end case;
          end if;
       end loop;
   end if;


.. _POS_On_Enumeration_Types:

``POS_On_Enumeration_Types``
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: POS_On_Enumeration_Types

Flag ``'Pos`` attribute in case if the attribute prefix has an enumeration
type (including types derived from enumeration types).

This rule has no parameters.

.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 3, 5, 7

   procedure Bar (Ch1, Ch2 : Character; I : in out Integer) is
   begin
      if Ch1'Pos in 32 .. 126           --  FLAG
        and then
         Ch2'Pos not in 0 .. 31         --  FLAG
      then
         I := (Ch1'Pos + Ch2'Pos) / 2;  --  FLAG (twice)
      end if;
   end Bar;


.. _Positional_Actuals_For_Defaulted_Generic_Parameters:

``Positional_Actuals_For_Defaulted_Generic_Parameters``
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: Positional_Actuals_For_Defaulted_Generic_Parameters

Flag each generic actual parameter corresponding to a generic formal
parameter with a default initialization, if positional notation is used.

This rule has no parameters.

.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 23-25

   package Foo is
      function Fun_1 (I : Integer) return Integer;
      function Fun_2 (I : Integer) return Integer;

      generic
         I_Par1 : Integer;
         I_Par2 : Integer := 1;
         with function Fun_1 (I : Integer) return Integer is <>;
         with function Fun_3 (I : Integer) return Integer is Fun_2;
      package Pack_G is
         Var_1 : Integer := I_Par1;
         Var_2 : Integer := I_Par2;
         Var_3 : Integer := Fun_1 (Var_1);
         Var_4 : Integer := Fun_3 (Var_2);
      end Pack_G;

      package Pack_I_1 is new Pack_G (1);

      package Pact_I_2 is new Pack_G
        (2, I_Par2 => 3, Fun_1 => Fun_2, Fun_3 => Fun_1);

      package Pack_I_3 is new Pack_G (1,
                                      2,            --  FLAG
                                      Fun_2,        --  FLAG
                                      Fun_1);       --  FLAG

   end Foo;


.. _Positional_Actuals_For_Defaulted_Parameters:

``Positional_Actuals_For_Defaulted_Parameters``
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: Positional_Actuals_For_Defaulted_Parameters

Flag each actual parameter to a subprogram or entry call where the
corresponding formal parameter has a default expression, if positional
notation is used.

This rule has no parameters.

.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 7

      procedure Proc (I : in out Integer; J : Integer := 0) is
      begin
         I := I + J;
      end Proc;

   begin
      Proc (Var1, Var2);   --  FLAG


.. _Positional_Components:

``Positional_Components``
^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: Positional_Components

Flag each array, record and extension aggregate that includes positional
notation.

This rule has no parameters.

.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 11, 12, 15

   package Foo is
      type Arr is array (1 .. 10) of Integer;

      type Rec is record
         C_Int  : Integer;
         C_Bool : Boolean;
         C_Char : Character;
      end record;

      Var_Rec_1 : Rec := (C_Int => 1, C_Bool => True, C_Char => 'a');
      Var_Rec_2 : Rec := (2, C_Bool => False, C_Char => 'b');   --  FLAG
      Var_Rec_3 : Rec := (1, True, 'c');                        --  FLAG

      Var_Arr_1 : Arr := (1 => 1, others => 10);
      Var_Arr_2 : Arr := (1, others => 10);                     --  FLAG
   end Foo;


.. _Positional_Generic_Parameters:

``Positional_Generic_Parameters``
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: Positional_Generic_Parameters

Flag each positional actual generic parameter except for the case when
the generic unit being instantiated has exactly one generic formal
parameter.

This rule has no parameters.

.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 10

   with Ada.Text_IO; use Ada.Text_IO;
   with Ada.Unchecked_Conversion;
   procedure Bar (I : in out Integer) is
      type My_Int is range -12345 .. 12345;

      function To_My_Int is new Ada.Unchecked_Conversion
        (Source => Integer, Target => My_Int);

      function To_Integer is new Ada.Unchecked_Conversion
        (My_Int, Integer);                                --  FLAG (twice)

      package My_Int_IO is new  Ada.Text_IO.Integer_IO (My_Int);


.. _Positional_Parameters:

``Positional_Parameters``
^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: Positional_Parameters

Flag each positional parameter notation in a subprogram or entry call,
except for the following:

*
  Parameters of calls to attribute subprograms are not flagged;
*
  Parameters of prefix or infix calls to operator functions are not flagged;
*
  If the called subprogram or entry has only one formal parameter,
  the parameter of the call is not flagged;
*
  If a subprogram call uses the *Object.Operation* notation, then

  *
    the first parameter (that is, *Object*) is not flagged;
  *
    if the called subprogram has only two parameters, the second parameter
    of the call is not flagged;

This rule has the following (optional) parameters for the ``+R`` option:



*All*
  if this parameter is specified, all the positional parameter
  associations that can be replaced with named associations
  according to language rules are flagged, except parameters of
  the calls to operator functions.

.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 17, 21

   procedure Bar (I : in out Integer) is
      function My_Max (Left, Right : Integer) return Integer renames Integer'Max;

      procedure Proc1 (I : in out Integer) is
      begin
         I := I + 1;
      end Proc1;

      procedure Proc2 (I, J : in out Integer) is
      begin
         I := I + J;
      end Proc2;

      L, M : Integer := 1;
   begin
      Proc1 (L);
      Proc2 (L, M);                              --  FLAG (twice)
      Proc2 (I => M, J => L);

      L := Integer'Max (10, M);
      M := My_Max (100, Right => L);             --  FLAG

   end Bar;

.. _Profile_Discrepancies:

``Profile_Discrepancies``
^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: Profile_Discrepancies

Flag subprogram or entry body (or body stub) if its parameter (or
parameter and result) profile does not follow the lexical structure
of the profile in the corresponding subprogram or entry declaration.

This rule has no parameters.

.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 8

   package Pack is
      procedure Proc
        (I : Integer;
         J : Integer);
   end Pack;

   package body Pack is
      procedure Proc (I, J : Integer) is    --  FLAG

.. _Recursive_Subprograms:

``Recursive_Subprograms``
^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: Recursive_Subprograms

Flags specs (and bodies that act as specs) of recursive subprograms. A
subprogram is considered as recursive in a given context if there exists
a chain of direct calls starting from the body of, and ending at
this subprogram within this context. A context is provided by the set
of Ada sources specified as arguments of a given ``gnatcheck`` call.
Neither dispatching calls nor calls through access-to-subprograms
are considered as direct calls by this rule. If *Follow_Dispatching_Calls*
rule parameter is set, ``gnatcheck`` considers a dispatching call as a set
of calls to all the subprograms the dispatching call may dispatch to,
otherwise dispatching calls are ignored. The current rule limitation is
that when processing dispatching calls the rule does not take into account
type primitive operations declared in generic instantiations.

This rule does not take into account calls to subprograms whose
bodies are not available because of any reason (a subprogram is imported,
the Ada source containing the body is not provided as ``gnatcheck``
argument source etc.). The *Unavailable_Body_Calls* rule can be used to
detect these cases.

Generic subprograms and subprograms detected in generic units are not
flagged. Recursive subprograms in generic instantiations
are flagged.

The rule does not take into account implicit calls that are the
result of computing default initial values for an object or a subcomponent
thereof as a part of the elaboration of an object declaration.

The rule also does not take into account subprogram calls inside
aspect definitions.

The rule has an optional parameter for ``+R`` option:

*Follow_Dispatching_Calls*
   Treat a dispatching call as a set of calls to all the subprograms
   the dispatching call may dispatch to.

.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 1

   function Factorial (N : Natural) return Positive is  --  FLAG
   begin
      if N = 0 then
         return 1;
      else
         return N * Factorial (N - 1);
      end if;
   end Factorial;



.. _Restrictions:

``Restrictions``
^^^^^^^^^^^^^^^^

.. index:: Restrictions

Flags violations of Ada predefined and GNAT-specific restrictions
according to the rule parameter(s) specified.

``gnatcheck`` does not check Ada or GNAT restrictions itself, instead
it compiles an argument source with a configuration file that
defines restrictions of interest,
analyses the style warnings generated by the GNAT compiler and
includes the information about restriction violations detected into
the ``gnatcheck`` messages.

This rule allows parametric rule exemptions, the parameters
that are allowed in the definition of exemption sections are
the names of the restrictions except for the case when a restriction
requires a non-numeric parameter, in this case the parameter should be
the name of the restriction with the parameter, as it is given for the
rule.

The rule should have a parameter, the format of the rule parameter is the
same as the parameter of
the pragma ``Restrictions`` or ``Restriction_Warnings``.

If your code contains pragmas ``Warnings`` with parameter ``Off``, this may
result in false negatives for this rule, because the corresponding warnings
generated during compilation will be suppressed. The workaround is to
use for ``gnatcheck`` call a configuration file that
contains ``pragma Ignore_Pragma (Warnings);``.

.. warning:: Note, that some restriction checks cannot be performed by gnatcheck
   because they are either dynamic or require information from the code
   generation phase. For such restrictions gnatcheck generates the
   corresponding warnings and disables the ``Restrictions`` rules.

.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 1,6

   with Ada.Finalization;      --  FLAG (+RRestrictions:No_Dependence=>Ada.Finalization)
   procedure Proc is
      type Access_Integer is access Integer;
      Var : Access_Integer;
   begin
      Var := new Integer'(1);  --  FLAG (+RRestrictions:No_Allocators)
   end Proc;



.. _Same_Logic:

``Same_Logic``
^^^^^^^^^^^^^^

.. index:: Same_Logic

Flags expressions that contain a chain of infix calls to the same boolean
operator (``and``, ``or``, ``and then``, ``or else``, ``xor``) if an expression
contains syntactically equivalent operands. The check for syntactical
equivalence of operands is case-sensitive.

This rule has no parameters.


.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 2

   B := Var1 and Var2;            --  NO FLAG
   return A or else B or else A;  --  FLAG


.. _Same_Operands:

``Same_Operands``
^^^^^^^^^^^^^^^^^

.. index:: Same_Operands

Flags infix calls to binary operators ``/``, ``=``, ``/=``, ``>``, ``>=``,
``<``, ``<=``, ``-``, ``mod``, ``rem`` (except for the
calls to ``=`` and ``/=`` operators for floating point types) if operands
of a call are syntactically equivalent. The check for syntactical equivalence
of operands is case-sensitive.

This rule has no parameters.


.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 2

   Y := (X + 1) / (X - 1);        --  NO FLAG
   Z := (X + 1) / (X + 1);        --  FLAG


.. _Same_Tests:

``Same_Tests``
^^^^^^^^^^^^^^

.. index:: Same_Tests

Flags condition expressions in ``if`` statements or ``if`` expressions if
a statement or expression contains another condition expression that is
syntactically equivalent to the first one. The check for syntactical equivalence
is case-sensitive.

This rule has no parameters.

.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 1, 5

   if Str = A then                --  FLAG: same test at line 5
      Put_Line("Hello, tata!");
   elsif Str = B then
      Put_Line("Hello, titi!");
   elsif Str = A then
      Put_Line("Hello, toto!");
   else
      Put_Line("Hello, world!");
   end if;


.. _Silent_Exception_Handlers:

``Silent_Exception_Handlers``
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: Silent_Exception_Handlers

Flag any exception handler that neither raises an exception by a
``raise`` statement or a call to ``Ada.Exceptions.Raise_Exception`` or
to ``Ada.Exceptions.Reraise_Occurrence`` nor contains a call to some subprogram
specified by the rule parameters. The rule can have any number of parameters,
each parameter should be of one of the following kinds:

*
  a full qualified Ada name of a subprogram that starts from some
  root unit name (when gnatcheck compares full expanded Ada names
  of the called subprograms it does it in non-case-sensitive way);

*
  if a parameter has a format of an Ada string constant, then the content
  of this constant (without outer string quotes) is treated as a
  case-sensitive regular expression as defined in ``s-regpat.ads``.
  An exception handler is not flagged if it contains a call to a subprogram
  that has a full expanded Ada name that matches this regular
  expression.

Note that if you specify the rule with parameters in a command shell, you may
need to escape its parameters. The best and the safest way of using this rule
is to place it into a rule file and to use this rule file as a parameter of the
``-from=`` option, no escaping is needed in this case.


.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 12

   with Ada.Exceptions; use Ada.Exceptions;

   procedure Exc is
      procedure Log (Msg : String) with Import;
      --  Suppose the rule parameters are:
     --      ada.exceptions.exception_message,"\.Log$"
      I : Integer := 0;
   begin
      begin
         I := I + 1;
      exception
         when others =>   --  FLAG
            null;
      end;

   exception
      when Constraint_Error =>  --  NO FLAG
         raise;
      when Program_Error =>     --  NO FLAG
         Log ("");
      when E : others =>        --  NO FLAG
         I := 0;
         Log (Exception_Message (E));
   end Exc;


.. _Single_Value_Enumeration_Types:

``Single_Value_Enumeration_Types``
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: Single_Value_Enumeration_Types

Flag an enumeration type definition if it contains a single enumeration
literal specification

This rule has no parameters.

.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 2

   type Enum3 is (A, B, C);
   type Enum1 is (D);      --  FLAG


.. _Unavailable_Body_Calls:

``Unavailable_Body_Calls``
^^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: Unavailable_Body_Calls

Flag any subprogram call if the set of argument sources does not
contain a body of the called subprogram because of any reason.
Calls to formal subprograms in generic bodies are not flagged.
This rule can be useful as a complementary rule for the
*Recursive_Subprograms* rule - it flags potentially missing recursion
detection and identify potential missing checks.

This rule has the following (optional) parameters for the ``+R`` option:


*Indirect_Calls*
   Flag all the indirect calls (that is, calls through access-to-subprogram
   values).


.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 7,8

   procedure Calls is
      procedure Unknown with Import;

      type Proc_A is access procedure (X : Integer);
      X : Proc_A := Some_Proc'Access;
   begin
      Unknown;     --  FLAG
      X (1);       --  FLAG (if Indirect_Calls is enabled)


.. _Unchecked_Address_Conversions:

``Unchecked_Address_Conversions``
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: Unchecked_Address_Conversions

Flag instantiations of ``Ada.Unchecked_Conversion`` if the actual for the
formal type Source is the ``System.Address`` type (or a type derived from
it), and the actual for the formal type ``Target`` is an access type
(including types derived from access types). This include cases when the
actual for ``Source`` is a private type and its full declaration is a type
derived from ``System.Address``, and cases when the actual for ``Target`` is
a private type and its full declaration is an access type. The rule is
checked inside expanded generics.

This rule has no parameters.

.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 9

   with Ada.Unchecked_Conversion;
   with System;
   package Foo is
      type My_Address is new System.Address;

      type My_Integer is new Integer;
      type My_Access is access all My_Integer;

      function Address_To_Access is new Ada.Unchecked_Conversion  --  FLAG
        (Source => My_Address,
         Target => My_Access);
   end Foo;


.. _Unchecked_Conversions_As_Actuals:

``Unchecked_Conversions_As_Actuals``
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: Unchecked_Conversions_As_Actuals

Flag call to instantiation of ``Ada.Unchecked_Conversion`` if it is an actual in
procedure or entry call or if it is a default value in a subprogram or
entry parameter specification.

This rule has no parameters.

.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 11, 22

   with Ada.Unchecked_Conversion;
   procedure Bar (I : in out Integer) is
      type T1 is array (1 .. 10) of Integer;
      type T2 is array (1 .. 10) of Integer;

      function UC is new Ada.Unchecked_Conversion (T1, T2);

      Var1 : T1 := (others => 1);
      Var2 : T2 := (others => 2);

      procedure Init (X : out T2; Y : T2 := UC (Var1)) is   --  FLAG
      begin
         X := Y;
      end Init;

      procedure Ident (X : T2; Y : out T2) is
      begin
         Y := X;
      end Ident;

   begin
      Ident (UC (Var1), Var2);                              --  FLAG
   end Bar;


.. _Unconditional_Exits:

``Unconditional_Exits``
^^^^^^^^^^^^^^^^^^^^^^^

.. index:: Unconditional_Exits

Flag unconditional ``exit`` statements.

This rule has no parameters.

.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 8

   procedure Find_A (S : String; Idx : out Natural) is
   begin
      Idx := 0;

      for J in S'Range loop
         if S (J) = 'A' then
            Idx := J;
            exit;             --  FLAG
         end if;
      end loop;
   end Find_A;


.. _Uninitialized_Global_Variables:

``Uninitialized_Global_Variables``
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: Uninitialized_Global_Variables

Flag an object declaration that does not have an explicit initialization if it is
located in a library-level package or generic package or bodies of library-level package
or generic package (including packages and generic packages nested in those).
Do not flag deferred constant declarations.

This rule has no parameters.

.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 2

   package Foo is
      Var1 : Integer;      --  FLAG
      Var2 : Integer := 0;
   end Foo;


.. _Unnamed_Blocks_And_Loops:

``Unnamed_Blocks_And_Loops``
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: Unnamed_Blocks_And_Loops

Flag each unnamed block statement. Flag a unnamed loop statement if this
statement is enclosed by another loop statement or if it encloses another
loop statement.

The rule has no parameters.

.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 5, 10, 14

   procedure Bar (S : in out String) is
      I : Integer := 1;
   begin
      if S'Length > 10 then
         declare                                  --  FLAG
            S1   : String (S'Range);
            Last : Positive := S1'Last;
            Idx  : Positive := 0;
         begin
            for J in S'Range loop                 --  FLAG
               S1 (Last - Idx) := S (J);
               Idx             := Idx + 1;

               for K in S'Range loop              --  FLAG
                  S (K) := Character'Succ (S (K));
               end loop;

            end loop;

            S := S1;
         end;
      end if;
   end Bar;


.. _Use_Array_Slices:

``Use_Array_Slices``
^^^^^^^^^^^^^^^^^^^^

.. index:: Use_Array_Slices

Flag ``FOR`` loops if a loop contains a single assignment statement, and
this statement is an assignment between array components and such a loop can
be replaced by a single assignment statement with array slices or
array objects as the source and the target of the assignment.

This rule has no parameters.

.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 6, 10

      type Table_Array_Type is array (1 .. 10) of Integer;
      Primary_Table   : Table_Array_Type;
      Secondary_Table : Table_Array_Type;

   begin
      for I in Table_Array_Type'Range loop   --  FLAG
         Secondary_Table (I) := Primary_Table (I);
      end loop;

      for I in 2 .. 5 loop                   --  FLAG
         Secondary_Table (I) := Primary_Table (I + 1);
      end loop;


.. _Use_Case_Statements:

``Use_Case_Statements``
^^^^^^^^^^^^^^^^^^^^^^^

.. index:: Use_Case_Statements

Flag an ``IF`` statement if this statement could be replaced by a
``CASE`` statement. An ``IF`` statement is considered as being
replaceable by a ``CASE`` statement if:

*
  it contains at least one ``ELSIF`` alternative;

*
  all the conditions are infix calls to some predefined relation operator,
  for all of them one operand is the reference to the same variable of some
  discrete type;

*
  for calls to relation operator another operand is some static expression;

This rule has no parameters.

.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 1

   if I = 1 then      --  FLAG
      I := I + 1;
   elsif I > 2 then
      I := I + 2;
   else
      I := 0;
   end if;

.. _USE_Clauses:

``USE_Clauses``
^^^^^^^^^^^^^^^

.. index:: USE_Clauses

Flag names mentioned in use clauses. Use type clauses and names mentioned
in them are not flagged.

This rule has the following optional parameter for the ``+R`` option:

*Exempt_Operator_Packages*

   Do not flag a package name in a package use clause if it refers to a package
   that only declares operators in its visible part.

.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 10, 11

   package Pack is
      I : Integer;
   end Pack;

   package Operator_Pack is
      function "+" (L, R : Character) return Character;
   end Operator_Pack;

   with Pack, Operator_Pack;
   use Pack;                   --  FLAG
   use Operator_Pack;          --  FLAG only if Exempt_Operator_Packages is not set


.. _Use_For_Loops:

``Use_For_Loops``
^^^^^^^^^^^^^^^^^

.. index:: Use_For_Loops

Flag ``WHILE`` loops which could be replaced by a ``FOR`` loop. The rule detects
the following code patterns:

.. code-block:: ada

      ...
      Id : Some_Integer_Type ...;
      ... -- no write reference to Id
   begin
      ...
      while Id <relation_operator> Limit loop
         ...  -- no write reference to Id
         Id := Id <increment_operator> 1;
      end loop;
      ...  -- no reference to Id
   end;

where relation operator in the loop condition should be some predefined
relation operator, and increment_operator should be a predefined "+" or
"-" operator.

Note, that the rule only informs about a possibility to replace a
``WHILE`` loop by a ``FOR``, but does not guarantee that this is
really possible, additional human analysis is required for all the
loops marked by the rule.

This rule has no parameters.

.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 3

      Idx : Integer := 1;
   begin
      while Idx <= 10 loop    --  FLAG
         Idx := Idx + 1;
      end loop;
   end;



.. _USE_PACKAGE_Clauses:

``USE_PACKAGE_Clauses``
^^^^^^^^^^^^^^^^^^^^^^^

.. index:: USE_PACKAGE_Clauses

Flag all ``use`` clauses for packages; ``use type`` clauses are
not flagged.

This rule has no parameters.

.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 2

   with Ada.Text_IO;
   use Ada.Text_IO;                               --  FLAG
   procedure Bar (S : in out String) is



.. _Variable_Scoping:

``Variable_Scoping``
^^^^^^^^^^^^^^^^^^^^

.. index:: Variable_Scoping

Flag local object declarations that can be moved into declare blocks
nested into the declaration scope. A declaration is considered as movable
into a nested scope if:

*
  The declaration does not contain an initialization expression;

*
  The declared object is used only in a nested block statement,
  and this block statement has a declare part;

*
  the block statement is not enclosed into a loop statement.

This rule has no parameters.

.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 2

   procedure Scope is
      X : Integer;  --  FLAG
   begin
      declare
         Y : Integer := 42;
      begin
         X := Y;
      end;
   end;

.. _Warnings:

``Warnings``
^^^^^^^^^^^^

.. index:: Warnings

Flags construct that would result in issuing a GNAT warning if an argument
source would be compiled with warning options corresponding to the rule
parameter(s) specified. For GNAT warnings and corresponding warning control
options see the `Warning Message Control <https://docs.adacore.com/gnat_ugn-docs/html/gnat_ugn/gnat_ugn/building_executable_programs_with_gnat.html#warning-message-control>`_ section of the GNAT User's Guide.

``gnatcheck`` does not check itself if this or that construct would result
in issuing a warning, instead it compiles the project sources with the
needed warning control compilation options combined with the ``-gnatc``
switch, analyses the warnings generated by GNAT and adds the relevant
information the ``gnatcheck`` messages.

The rule should have a parameter, the format of the parameter should
be a valid ``static_string_expression`` listing GNAT warnings switches
(the letter following ``-gnatw`` in the `Warning Message Control` section
mentioned above).

Note that the ``s`` parameter corresponding to the GNAT ``-gnatws`` option
disables all the specific warnings, but not does not suppress the warning mode,
and the ``e`` parameter corresponding to ``-gnatwe`` is ignored.

Note also that some GNAT warnings are only emitted when generating code,
these warnings will not be generated by this rule. In other words, this
rule will only generate warnings that are enabled when using ``-gnatc``.

If your code contains pragmas ``Warnings`` with parameter ``Off``, this may
result in false negatives for this rule, because the corresponding warnings
generated during compilation will be suppressed. The workaround is to
use for ``gnatcheck`` call a configuration file that
contains ``pragma Ignore_Pragma (Warnings);``.

This rule allows parametric rule exemptions, the parameters
that are allowed in the definition of exemption sections are the
same as the parameters of the rule itself.

.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 1,4

   with Ada.Text_IO;                        --  FLAG (+RWarnings:u)
   procedure Proc (I : in out Integer) is
   begin
      pragma Unrecognized;                  --  FLAG (+RWarnings:g)

      I := I + 1;
   end Proc;

.. _Readability:

Readability
-----------

.. index:: Readability-related rules

The rules described in this subsection may be used to enforce feature usages
that contribute towards readability.

.. _End_Of_Line_Comments:

``End_Of_Line_Comments``
^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: End_Of_Line_Comments

Flags comments that are located in the source lines that
contains Ada code.

This rule has no parameters.

.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 3,4

   package A is
      -- NO FLAG
      I : Integer;  -- FLAG
   end A;  --  FLAG


.. _Headers:

``Headers``
^^^^^^^^^^^

.. index:: Headers

Check that the source text of a compilation unit starts from
the text fragment specified as a rule parameter.

This rule has the following (mandatory) parameters for the ``+R`` option:

*header*
  The name of a header file.

A header file is a plain text file. The rule checks that
the beginning of the compilation unit source text is literally
the same as the content of the header file. Blank lines and trailing
spaces are not ignored and are taken into account, casing is important.
The format of the line breaks (DOS or UNIX) is not important.

.. _Identifier_Casing:

``Identifier_Casing``
^^^^^^^^^^^^^^^^^^^^^

.. index:: Identifier_Casing

Flag each defining identifier that does not have a casing corresponding to the
kind of entity being declared. All defining names are checked. For the
defining names from the following kinds of declarations a special casing scheme
can be defined:

*
  type and subtype declarations;

*
  enumeration literal specifications (not including character literals)
  and function renaming declarations if the renaming entity is an
  enumeration literal;

*
  constant and number declarations (including object renaming
  declarations if the renamed object is a constant);

*
  exception declarations and exception renaming declarations.

The rule may have the following parameters for ``+R``:



*
  Type=\ *casing_scheme*

  Specifies casing for names from type and subtype declarations.


*
  Enum=\ *casing_scheme*

  Specifies the casing of defining enumeration literals and for the
  defining names in a function renaming declarations if the renamed
  entity is an enumeration literal.


*
  Constant=\ *casing_scheme*

  Specifies the casing for defining names from constants and named number
  declarations, including the object renaming declaration if the
  renamed object is a constant


*
  Exception=\ *casing_scheme*

  Specifies the casing for names from exception declarations and exception
  renaming declarations.


*
  Others=\ *casing_scheme*

  Specifies the casing for all defining names for which no special casing
  scheme is specified. If this parameter is not set, the casing for the
  entities that do not correspond to the specified parameters is not checked.


*
  Exclude=\ *dictionary_file*

  Specifies casing exceptions.

Where:


::

     casing_scheme ::= upper|lower|mixed


*upper* means that the defining identifier should be upper-case.
*lower* means that the defining identifier should be lower-case
*mixed* means that the first defining identifier letter and the first
letter after each underscore should be upper-case, and all the other
letters should be lower-case

If a defining identifier is from a declaration for which a specific casing
scheme can be set, but the corresponding parameter is not specified for the
rule, then the casing scheme defined by ``Others`` parameter is used to
check this identifier. If ``Others`` parameter also is not set, the
identifier is not checked.

*dictionary_file* is the name of the text file that contains casing
exceptions. The way how this rule is using the casing exception dictionary
file is consistent with using the casing exception dictionary in the
GNAT pretty-printer *gnatpp*, see
GNAT User's Guide.

There are two kinds of exceptions:



*identifier*
  If a dictionary file contains an identifier, then each occurrence of that
  (defining) identifier in the checked source should use the casing specified
  included in *dictionary_file*


*wildcard*
  A wildcard has the following syntax


::

      wildcard ::= *simple_identifier* |
                         *simple_identifier |
                         simple_identifier*
      simple_identifier ::= letter{letter_or_digit}


``simple_identifier`` specifies the casing of subwords (the term 'subword'
is used below to denote the part of a name which is delimited by '_' or by
the beginning or end of the word and which does not contain any '_' inside).
A wildcard of the form ``simple_identifier*`` defines the casing of the
first subword of a defining name to check, the wildcard of the form
``*simple_identifier`` specifies the casing of the last subword, and
the wildcard of the form ``*simple_identifier*`` specifies the casing of
any subword.

If for a defining identifier some of its subwords can be mapped onto
wildcards, but some other cannot, the casing of the identifier subwords
that are not mapped onto wildcards from casing exception dictionary
is checked against the casing scheme defined for the corresponding
entity.

If some identifier is included in the exception dictionary both as a whole
identifier and can be mapped onto some wildcard from the
dictionary, then it is the identifier and not the wildcard that is used to check
the identifier casing.

If more than one dictionary file is specified, or a dictionary file contains
more than one exception variant for the same identifier, the new casing
exception overrides the previous one.

Casing check against dictionary file(s) has a higher priority than checks
against the casing scheme specified for a given entity/declaration kind.

``+R`` option should contain at least one parameter.

There is no parameter for ``-R`` option, it just turns the rule off.

The rule allows parametric exemption, the parameters that are allowed in
the definition of exemption sections are:



*Type*
  Exempts check for type and subtype name casing


*Enum*
  Exempts check for enumeration literal name casing


*Constant*
  Exempts check for constant name casing


*Exception*
  Exempts check for exception name casing


*Others*
  Exempts check for defining names for which no special casing scheme is specified.


*Exclude*
  Exempts check for defining names for which casing schemes are specified in exception
  dictionaries

.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 4, 7

   --  if the rule is activated as '+RIdentifier_Casing:Type=upper,others=mixed'
   package Foo is
      type ENUM_1 is (A1, B1, C1);
      type Enum_2 is (A2, B2, C2);      --  FLAG

      Var1 : Enum_1 := A1;
      VAR2 : ENUM_2 := A2;              --  FLAG
   end Foo;


.. _Identifier_Prefixes:

``Identifier_Prefixes``
^^^^^^^^^^^^^^^^^^^^^^^

.. index:: Identifier_Prefixes

Flag each defining identifier that does not have a prefix corresponding
to the kind of declaration it is defined by. The defining names in the
following kinds of declarations are checked:

*
  type and subtype declarations (task, protected and access types are treated
  separately);

*
  enumeration literal specifications (not including character literals)
  and function renaming declarations if the renaming entity is an
  enumeration literal;

*
  exception declarations and exception renaming declarations;

*
  constant and number declarations (including object renaming
  declarations if the renamed object is a constant).

Defining names declared by single task declarations or single protected
declarations are not checked by this rule.

The defining name from the full type declaration corresponding to a
private type declaration or a private extension declaration is never
flagged. A defining name from an incomplete type declaration is never
flagged.

The defining name from a subprogram renaming-as-body declaration is
never flagged.

For a deferred constant, the defining name in the corresponding full
constant declaration is never flagged.

The defining name from a body that is a completion of a program unit
declaration or a proper body of a subunit is never flagged.

The defining name from a body stub that is a completion of a program
unit declaration is never flagged.

Note that the rule checks only defining names. Usage name occurrence are
not checked and are never flagged.

The rule may have the following parameters:

*
  For the ``+R`` option:


*
  Type=\ *string*

    Specifies the prefix for a type or subtype name.


*
  Concurrent=\ *string*

    Specifies the prefix for a task and protected type/subtype name. If this
    parameter is set, it overrides for task and protected types the prefix set by
    the Type parameter.


*
  Access=\ *string*

    Specifies the prefix for an access type/subtype name. If this parameter is
    set, it overrides for access types the prefix set by the ``Type``
    parameter.


*
  Class_Access=\ *string*

    Specifies the prefix for the name of an access type/subtype that points to some
    class-wide type. If this parameter is set, it overrides for such access types
    and subtypes the prefix set by the ``Type`` or ``Access`` parameter.


*
  Subprogram_Access=\ *string*

    Specifies the prefix for the name of an access type/subtype that points to a
    subprogram. If this parameter is set, it overrides for such access
    types/subtypes the prefix set by the ``Type`` or ``Access`` parameter.


*
  Derived=\ *string1:string2*

    Specifies the prefix for a type that is directly derived from a given type or
    from a subtype thereof. *string1* should be a full expanded Ada name of the
    ancestor type (starting from the full expanded compilation unit
    name), *string2* defines the prefix to check. If this
    parameter is set, it overrides for types that are directly derived from the
    given type the prefix set by the ``Type`` parameter.


*
  Constant=\ *string*

    Specifies the prefix for defining names from constants and named number
    declarations, including the object renaming declaration if the
    renamed object is a constant


*
  Enum=\ *string*

    Specifies the prefix for defining enumeration literals and for the
    defining names in a function renaming declarations if the renamed
    entity is an enumeration literal.


*
  Exception=\ *string*

    Specifies the prefix for defining names from exception declarations
    and exception renaming declarations.


*Exclusive*
    Check that only those kinds of names for which specific prefix is defined have
    that prefix (e.g., only type/subtype names have prefix *T_*, but
    not variable or package names), and flag all defining names that have any
    of the specified prefixes but do not belong to the kind of entities this
    prefix is defined for. By default the exclusive check mode is ON.

  For the ``-R`` option:


*All_Prefixes*
    Removes all the prefixes specified for the identifier prefix
    checks, whether by default or as specified by other rule
    parameters and disables the rule.


*Type*
    Removes the prefix specified for type/subtype names. This does not remove
    prefixes specified for specific type kinds and does not disable checks for
    these specific kinds.


*Concurrent*
    Removes the prefix specified for task and protected types.


*Access*
    Removes the prefix specified for access types. This does not remove prefixes
    specified for specific access types (access to subprograms and class-wide
    access)


*Class_Access*
    Removes the prefix specified for access types pointing to class-wide types.


*Subprogram_Access*
    Removes the prefix specified for access types pointing to subprograms.


*Derived*
    Removes prefixes specified for derived types that are directly derived from
    specific types.


*Constant*
    Removes the prefix specified for constant and number names and turns off the
    check for these names.


*Exception*
    Removes the prefix specified for exception names and turns off the
    check for exception names.


*Enum*
    Removes the prefix specified for enumeration literal names and
    turns off the check for them.


*Exclusive*
    Turns of the check that only names of specific kinds of entities have prefixes
    specified for these kinds.

If more than one parameter is used, parameters must be separated by
commas.

If more than one option is specified for the gnatcheck invocation, a new
option overrides the previous one(s).

The ``+RIdentifier_Prefixes`` option (with no parameter) enables checks
for all the name prefixes specified by previous options used for this
rule. If no prefix is specified, the rule is not enabled.

The ``-RIdentifier_Prefixes`` option (with no parameter) disables all the
checks but keeps all the prefixes specified by previous options used for
this rule.

There is no default prefix setting for this rule. All checks for
name prefixes are case-sensitive

If any error is detected in a rule parameter, that parameter is ignored.
In such a case the options that are set for the rule are not specified.

The rule allows parametric exemption, the parameters that are allowed in
the definition of exemption sections are:



*Type*
  Exempts check for type and subtype name prefixes


*Concurrent*
  Exempts check for task and protected type/subtype name prefixes


*Access*
  Exempts check for access type/subtype name prefixes


*Class_Access*
  Exempts check for names of access types/subtypes that point to
  some class-wide types


*Subprogram_Access*
  Exempts check for names of access types/subtypes that point to
  subprograms


*Derived*
  Exempts check for derived type name prefixes


*Constant*
  Exempts check for constant and number name prefixes


*Exception*
  Exempts check for exception name prefixes


*Enum*
  Exempts check for enumeration literal name prefixes


*Exclusive*
  Exempts check that only names of specific kinds of entities have prefixes
  specified for these kinds

.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 4, 7, 10

   --  if the rule is activated as '+RIdentifier_Prefixes:Type=Type_,Constant=Const_,ExceptioN=X_'
   package Foo is
      type Type_Enum_1 is (A1, B1, C1);
      type Enum_2      is (A2, B2, C2);         --  FLAG

      Const_C1 : constant Type_Enum_1 := A1;
      Const2   : constant Enum_2      := A2;    --  FLAG

      X_Exc_1 : exception;
      Exc_2   : exception;                      --  FLAG
   end Foo;


.. _Identifier_Suffixes:

``Identifier_Suffixes``
^^^^^^^^^^^^^^^^^^^^^^^

.. index:: Identifier_Suffixes
.. index:: Misnamed_Identifiers

Because of upward compatibility reasons this rule has a synonym
``Misnamed_Identifiers``.

Flag the declaration of each identifier that does not have a suffix
corresponding to the kind of entity being declared.
The following declarations are checked:

*
  type declarations

*
  subtype declarations

*
  object declarations (variable and constant declarations, but not number,
  declarations, record component declarations, parameter specifications,
  extended return object declarations, formal object declarations)

*
  package renaming declarations (but not generic package renaming
  declarations)

The default checks (enforced by the *Default* rule parameter) are:

*
  type-defining names end with ``_T``, unless the type is an access type,
  in which case the suffix must be ``_A``
*
  constant names end with ``_C``
*
  names defining package renamings end with ``_R``
*
  the check for access type objects is not enabled

Defining identifiers from incomplete type declarations are never flagged.

For a private type declaration (including private extensions), the defining
identifier from the private type declaration is checked against the type
suffix (even if the corresponding full declaration is an access type
declaration), and the defining identifier from the corresponding full type
declaration is not checked.

For a deferred constant, the defining name in the corresponding full constant
declaration is not checked.

Defining names of formal types are not checked.

Check for the suffix of access type data objects is applied to the
following kinds of declarations:

*
  variable and constant declaration

*
  record component declaration

*
  return object declaration

*
  parameter specification

*
  extended return object declaration

*
  formal object declaration

If both checks for constant suffixes and for access object suffixes are
enabled, and if different suffixes are defined for them, then for constants
of access type the check for access object suffixes is applied.

The rule may have the following parameters:

*
  For the ``+R`` option (unless the parameter
  is ``Default``, then only the explicitly specified
  suffix is checked, and no defaults are used):


*Default*
    Sets the default listed above for all the names to be checked.


*
  Type_Suffix=\ *string*

    Specifies the suffix for a type name.


*
  Access_Suffix=\ *string*

    Specifies the suffix for an access type name. If
    this parameter is set, it overrides for access
    types the suffix set by the ``Type_Suffix`` parameter.
    For access types, *string* may have the following format:
    *suffix1(suffix2)*. That means that an access type name
    should have the *suffix1* suffix except for the case when
    the designated type is also an access type, in this case the
    type name should have the *suffix1 & suffix2* suffix.


*
  Class_Access_Suffix=\ *string*

    Specifies the suffix for the name of an access type that points to some
    class-wide type.
    If this parameter is set, it overrides for such access
    types the suffix set by the ``Type_Suffix`` or ``Access_Suffix``
    parameter.


*
  Class_Subtype_Suffix=\ *string*

    Specifies the suffix for the name of a subtype that denotes a class-wide type.


*
  Constant_Suffix=\ *string*

    Specifies the suffix for a constant name.


*
  Renaming_Suffix=\ *string*

    Specifies the suffix for a package renaming name.

*
  Access_Obj_Suffix=\ *string*

    Specifies the suffix for objects that have an access type
    (including types derived from access types).

*
  Interrupt_Suffix=\ *string*

    Specifies the suffix for protected subprograms used as
    interrupt handlers.


*
  For the ``-R`` option:


*All_Suffixes*
    Remove all the suffixes specified for the
    identifier suffix checks, whether by default or
    as specified by other rule parameters. All the
    checks for this rule are disabled as a result.


*Type_Suffix*
    Removes the suffix specified for types. This
    disables checks for types but does not disable
    any other checks for this rule (including the
    check for access type names if ``Access_Suffix`` is
    set).


*Access_Suffix*
    Removes the suffix specified for access types.
    This disables checks for access type names but
    does not disable any other checks for this rule.
    If ``Type_Suffix`` is set, access type names are
    checked as ordinary type names.


*Class_Access_Suffix*
    Removes the suffix specified for access types pointing to class-wide
    type. This disables specific checks for names of access types pointing to
    class-wide types but does not disable any other checks for this rule.
    If ``Type_Suffix`` is set, access type names are
    checked as ordinary type names. If ``Access_Suffix`` is set, these
    access types are checked as any other access type name.


*Class_Subtype_Suffix*
    Removes the suffix specified for subtype names.
    This disables checks for subtype names but
    does not disable any other checks for this rule.


*Constant_Suffix*
    Removes the suffix specified for constants. This
    disables checks for constant names but does not
    disable any other checks for this rule.


*Renaming_Suffix*
    Removes the suffix specified for package
    renamings. This disables checks for package
    renamings but does not disable any other checks
    for this rule.

*Access_Obj_Suffix*
    Removes the suffix specified for objects of access types,
    this disables checks for such objects. It does not disable
    any other checks for this rule

*Interrupt_Suffix*
    Removes the suffix specified for protected subprograms used
    as interrupt handlers. It does not disable any other checks
    for this rule.

If more than one parameter is used, parameters must be separated by commas.

If more than one  option is specified for the ``gnatcheck`` invocation,
a new option overrides the previous one(s).

The ``+RIdentifier_Suffixes`` option (with no parameter) enables
checks for all the
name suffixes specified by previous options used for this rule.

The ``-RIdentifier_Suffixes`` option (with no parameter) disables
all the checks but keeps
all the suffixes specified by previous options used for this rule.

The *string* value must be a valid suffix for an Ada identifier (after
trimming all the leading and trailing space characters, if any).
Parameters are not case sensitive, except the *string* part.

If any error is detected in a rule parameter, the parameter is ignored.
In such a case the options that are set for the rule are not
specified.

The rule allows parametric exemption, the parameters that are allowed in
the definition of exemption sections are:



*Type*
  Exempts check for type name suffixes


*Access*
  Exempts check for access type name suffixes


*Access_Obj*
  Exempts check for access object name suffixes


*Class_Access*
  Exempts check for names of access types that point to
  some class-wide types


*Class_Subtype*
  Exempts check for names of subtypes that denote class-wide types


*Constant*
  Exempts check for constant name suffixes


*Renaming*
  Exempts check for package renaming name suffixes

.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 3, 6, 9

   --  if the rule is activated as '+RIdentifier_Suffixes:Access_Suffix=_PTR,Type_Suffix=_T,Constant_Suffix=_C'
   package Foo is
      type Int   is range 0 .. 100;      --  FLAG
      type Int_T is range 0 .. 100;

      type Int_A   is access Int;        --  FLAG
      type Int_PTR is access Int;

      Const   : constant Int := 1;       --  FLAG
      Const_C : constant Int := 1;

   end Foo;


.. _Max_Identifier_Length:

``Max_Identifier_Length``
^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: Max_Identifier_Length

Flag any defining identifier that has length longer than specified by
the rule parameter. Defining identifiers of enumeration literals are not
flagged. The rule has a mandatory parameter for +R option:

*N*
   The maximal allowed identifier length specification.

.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 2

   type My_Type is range -100 .. 100;
   My_Variable_With_A_Long_Name : My_Type;  -- FLAG (if rule parameter is 27 or smaller)


.. _Min_Identifier_Length:

``Min_Identifier_Length``
^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: Min_Identifier_Length

Flag any defining identifier that has length shorter than specified by
the rule parameter. Defining identifiers of objects and components of
numeric types are not flagged.

The rule has a mandatory parameter for +R option:

*N*
   The minimal allowed identifier length specification.

.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 2

   I : Integer;              --  NO FLAG
   J : String (1 .. 10);     --  FLAG


.. _Misnamed_Controlling_Parameters:

``Misnamed_Controlling_Parameters``
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: Misnamed_Controlling_Parameters

Flag a declaration of a dispatching operation, if the first parameter is
not a controlling one and its name is not ``This`` (the check for
parameter name is not case-sensitive). Declarations of dispatching functions
with a controlling result and no controlling parameter are never flagged.

A subprogram body declaration, subprogram renaming declaration, or subprogram
body stub is flagged only if it is not a completion of a prior subprogram
declaration.

This rule has no parameters.

.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 5, 6

   package Foo is
      type T is tagged private;

      procedure P1 (This : in out T);
      procedure P2 (That : in out T);              --  FLAG
      procedure P1 (I : Integer; This : in out T); --  FLAG


.. _Name_Clashes:

``Name_Clashes``
^^^^^^^^^^^^^^^^

.. index:: Name_Clashes

Check that certain names are not used as defining identifiers. The names that
should not be used as identifiers must be listed in a dictionary file that is
a rule parameter. A defining identifier is flagged if it is included in a
dictionary file specified as a rule parameter, the check is not case-sensitive.
Only the whole identifiers are checked, not substrings thereof.
More than one dictionary file can be specified as the rule parameter, in this
case the rule checks defining identifiers against the union of all the
identifiers from all the dictionary files provided as the rule parameters.

This rule has the following (mandatory) parameters for the ``+R`` option:

*dictionary_file*
  The name of a dictionary file.

A dictionary file is a plain text file. The maximum line length for this file
is 1024 characters.  If the line is longer than this limit, extra characters
are ignored.

If the name of the dictionary file does not contain any path information and
the rule option is specifies in a rule file, first the tool tries to locate
the dictionary file in the same directory where the rule file is located, and
if the attempt fails - in the current directory.

Each line can be either an empty line, a comment line, or a line containing
a list of identifiers separated by space or HT characters.
A comment is an Ada-style comment (from ``--`` to end-of-line).
Identifiers must follow the Ada syntax for identifiers.
A line containing one or more identifiers may end with a comment.

.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 2, 3

   --  If the dictionary file contains names 'One' and 'Two":
   One          : constant Integer := 1;     --  FLAG
   Two          : constant Float   := 2.0;   --  FLAG
   Constant_One : constant Float   := 1.0;


.. _Numeric_Format:

``Numeric_Format``
^^^^^^^^^^^^^^^^^^

.. index:: Numeric_Format

Flag each numeric literal which does not satisfy at least one of the
following requirements:

*
  the literal is given in the conventional decimal notation given,
  or, if its base is specified explicitly, this base should be
  2, 8, 10 or 16 only;

*
  if the literal base is 8 or 10, an underscore should separate groups
  of 3 digits starting from the right end of the literal;

*
  if the literal base is 2 or 16, an underscore should separate groups
  of 4 digits starting from the right end of the literal;

*
  all letters (exponent symbol and digits above 9) should be in upper case.

This rule has no parameters.

.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 4, 5

   D : constant := 16#12AB_C000#;          --  NO FLAG
   E : constant := 3.5E3;                  --  NO FLAG

   F : constant := 1000000;                --  FLAG
   G : constant := 2#0001000110101011#;    --  FLAG


.. _Object_Declarations_Out_Of_Order:

``Object_Declarations_Out_Of_Order``
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: Object_Declarations_Out_Of_Order

Flag any object declaration that is located in a library unit body if
this is preceding by a declaration of a program unit spec, stub or body.

This rule has no parameters.

.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 4

   procedure Proc is
      procedure Proc1 is separate;

      I : Integer;    -- FLAG


.. _One_Construct_Per_Line:

``One_Construct_Per_Line``
^^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: One_Construct_Per_Line

Flag any statement, declaration or representation clause if the code
line where this construct starts contains some other Ada code symbols
preceding or following this construct. The following constructs are not
flagged:

*
  enumeration literal specification;

*
  parameter specifications;

*
  discriminant specifications;

*
  mod clauses;

*
  loop parameter specification;

*
  entry index specification;

*
  choice parameter specification;

In case if we have two or more declarations/statements/clauses on a
line and if there is no Ada code preceding the first construct, the
first construct is flagged

This rule has no parameters.

.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 5

   procedure Swap (I, J : in out Integer) is
      Tmp : Integer;
   begin
      Tmp := I;
      I := J; J := Tmp;      --  FLAG
   end Swap;

.. _Style_Checks:

``Style_Checks``
^^^^^^^^^^^^^^^^

.. index:: Style_Checks

Flags violations of the source code presentation and formatting rules
specified in the `Style Checking <https://docs.adacore.com/gnat_ugn-docs/html/gnat_ugn/gnat_ugn/building_executable_programs_with_gnat.html#style-checking>`_
section of the ``GNAT User's Guide`` according to the rule parameter(s)
specified.

``gnatcheck`` does not check GNAT style rules itself, instead it compiles
an argument source with the needed style check compilation options,
analyses the style messages generated by the GNAT compiler and
includes the information about style violations detected into
the ``gnatcheck`` messages.

This rule takes a parameter in one of the following forms:

* *All_Checks*, which enables the standard style checks corresponding
  to the ``-gnatyy`` GNAT style check option,

* A string with the same
  structure and semantics as the ``string_LITERAL`` parameter of the
  GNAT pragma ``Style_Checks``
  (see ``Pragma Style_Checks`` in the GNAT Reference Manual).

For instance, the ``+RStyle_Checks:O`` rule option activates
the compiler style check that corresponds to ``-gnatyO`` style check option.

.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 3

   package Pack is
      I : Integer;
   end;   -- FLAG (for +RStyle_Checks:e)

.. _Uncommented_BEGIN:

``Uncommented_BEGIN``
^^^^^^^^^^^^^^^^^^^^^

.. index:: Uncommented_BEGIN

Flags ``BEGIN`` keywords in program unit bodies if the body contains
both declarations and a statement part and if there is no trailing
comment just after the keyword (on the same line) with the unit
name as the only content of the comment, the casing of the unit
name in the comment should be the same as the casing of the defining
unit name in the unit body declaration.

.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 3

   procedure Proc (I : out Integer) is
      B : Boolean;
   begin
      I := Var;
   end Proc;


.. _Uncommented_BEGIN_In_Package_Bodies:

``Uncommented_BEGIN_In_Package_Bodies``
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: Uncommented_BEGIN_In_Package_Bodies

Flags ``BEGIN`` keywords in package bodies if the body contains
both declarations and a statement part and if there is no trailing
comment just after the keyword (on the same line) with the package
name as the only content of the comment, the casing of the package
name in the comment should be the same as the casing of the defining
unit name in the package body.

.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 15

   package body Foo is
      procedure Proc (I : out Integer) is
      begin
         I := Var;
      end Proc;

      package body Inner is
         procedure Inner_Proc (I : out Integer) is
         begin
            I := Inner_Var;
         end  ;
      begin  -- Inner
         Inner_Var := 1;
      end Inner;
   begin                 --  FLAG
      Var := Inner.Inner_Var + 1;
   end Foo;


Feature Usage Rules
===================

.. index:: Feature usage related rules

The rules in this section can be used to enforce specific
usage patterns for a variety of language features.

.. _Abort_Statements:

``Abort_Statements``
--------------------

.. index:: Abort_Statements

Flag abort statements.

This rule has no parameters.

.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 2

   if Flag then
      abort T;    --  FLAG
   end if;


.. _Abstract_Type_Declarations:

``Abstract_Type_Declarations``
------------------------------

.. index:: Abstract_Type_Declarations

Flag all declarations of abstract types, including generic formal types.
For an abstract private type, the full type declarations is flagged
only if it is itself declared as abstract. Interface types are not
flagged.

This rule has no parameters.

.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 2, 5

   package Foo is
      type Figure is abstract tagged private;              --  FLAG
      procedure Move (X : in out Figure) is abstract;
   private
      type Figure is abstract tagged null record;          --  FLAG
   end Foo;

.. _Anonymous_Access:

``Anonymous_Access``
--------------------

.. index:: Anonymous_Access

Flag object declarations, formal object declarations and component declarations with
anonymous access type definitions. Discriminant specifications and parameter
specifications are not flagged.

This rule has no parameters.

.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 10, 13

   procedure Anon (X : access Some_Type) is   --  NO FLAG
      type Square
        (Location : access Coordinate)        --  NO FLAG
      is record
         null;
      end record;

      type Cell is record
         Some_Data : Integer;
         Next      : access Cell;             --  FLAG
      end record;

      Link : access Cell;                     --  FLAG

.. _Anonymous_Subtypes:

``Anonymous_Subtypes``
----------------------

.. index:: Anonymous_Subtypes

Flag all uses of anonymous subtypes except for the following:

*
  when the subtype indication depends on a discriminant, this includes the
  cases of a record component definitions when a component depends on a
  discriminant, and using the discriminant of the derived type to
  constraint the parent type;

*
  when a self-referenced data structure is defined, and a discriminant
  is constrained by the reference to the current instance of a type;

A use of an anonymous subtype is
any instance of a subtype indication with a constraint, other than one
that occurs immediately within a subtype declaration. Any use of a range
other than as a constraint used immediately within a subtype declaration
is considered as an anonymous subtype.

The rule does not flag ranges in the component clauses from a record
representation clause, because the language rules do not allow to use
subtype names there.

An effect of this rule is that ``for`` loops such as the following are
flagged (since ``1..N`` is formally a 'range')

.. code-block:: ada
   :emphasize-lines: 1

   for I in 1 .. N loop   --  FLAG
      ...
   end loop;

Declaring an explicit subtype solves the problem:

.. code-block:: ada
   :emphasize-lines: 3

   subtype S is Integer range 1..N;
   ...
   for I in S loop        --  NO FLAG
      ...
   end loop;

This rule has no parameters.

.. _Blocks:

``Blocks``
----------

.. index:: Blocks

Flag each block statement.

This rule has no parameters.

.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 2

   if I /= J then
      declare             --  FLAG
         Tmp : Integer;
      begin
         TMP := I;
         I   := J;
         J   := Tmp;
      end;
   end if;


.. _Complex_Inlined_Subprograms:

``Complex_Inlined_Subprograms``
-------------------------------

.. index:: Complex_Inlined_Subprograms

Flag a subprogram (or generic subprogram, or instantiation of a subprogram) if
pragma Inline is applied to it and at least one of the following
conditions is met:

*
  it contains at least one complex declaration such as a subprogram body,
  package, task, protected declaration, or a generic instantiation
  (except instantiation of ``Ada.Unchecked_Conversion``);

*
  it contains at least one complex statement such as a loop, a case
  or an if statement;

*
  the number of statements exceeds
  a value specified by the *N* rule parameter;

Subprogram renamings are also considered.

This rule has the following (mandatory) parameter for the ``+R`` option:



*N*
  Positive integer specifying the maximum allowed total number of statements
  in the subprogram body.

.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 3

   procedure Swap (I, J : in out Integer) with Inline => True;

   procedure Swap (I, J : in out Integer) is   --  FLAG
   begin

      if I /= J then
         declare
            Tmp : Integer;
         begin
            TMP := I;
            I   := J;
            J   := Tmp;
         end;
      end if;

   end Swap;

.. _Conditional_Expressions:

``Conditional_Expressions``
---------------------------

.. index:: Conditional_Expressions

Flag use of conditional expression.

This rule has the following (optional) parameters for the ``+R`` option:



*Except_Assertions*
  Do not flag a conditional expression if it is a subcomponent
  of the following constructs:



*argument of the following pragmas*


*Language-defined*

*
  ``Assert``


*GNAT-specific*

*
  ``Assert_And_Cut``

*
  ``Assume``

*
  ``Contract_Cases``

*
  ``Debug``

*
  ``Invariant``

*
  ``Loop_Invariant``

*
  ``Loop_Variant``

*
  ``Postcondition``

*
  ``Precondition``

*
  ``Predicate``

*
  ``Refined_Post``



*definition of the following aspects*


*Language-defined*

*
  ``Static_Predicate``

*
  ``Dynamic_Predicate``

*
  ``Pre``

*
  ``Pre'Class``

*
  ``Post``

*
  ``Post'Class``

*
  ``Type_Invariant``

*
  ``Type_Invariant'Class``


*GNAT-specific*

*
  ``Contract_Cases``

*
  ``Invariant``

*
  ``Invariant'Class``

*
  ``Predicate``

*
  ``Refined_Post``

.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 1

   Var1 : Integer := (if I > J then 1 else 0);  --  FLAG
   Var2 : Integer := I + J;


.. _Controlled_Type_Declarations:

``Controlled_Type_Declarations``
--------------------------------

.. index:: Controlled_Type_Declarations

Flag all declarations of controlled types. A declaration of a private type
is flagged if its full declaration declares a controlled type. A declaration
of a derived type is flagged if its ancestor type is controlled. Subtype
declarations are not checked. A declaration of a type that itself is not a
descendant of a type declared in ``Ada.Finalization`` but has a controlled
component is not checked.

This rule has no parameters.

.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 3

   with Ada.Finalization;
   package Foo is
      type Resource is new Ada.Finalization.Controlled with private;  --  FLAG


.. _Declarations_In_Blocks:

``Declarations_In_Blocks``
--------------------------

.. index:: Declarations_In_Blocks

Flag all block statements containing local declarations. A ``declare``
block with an empty *declarative_part* or with a *declarative part*
containing only pragmas and/or ``use`` clauses is not flagged.

This rule has no parameters.

.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 2

   if I /= J then
      declare                       --  FLAG
         Tmp : Integer;
      begin
         TMP := I;
         I   := J;
         J   := Tmp;
      end;
   end if;

.. _Deeply_Nested_Inlining:

``Deeply_Nested_Inlining``
--------------------------

.. index:: Deeply_Nested_Inlining

Flag a subprogram (or generic subprogram) if pragma Inline has been applied
to it, and it calls another subprogram to which pragma Inline applies,
resulting in potential nested inlining, with a nesting depth exceeding the
value specified by the *N* rule parameter.

This rule requires the global analysis of all the compilation units that
are ``gnatcheck`` arguments; such analysis may affect the tool's
performance. If gnatcheck generates warnings saying that "*body is not
analyzed for ...*", this means that such an analysis is incomplete, this
may result in rule false negatives.

This rule has the following (mandatory) parameter for the ``+R`` option:



*N*
  Positive integer specifying the maximum level of nested calls to
  subprograms to which pragma Inline has been applied.

.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 1

   procedure P1 (I : in out integer) with Inline => True;   --  FLAG
   procedure P2 (I : in out integer) with Inline => True;
   procedure P3 (I : in out integer) with Inline => True;
   procedure P4 (I : in out integer) with Inline => True;

   procedure P1 (I : in out integer) is
   begin
      I := I + 1;
      P2 (I);
   end;

   procedure P2 (I : in out integer) is
   begin
      I := I + 1;
      P3 (I);
   end;

   procedure P3 (I : in out integer) is
   begin
      I := I + 1;
      P4 (I);
   end;

   procedure P4 (I : in out integer) is
   begin
      I := I + 1;
   end;


.. _Default_Parameters:

``Default_Parameters``
----------------------

.. index:: Default_Parameters

Flag formal part (in subprogram specifications and entry declarations)
if it defines more than N parameters with default values, when N is a
rule parameter. If no parameter is provided for the rule then all the
formal parts with defaulted parameters are flagged.

This rule has the following (optional) parameter for the ``+R`` option:

*N*
  Integer not less than 0 specifying the minimal allowed number of
  defaulted parameters.


.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 3,4

   procedure P (I : in out Integer; J : Integer := 0);  -- No FLAG (if parameter is 1)
   procedure Q (I : in out Integer; J : Integer);
   procedure R (I, J : Integer := 0; K : Integer := 0); --  FLAG (if parameter is 2 or less)
   procedure S (I : Integer; J, K : Integer := 0);      --  FLAG (if parameter is 2 or less)


.. _Discriminated_Records:

``Discriminated_Records``
-------------------------

.. index:: Discriminated_Records

Flag all declarations of record types with discriminants. Only the
declarations of record and record extension types are checked. Incomplete,
formal, private, derived and private extension type declarations are not
checked. Task and protected type declarations also are not checked.

This rule has no parameters.

.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 5, 9

   type Idx is range 1 .. 100;
   type Arr is array (Idx range <>) of Integer;
   subtype Arr_10 is Arr (1 .. 10);

   type Rec_1 (D : Idx) is record        --  FLAG
      A : Arr (1 .. D);
   end record;

   type Rec_2 (D : Idx) is record        --  FLAG
      B : Boolean;
   end record;

   type Rec_3 is record
      B : Boolean;
   end record;


.. _Explicit_Full_Discrete_Ranges:

``Explicit_Full_Discrete_Ranges``
---------------------------------

.. index:: Explicit_Full_Discrete_Ranges

Flag each discrete range that has the form ``A'First .. A'Last``.

This rule has no parameters.

.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 3

      subtype Idx is Integer range 1 .. 100;
   begin
      for J in Idx'First .. Idx'Last loop   --  FLAG
         K := K + J;
      end loop;

      for J in Idx loop
         L := L + J;
      end loop;


.. _Explicit_Inlining:

``Explicit_Inlining``
---------------------

.. index:: Explicit_Inlining

Flag a subprogram declaration, a generic subprogram declaration or
a subprogram instantiation if this declaration has an Inline aspect specified
or an Inline pragma applied to it. If a generic subprogram declaration
has an Inline aspect specified or pragma Inline applied, then only
generic subprogram declaration is flagged but not its instantiations.

This rule has no parameters.

.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 1, 4

   procedure Swap (I, J : in out Integer);                    --  FLAG
   pragma Inline (Swap);

   function Increment (I : Integer) return Integer is (I + 1) --  FLAG
     with Inline;


.. _Expression_Functions:

``Expression_Functions``
---------------------------------

.. index:: Expression_Functions

Flag each expression function declared in a package specification
(including specification of local packages and generic package
specifications).

This rule has no parameters.

.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 3

   package Foo is

      function F (I : Integer) return Integer is   --  FLAG
        (if I > 0 then I - 1 else I + 1);


.. _Fixed_Equality_Checks:

``Fixed_Equality_Checks``
-------------------------

.. index:: Fixed_Equality_Checks

Flag all explicit calls to the predefined equality operations for fixed-point
types. Both '``=``' and '``/=``' operations are checked.
User-defined equality operations are not flagged, nor are uses of operators
that are renamings of the predefined equality operations.
Also, the '``=``' and '``/=``' operations for floating-point types
are not flagged.

This rule has no parameters.

.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 11

   package Pack is
        type Speed is delta 0.01 range 0.0 .. 10_000.0;
        function Get_Speed return Speed;
   end Pack;

   with Pack; use Pack;
   procedure Process is
        Speed1 : Speed := Get_Speed;
        Speed2 : Speed := Get_Speed;

        Flag : Boolean := Speed1 = Speed2;     --  FLAG


.. _Float_Equality_Checks:

``Float_Equality_Checks``
-------------------------

.. index:: Float_Equality_Checks

Flag all explicit calls to the predefined equality operations for
floating-point types and private types whose completions are floating-point
types. Both '``=``' and '``/=``' operations are checked.
User-defined equality operations are not flagged, nor are uses of operators
that are renamings of the predefined equality operations.
Also, the '``=``' and '``/=``' operations for fixed-point types
are not flagged.

This rule has no parameters.

.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 11

   package Pack is
        type Speed is digits 0.01 range 0.0 .. 10_000.0;
        function Get_Speed return Speed;
   end Pack;

   with Pack; use Pack;
   procedure Process is
        Speed1 : Speed := Get_Speed;
        Speed2 : Speed := Get_Speed;

        Flag : Boolean := Speed1 = Speed2;     --  FLAG



.. _Function_Style_Procedures:

``Function_Style_Procedures``
-----------------------------

.. index:: Function_Style_Procedures

Flag each procedure that can be rewritten as a function. A procedure can be
converted into a function if it has exactly one parameter of mode ``out``
and no parameters of mode ``in out``, with no ``Global`` aspect
specified or with explicit specification that its ``Global`` aspect is set to
``null`` (either by aspect specification or by pragma Global). Procedure
declarations, formal procedure declarations, and generic procedure declarations
are always checked. Procedure
bodies and body stubs are flagged only if they do not have corresponding
separate declarations. Procedure renamings and procedure instantiations are
not flagged.

If a procedure can be rewritten as a function, but its ``out`` parameter is
of a limited type, it is not flagged.

Protected procedures are not flagged. Null procedures also are not flagged.

This rule has no parameters.

.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 2

   procedure Cannot_be_a_function (A, B : out Boolean);
   procedure Can_be_a_function (A : out Boolean);           --  FLAG

.. _Generic_IN_OUT_Objects:

``Generic_IN_OUT_Objects``
---------------------------

.. index:: Generic_IN_OUT_Objects

Flag declarations of generic formal objects of mode IN OUT.

This rule has no parameters.

.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 4

   generic
      I :        Integer;
      J : in     Integer;
      K : in out Integer;             --  FLAG
   package Pack_G is

.. _Generics_In_Subprograms:

``Generics_In_Subprograms``
---------------------------

.. index:: Generics_In_Subprograms

Flag each declaration of a generic unit in a subprogram. Generic
declarations in the bodies of generic subprograms are also flagged.
A generic unit nested in another generic unit is not flagged.
If a generic unit is
declared in a local package that is declared in a subprogram body, the
generic unit is flagged.

This rule has no parameters.

.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 3

   procedure Proc is

      generic                                --  FLAG
         type FT is range <>;
      function F_G (I : FT) return FT;

.. _Implicit_IN_Mode_Parameters:

``Implicit_IN_Mode_Parameters``
-------------------------------

.. index:: Implicit_IN_Mode_Parameters

Flag each occurrence of a formal parameter with an implicit ``in`` mode.
Note that ``access`` parameters, although they technically behave
like ``in`` parameters, are not flagged.

This rule has no parameters.

.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 1

   procedure Proc1 (I :    Integer);          --  FLAG
   procedure Proc2 (I : in Integer);
   procedure Proc3 (I :    access Integer);


.. _Improperly_Located_Instantiations:

``Improperly_Located_Instantiations``
-------------------------------------

.. index:: Improperly_Located_Instantiations

Flag all generic instantiations in library-level package specs
(including library generic packages) and in all subprogram bodies.

Instantiations in task and entry bodies are not flagged. Instantiations in the
bodies of protected subprograms are flagged.

This rule has no parameters.

.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 3

   with Ada.Text_IO; use Ada.Text_IO;
   procedure Proc is
      package My_Int_IO is new Integer_IO (Integer);   --  FLAG


.. _Library_Level_Subprograms:

``Library_Level_Subprograms``
-----------------------------

.. index:: Library_Level_Subprograms

Flag all library-level subprograms (including generic
subprogram instantiations).

This rule has no parameters.

.. code-block:: ada
   :emphasize-lines: 2

   with Ada.Text_IO; use Ada.Text_IO;
   procedure Proc is                         --  FLAG

.. _Membership_Tests:

``Membership_Tests``
---------------------------

.. index:: Membership_Tests

Flag use of membership test expression.

This rule has the following (optional) parameters for the ``+R`` option:

*Multi_Alternative_Only*
  Flag only those membership test expressions that have more than one
  membership choice in the membership choice list.

*Float_Types_Only*
  Flag only those membership test expressions that checks objects of floating
  point type and private types whose completions are floating-point types.

*Except_Assertions*
  Do not flag a membership test expression if it is a subcomponent
  of the following constructs:



*argument of the following pragmas*


*Language-defined*

*
  ``Assert``


*GNAT-specific*

*
  ``Assert_And_Cut``

*
  ``Assume``

*
  ``Contract_Cases``

*
  ``Debug``

*
  ``Invariant``

*
  ``Loop_Invariant``

*
  ``Loop_Variant``

*
  ``Postcondition``

*
  ``Precondition``

*
  ``Predicate``

*
  ``Refined_Post``



*definition of the following aspects*


*Language-defined*

*
  ``Static_Predicate``

*
  ``Dynamic_Predicate``

*
  ``Pre``

*
  ``Pre'Class``

*
  ``Post``

*
  ``Post'Class``

*
  ``Type_Invariant``

*
  ``Type_Invariant'Class``


*GNAT-specific*

*
  ``Contract_Cases``

*
  ``Invariant``

*
  ``Invariant'Class``

*
  ``Predicate``

*
  ``Refined_Post``


These three parameters are independent on each other.


.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 3

   procedure Proc (S : in out Speed) is
   begin
      if S in Low .. High then      --  FLAG


.. _Non_Qualified_Aggregates:

``Non_Qualified_Aggregates``
----------------------------

.. index:: Non_Qualified_Aggregates

Flag each non-qualified aggregate.
A non-qualified aggregate is an
aggregate that is not the expression of a qualified expression. A
string literal is not considered an aggregate, but an array
aggregate of a string type is considered as a normal aggregate.
Aggregates of anonymous array types are not flagged.

This rule has no parameters.

.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 3

   type Arr is array (1 .. 10) of Integer;

   Var1 : Arr := (1 => 10, 2 => 20, others => 30);             --  FLAG
   Var2 : array (1 .. 10) of Integer := (1 => 10, 2 => 20, others => 30);

.. _Number_Declarations:

``Number_Declarations``
-----------------------

.. index:: Number_Declarations

Number declarations are flagged.

This rule has no parameters.

.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 1, 2

   Num1 : constant := 13;                 --  FLAG
   Num2 : constant := 1.3;                --  FLAG

   Const1 : constant Integer := 13;
   Const2 : constant Float := 1.3;

.. _Numeric_Indexing:

``Numeric_Indexing``
--------------------

.. index:: Numeric_Indexing

Flag numeric literals, including those preceded by a predefined unary minus,
if they are used as index expressions in array components.
Literals that are subcomponents of index expressions are not flagged
(other than the aforementioned case of unary minus).


This rule has no parameters.

.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 5

   procedure Proc is
      type Arr is array (1 .. 10) of Integer;
      Var : Arr;
   begin
      Var (1) := 10;       --  FLAG


.. _Numeric_Literals:

``Numeric_Literals``
--------------------

.. index:: Numeric_Literals

Flag each use of a numeric literal except for the following:

*
  a literal occurring in the initialization expression for a constant
  declaration or a named number declaration, or

*
  a literal occurring in an aspect definition or in an aspect clause, or

*
  an integer literal that is less than or equal to a value
  specified by the *N* rule parameter, or

*
  an integer literal that is the right operand of an infix call to an
  exponentiation operator, or

*
  an integer literal that denotes a dimension in array types attributes
  ``First``, ``Last`` and ``Length``, or

*
  a literal occurring in a declaration in case the *Statements_Only*
  rule parameter is given.

This rule may have the following parameters for the ``+R`` option:



*N*
  *N* is an integer literal used as the maximal value that is not flagged
  (i.e., integer literals not exceeding this value are allowed)


``ALL``
  All integer literals are flagged


``Statements_Only``
  Numeric literals are flagged only when used in statements

If no parameters are set, the maximum unflagged value is 1, and the check for
literals is not limited by statements only.

The last specified check limit (or the fact that there is no limit at
all) is used when multiple ``+R`` options appear.

The ``-R`` option for this rule has no parameters.
It disables the rule and restores its default operation mode.
If the ``+R`` option subsequently appears, will be 1, and the check will
not be limited by statements only.

.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 3

   C1 : constant Integer := 10;
   V1 :          Integer := C1;
   V2 :          Integer := 20;      --  FLAG


.. _Parameters_Out_Of_Order:

``Parameters_Out_Of_Order``
---------------------------

.. index:: Parameters_Out_Of_Order

Flag each parameter specification if it does not follow the required
ordering of parameter specifications in a formal part. The required
order may be specified by the following rule parameters:

*in*
  ``in`` non-access parameters without initialization expressions;

*access*
  ``access`` parameters  without initialization expressions;

*in_out*
  ``in out`` parameters;

*out*
  ``out`` parameters;

*defaulted_in*
  parameters with initialization expressions (the order of ``access``
  and non-access parameters is not checked.

When the rule is used with parameters, all the five parameters should
be given, and each parameter should be specified only once.

The rule can be called without parameters, in this case it checks the
default ordering that corresponds to the order in which the
rule parameters are listed above.


.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 1

   procedure Proc1 (I : in out Integer; B : Boolean) is    --  FLAG


.. _Predicate_Testing:

``Predicate_Testing``
---------------------------

.. index:: Predicate_Testing

Flag a membership test if at least one of its membership choice contains a
subtype mark denoting a subtype defined with (static or dynamic)
subtype predicate.

Flags 'Valid attribute reference if the nominal subtype of the attribute
prefix has (static or dynamic) subtype predicate.


This rule has the following (optional) parameters for the ``+R`` option:

*Except_Assertions*
  Do not flag the use of non-short-circuit_operators inside
  assertion-related pragmas or aspect specifications.

A pragma or an aspect is considered as assertion-related if its name
is from the following list:

*
  ``Assert``
*
  ``Assert_And_Cut``
*
  ``Assume``
*
  ``Contract_Cases``
*
  ``Debug``
*
  ``Default_Initial_Condition``
*
  ``Dynamic_Predicate``
*
  ``Invariant``
*
  ``Loop_Invariant``
*
  ``Loop_Variant``
*
  ``Post``
*
  ``Postcondition``
*
  ``Pre``
*
  ``Precondition``
*
  ``Predicate``
*
  ``Predicate_Failure``
*
  ``Refined_Post``
*
  ``Static_Predicate``
*
  ``Type_Invariant``


.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 7

   with Support; use Support;
   package Pack is
      subtype Even is Integer with Dynamic_Predicate => Even mod 2 = 0;

      subtype Small_Even is Even range -100 .. 100;

      B1 : Boolean := Ident (101) in Small_Even;      --  FLAG


.. _Relative_Delay_Statements:

``Relative_Delay_Statements``
----------------------------------

.. index:: Relative_Delay_Statements

Relative delay statements are flagged. Delay until statements are not
flagged.

This rule has no parameters.

.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 4

   if I > 0 then
      delay until Current_Time + Big_Delay;
   else
      delay Small_Delay;                      --  FLAG
   end if;

.. _Renamings:

``Renamings``
-------------

.. index:: Renamings

Flag renaming declarations.

This rule has no parameters.

.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 2

   I : Integer;
   J : Integer renames I;     --  FLAG

.. _Representation_Specifications:

``Representation_Specifications``
----------------------------------

.. index:: Representation_Specifications

Flag each record representation clause, enumeration representation
clause and representation attribute clause. Flag each aspect definition
that defines a representation aspect. Also flag any pragma that is
classified by the Ada Standard as a representation pragma, and the
definition of the corresponding aspects.

The rule has an optional parameter for +R option:

*Record_Rep_Clauses_Only*
  Only record representation clauses are flagged.

.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 5, 8, 11

   type State         is (A,M,W,P);
   type Mode          is (Fix, Dec, Exp, Signif);

   type Byte_Mask     is array (0..7)  of Boolean
     with Component_Size => 1;                                --  FLAG

   type State_Mask    is array (State) of Boolean
     with Component_Size => 1;                                --  FLAG

   type Mode_Mask     is array (Mode)  of Boolean;
   for Mode_Mask'Component_Size use 1;                        --  FLAG


.. _Quantified_Expressions:

``Quantified_Expressions``
--------------------------

.. index:: Quantified_Expressions

Flag use of quantified expression.

This rule has the following (optional) parameters for the ``+R`` option:



*Except_Assertions*
  Do not flag a conditional expression if it is a subcomponent
  of the following constructs:



*argument of the following pragmas*


*Language-defined*

*
  ``Assert``


*GNAT-specific*

*
  ``Assert_And_Cut``

*
  ``Assume``

*
  ``Contract_Cases``

*
  ``Debug``

*
  ``Invariant``

*
  ``Loop_Invariant``

*
  ``Loop_Variant``

*
  ``Postcondition``

*
  ``Precondition``

*
  ``Predicate``

*
  ``Refined_Post``



*definition of the following aspects*


*Language-defined*

*
  ``Static_Predicate``

*
  ``Dynamic_Predicate``

*
  ``Pre``

*
  ``Pre'Class``

*
  ``Post``

*
  ``Post'Class``

*
  ``Type_Invariant``

*
  ``Type_Invariant'Class``


*GNAT-specific*

*
  ``Contract_Cases``

*
  ``Invariant``

*
  ``Invariant'Class``

*
  ``Predicate``

*
  ``Refined_Post``

.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 5, 6

   subtype Ind is Integer range 1 .. 10;
   type Matrix is array (Ind, Ind) of Integer;

   function Check_Matrix (M : Matrix) return Boolean is
     (for some I in Ind =>                               --  FLAG
        (for all J in Ind => M (I, J) = 0));             --  FLAG


.. _Raising_Predefined_Exceptions:

``Raising_Predefined_Exceptions``
---------------------------------

.. index:: Raising_Predefined_Exceptions

Flag each ``raise`` statement that raises a predefined exception
(i.e., one of the exceptions ``Constraint_Error``, ``Numeric_Error``,
``Program_Error``, ``Storage_Error``, or ``Tasking_Error``).

This rule has no parameters.

.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 2

   begin
      raise Constraint_Error;    --  FLAG


.. _Separates:

``Separates``
-------------

.. index:: Separates

Flags subunits.

This rule has no parameters.

.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 7

   package body P is

      procedure Sep is separate;

   end P;

   separate(P)       --  FLAG
   procedure Sep is
      procedure Q is separate;
   begin
      null;
   end Sep;



.. _Simple_Loop_Statements:

``Simple_Loop_Statements``
--------------------------

.. index:: Simple_Loop_Statements

Flags simple loop statements (loop statements that do not
have iteration schemes).

This rule has no parameters.

.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 1

   loop                    --  FLAG
      I := I + 1;
      exit when I > 10;
   end loop;

   while I > 0 loop        --  NO FLAG
      I := I - 1;
   end loop;


.. _Subprogram_Access:

``Subprogram_Access``
---------------------

.. index:: Subprogram_Access

Flag all constructs that belong to access_to_subprogram_definition
syntax category, and all access definitions that define access to
subprogram.

This rule has no parameters.

.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 1, 5

   type Proc_A is access procedure ( I : Integer);       --  FLAG

   procedure Proc
     (I       : Integer;
      Process : access procedure (J : in out Integer));  --  FLAG


.. _Too_Many_Dependencies:

``Too_Many_Dependencies``
-------------------------

.. index:: Too_Many_Dependencies

Flag a library item or a subunit that immediately depends on more than
N library units (N is a rule parameter). In case of a dependency on
child units, implicit or explicit dependencies on all their parents are
not counted.

This rule has the following (mandatory) parameters for the ``+R`` option:

*N*
  Positive integer specifying the maximal number of dependencies when
  the library item or subunit is not flagged.

.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 8

   --  if rule parameter is 5 or smaller:
   with Pack1;
   with Pack2;
   with Pack3;
   with Pack4;
   with Pack5;
   with Pack6;
   procedure Main is               --  FLAG


.. _Unassigned_OUT_Parameters:

``Unassigned_OUT_Parameters``
-----------------------------

.. index:: Unassigned_OUT_Parameters

Flag procedures' ``out`` parameters that are not assigned.

An ``out`` parameter is flagged if the *sequence of statements* of
the procedure body (before the procedure body's exception part, if any)
contains no assignment to the parameter.

An ``out`` parameter is flagged in an *exception handler* in the exception
part of the procedure body, if the *exception handler* contains neither an
assignment to the parameter nor a raise statement.

Bodies of generic procedures are also considered.

The following are treated as assignments to an ``out`` parameter:

*
  an assignment statement, with the parameter or some component as the target

*
  passing the parameter (or one of its components) as an ``out`` or
  ``in out`` parameter, except for the case when it is passed to the
  call of an attribute subprogram.

The rule has an optional parameter for +R option:

*Ignore_Component_Assignments*
  Ignore assignments to subcomponents of an ``out`` parameter when detecting
  if the parameter is assigned.

.. warning:: This rule only detects a trivial case of an unassigned variable
   and doesn't provide a guarantee that there is no uninitialized access.
   It is not a replacement for rigorous check for uninitialized access provided
   by advanced static analysis tools.

.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 1

   procedure Proc                --  FLAG
     (I    : Integer;
      Out1 : out Integer;
      Out2 : out Integer)
   is
   begin
      Out1 := I + 1;
   end Proc;


.. _Unconstrained_Array_Returns:

``Unconstrained_Array_Returns``
-------------------------------

.. index:: Unconstrained_Array_Returns

Flag each function returning an unconstrained array. Function declarations,
function bodies (and body stubs) having no separate specifications,
and generic function instantiations are flagged.
Function calls and function renamings are
not flagged.

Generic function declarations, and function declarations in generic
packages, are not flagged.  Instead, this rule flags the results of
generic instantiations (that is, expanded specification and expanded
body corresponding to an instantiation).

This rule has the following (optional) parameters for the ``+R`` option:



*Except_String*
  Do not flag functions that return the predefined ``String`` type or a type
  derived from it, directly or indirectly.

.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 4

   type Arr is array (Integer range <>) of Integer;
   subtype Arr_S is Arr (1 .. 10);

   function F1 (I : Integer) return Arr;      --  FLAG
   function F2 (I : Integer) return Arr_S;


.. _Unconstrained_Arrays:

``Unconstrained_Arrays``
------------------------

.. index:: Unconstrained_Arrays

Unconstrained array definitions are flagged.

This rule has no parameters.

.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 3

   type Idx is range -100 .. 100;

   type U_Arr is array (Idx range <>) of Integer;      --  FLAG
   type C_Arr is array (Idx) of Integer;


Metrics-Related Rules
=====================

.. index:: Metrics-related rules

The rules in this section can be used to enforce compliance with
specific code metrics, by checking that the metrics computed for a program
lie within user-specifiable bounds.
Depending on the metric, there may be a lower bound, an upper bound, or both.
A construct is flagged if the value of the metric exceeds the upper bound
or is less than the lower bound.

The name of any metrics rule consists of the prefix ``Metrics_``
followed by the name of the corresponding metric:
``Essential_Complexity``, ``Cyclomatic_Complexity``, or
``LSLOC``.
(The 'LSLOC' acronym stands for 'Logical Source Lines Of Code'.)
The meaning and the computed values of the metrics are
the same as in *gnatmetric*.

For the ``+R`` option, each metrics rule has a numeric parameter
specifying the bound (integer or real, depending on a metric).
The ``-R``
option for the metrics rules does not have a parameter.

*Example:* the rule

::

  +RMetrics_Cyclomatic_Complexity : 7


means that all bodies with cyclomatic complexity exceeding 7 will be flagged.

To turn OFF the check for cyclomatic complexity metric,
use the following option:

::

  -RMetrics_Cyclomatic_Complexity


.. _Metrics_Essential_Complexity:

``Metrics_Essential_Complexity``
--------------------------------

.. index:: Metrics_Essential_Complexity

The ``Metrics_Essential_Complexity`` rule takes a positive integer as
upper bound.  A program unit that is an executable body exceeding this limit will be flagged.

The Ada essential complexity metric is a McCabe cyclomatic complexity metric counted
for the code that is reduced by excluding all the pure structural Ada control statements.

.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 2

   --  if the rule parameter is 3 or less
   procedure Proc (I : in out Integer; S : String) is   --  FLAG
   begin
      if I in 1 .. 10 then
         for J in S'Range loop

            if S (J) = ' ' then
               if I > 10 then
                  exit;
               else
                  I := 10;
               end if;
            end if;

            I := I + Character'Pos (S (J));
         end loop;
      end if;
   end Proc;


.. _Metrics_Cyclomatic_Complexity:

``Metrics_Cyclomatic_Complexity``
---------------------------------

.. index:: Metrics_Cyclomatic_Complexity

The ``Metrics_Cyclomatic_Complexity`` rule takes a positive integer as
upper bound.  A program unit that is an executable body exceeding this limit will be flagged.

This rule has the following optional parameter for the ``+R`` option:

*Exempt_Case_Statements*
  Count the complexity introduced by ``CASE`` statement or ``CASE`` expression as 1.

The McCabe cyclomatic complexity metric is defined
in `http://www.mccabe.com/pdf/mccabe-nist235r.pdf <http://www.mccabe.com/pdf/mccabe-nist235r.pdf>`_
The goal of cyclomatic complexity metric is to estimate the number
of independent paths in the control flow graph that in turn gives the number
of tests needed to satisfy paths coverage testing completeness criterion.

.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 2

   --  if the rule parameter is 6 or less
   procedure Proc (I : in out Integer; S : String) is   --  FLAG
   begin
      if I in 1 .. 10 then
         for J in S'Range loop

            if S (J) = ' ' then
               if I < 10 then
                  I := 10;
               end if;
            end if;

            I := I + Character'Pos (S (J));
         end loop;
      elsif S = "abs" then
         if I > 0 then
            I := I + 1;
         end if;
      end if;
   end Proc;


.. _Metrics_LSLOC:

``Metrics_LSLOC``
-----------------

.. index:: Metrics_LSLOC

The ``Metrics_LSLOC`` rule takes a positive integer as
upper bound.  A program unit declaration or a program unit body exceeding
this limit will be flagged.

The metric counts the total number of declarations and the total number of statements.

This rule contains optional parameters for ``+R`` option that allows to restrict the
rule to specific constructs:

*Subprograms*
   Check the rule for subprogram bodies only.

.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 2

   --  if the rule parameter is 20 or less
   package Pack is                             --  FLAG
      procedure Proc1 (I : in out Integer);
      procedure Proc2 (I : in out Integer);
      procedure Proc3 (I : in out Integer);
      procedure Proc4 (I : in out Integer);
      procedure Proc5 (I : in out Integer);
      procedure Proc6 (I : in out Integer);
      procedure Proc7 (I : in out Integer);
      procedure Proc8 (I : in out Integer);
      procedure Proc9 (I : in out Integer);
      procedure Proc10 (I : in out Integer);
   end Pack;

SPARK 2005 Rules
================

.. index:: SPARK related rules

The rules in this section can be used to enforce
compliance with the Ada subset allowed by the SPARK 2005 language.

More recent versions of SPARK support these language constructs,
so if you want to further restrict the SPARK constructs allowed
in your coding standard, you can use some of the following rules.

.. _Annotated_Comments:

``Annotated_Comments``
----------------------

.. index:: Annotated_Comments

Flags comments that are used as annotations or as
special sentinels/markers. Such comments have the following
structure::

    --<special_character> <comment_marker>

where

*<special_character>* is a character (such as '#', '$', '|' etc.)
  indicating that the comment is used for a specific purpose

*<comment_marker>* is a word identifying the annotation or special usage
  (word here is any sequence of characters except white space)

There may be any amount of white space (including none at all) between
``<special_character>`` and ``<comment_marker>``, but no white space
is permitted between ``'--'`` and ``<special_character>``. (A
white space here is either a space character or horizontal tabulation)

``<comment_marker>`` must not contain any white space.

``<comment_marker>`` may be empty, in which case the rule
flags each comment that starts with ``--<special_character>`` and
that does not contain any other character except white space

The rule has the following (mandatory) parameter for the ``+R`` option:

*S*
  String with the following interpretation: the first character
  is the special comment character, and the rest is
  the comment marker. S must not contain white space.

The ``-R`` option erases all definitions of special comment annotations
specified by the previous +R options.

The rule is case-sensitive.

Example:

The rule

::

  +RAnnotated_Comments:#hide

will flag the following comment lines

.. code-block:: ada

  --#hide
  --# hide
  --#           hide

     I := I + 1; --# hide

But the line

.. code-block:: ada

  -- # hide

will not be flagged, because of the space between '--' and '#'.

The line

.. code-block:: ada

  --#Hide

will not be flagged, because the string parameter is case sensitive.

.. _Boolean_Relational_Operators:

``Boolean_Relational_Operators``
--------------------------------

.. index:: Boolean_Relational_Operators

Flag each call to a predefined relational operator ('<', '>', '<=',
'>=', '=' and '/=') for the predefined Boolean type.
(This rule is useful in enforcing the SPARK language restrictions.)

Calls to predefined relational operators of any type derived from
``Standard.Boolean`` are not detected.  Calls to user-defined functions
with these designators, and uses of operators that are renamings
of the predefined relational operators for ``Standard.Boolean``,
are likewise not detected.

This rule has no parameters.

.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 3

      procedure Proc (Flag_1 : Boolean; Flag_2 : Boolean; I : in out Integer) is
      begin
         if Flag_1 >= Flag_2 then     --  FLAG


.. _Expanded_Loop_Exit_Names:

``Expanded_Loop_Exit_Names``
----------------------------

.. index:: Expanded_Loop_Exit_Names

Flag all expanded loop names in ``exit`` statements.

This rule has no parameters.

.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 6

   procedure Proc (S : in out String) is
   begin
      Search : for J in S'Range loop
         if S (J) = ' ' then
            S (J) := '_';
            exit Proc.Search;            --  FLAG
         end if;
      end loop Search;
   end Proc;


.. _Non_SPARK_Attributes:

``Non_SPARK_Attributes``
------------------------

.. index:: Non_SPARK_Attributes

The SPARK language defines the following subset of Ada 95 attribute
designators as those that can be used in SPARK programs. The use of
any other attribute is flagged.

* ``'Adjacent``
* ``'Aft``
* ``'Base``
* ``'Ceiling``
* ``'Component_Size``
* ``'Compose``
* ``'Copy_Sign``
* ``'Delta``
* ``'Denorm``
* ``'Digits``
* ``'Exponent``
* ``'First``
* ``'Floor``
* ``'Fore``
* ``'Fraction``
* ``'Last``
* ``'Leading_Part``
* ``'Length``
* ``'Machine``
* ``'Machine_Emax``
* ``'Machine_Emin``
* ``'Machine_Mantissa``
* ``'Machine_Overflows``
* ``'Machine_Radix``
* ``'Machine_Rounds``
* ``'Max``
* ``'Min``
* ``'Model``
* ``'Model_Emin``
* ``'Model_Epsilon``
* ``'Model_Mantissa``
* ``'Model_Small``
* ``'Modulus``
* ``'Pos``
* ``'Pred``
* ``'Range``
* ``'Remainder``
* ``'Rounding``
* ``'Safe_First``
* ``'Safe_Last``
* ``'Scaling``
* ``'Signed_Zeros``
* ``'Size``
* ``'Small``
* ``'Succ``
* ``'Truncation``
* ``'Unbiased_Rounding``
* ``'Val``
* ``'Valid``

This rule has no parameters.

.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 4

   type Integer_A is access all Integer;

   Var : aliased Integer := 1;
   Var_A : Integer_A := Var'Access;  --  FLAG


.. _Non_Tagged_Derived_Types:

``Non_Tagged_Derived_Types``
----------------------------

.. index:: Non_Tagged_Derived_Types

Flag all derived type declarations that do not have a record extension part.

This rule has no parameters.

.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 5

   type Coordinates is record
      X, Y, Z : Float;
   end record;

   type Hidden_Coordinates is new Coordinates;   --  FLAG


.. _Outer_Loop_Exits:

``Outer_Loop_Exits``
--------------------

.. index:: Outer_Loop_Exits

Flag each ``exit`` statement containing a loop name that is not the name
of the immediately enclosing ``loop`` statement.

This rule has no parameters.

.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 5

   Outer : for J in S1'Range loop
      for K in S2'Range loop
         if S1 (J) = S2 (K) then
            Detected := True;
            exit Outer;                     --  FLAG
         end if;
      end loop;
   end loop Outer;


.. _Overloaded_Operators:

``Overloaded_Operators``
------------------------

.. index:: Overloaded_Operators

Flag each function declaration that overloads an operator symbol.
A function body or an expression function is checked only if it
does not have a separate spec. Formal functions are also checked. For a
renaming declaration, only renaming-as-declaration is checked.

This rule has no parameters.

.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 6

   type Rec is record
      C1 : Integer;
      C2 : Float;
   end record;

   function "<" (Left, Right : Rec) return Boolean;    --  FLAG

.. _Slices:

``Slices``
----------

.. index:: Slices

Flag all uses of array slicing

This rule has no parameters.

.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 2

   procedure Proc (S : in out String; L, R : Positive) is
      Tmp : String := S (L .. R);        --  FLAG
   begin


.. _Universal_Ranges:

``Universal_Ranges``
--------------------

.. index:: Universal_Ranges

Flag discrete ranges that are a part of an index constraint, constrained
array definition, or ``for``-loop parameter specification, and whose bounds
are both of type *universal_integer*. Ranges that have at least one
bound of a specific type (such as ``1 .. N``, where ``N`` is a variable
or an expression of non-universal type) are not flagged.

This rule has no parameters.

.. rubric:: Example

.. code-block:: ada
   :emphasize-lines: 4

   L : Positive := 1;

   S1 : String (L .. 10);
   S2 : String (1 .. 10);     --  FLAG

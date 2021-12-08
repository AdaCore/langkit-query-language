------------------------------------------------------------------------------
--                                                                          --
--                                   LKQL                                   --
--                                                                          --
--                     Copyright (C) 2019-2021, AdaCore                     --
--                                                                          --
-- LKQL is free software;  you can redistribute it and/or modify  it        --
-- under terms of the GNU General Public License  as published by the Free  --
-- Software Foundation;  either version 3,  or (at your option)  any later  --
-- version.   This  software  is distributed in the hope that it  will  be  --
-- useful but  WITHOUT ANY WARRANTY;  without even the implied warranty of  --
-- MERCHANTABILITY  or  FITNESS  FOR  A PARTICULAR PURPOSE.                 --
--                                                                          --
-- As a special  exception  under  Section 7  of  GPL  version 3,  you are  --
-- granted additional  permissions described in the  GCC  Runtime  Library  --
-- Exception, version 3.1, as published by the Free Software Foundation.    --
--                                                                          --
-- You should have received a copy of the GNU General Public License and a  --
-- copy of the GCC Runtime Library Exception along with this program;  see  --
-- the files COPYING3 and COPYING.RUNTIME respectively.  If not, see        --
-- <http://www.gnu.org/licenses/>.                                          --
------------------------------------------------------------------------------

with Ada.Containers.Hashed_Maps;
with Ada.Containers.Vectors;
with Ada.Containers; use Ada.Containers;
with Ada.Unchecked_Deallocation;

with Langkit_Support.Text;   use Langkit_Support.Text;

with Options;
with Iters.Iterators;
with Iters.Vec_Iterators;
with LKQL.Partial_AST_Nodes; use LKQL.Partial_AST_Nodes;
with LKQL.Selector_Lists;    use LKQL.Selector_Lists;
with LKQL.Adaptive_Integers; use LKQL.Adaptive_Integers;

limited with LKQL.Eval_Contexts;

package LKQL.Primitives is

   Unsupported_Error : exception;

   type Primitive_List;
   --  List of Primitive Values

   type Primitive_List_Access is access Primitive_List;
   --  Pointer to a list of Primitive values

   type Primitive_Assocs;

   type Primitive_Assocs_Access is access Primitive_Assocs;

   type Iterator_Primitive;

   type Iterator_Primitive_Access is access all Iterator_Primitive;

   type Base_Primitive_Kind is
     (Kind_Unit,
      --  Unit value: representation of the result of a computation that
      --  doesn't produce a meaningful result.

      Kind_Int,
      --  Integer value, encoded as an Ada Integer or Big Integer

      Kind_Str,
      --  Unicode String value

      Kind_Bool,
      --  Either 'true' or 'false'

      Kind_Node,
      --  Libadalang node

      Kind_Iterator,
      --  Iterator yielding Primitive values

      Kind_List,
      --  List of Primitive values

      Kind_Object,
      --  object

      Kind_Tuple,
      --  Tuple of primitive values

      Kind_Selector_List,
      --  Lazy 'list' returned by a selector

      Kind_Function,
      --  Functions objects

      Kind_Builtin_Function,
      --  Builtin function objects

      Kind_Namespace,
      --  Namespace objects

      Kind_Property_Reference,
      --  Reference to a Langkit property

      Kind_Token,
      --  Langkit token

      Kind_Analysis_Unit,
      --  Langkit analysis unit

      Kind_Selector,
      --  Selector objects

      No_Kind
      --  Special value that allows using this enum as an option type
   );
   --  Denotes the kind of a primitive value

   subtype Valid_Primitive_Kind is Base_Primitive_Kind
      range Kind_Unit .. Kind_Selector;

   subtype Sequence_Kind is Valid_Primitive_Kind
   range Kind_Iterator .. Kind_List;

   subtype Introspectable_Kind is Valid_Primitive_Kind
     with Static_Predicate =>
       Introspectable_Kind not in
         Kind_Tuple | Kind_Unit | Kind_Function
           | Kind_Selector | Kind_Builtin_Function | Kind_Namespace
             | Kind_Property_Reference | Kind_Object;

   type Environment_Access is access all LKQL.Eval_Contexts.Environment;

   type Builtin_Function_Description (<>);
   --  Forward definition of the Builtin_Function_Description type.

   type Builtin_Function is access all Builtin_Function_Description;
   --  This is a painful indirection due to the fact that we cannot declare an
   --  access to function returning a primitive before the primitive type is
   --  defined, but that we need to store it in Primitive_Data. This incurs
   --  a performance penalty since we have to dynamically allocate those, but
   --  since builtin functions will be allocated only once this is better than
   --  storing an address and coercing, because it will allow a friendlier API.

   type Primitive_Data;

   ---------------
   -- Primitive --
   ---------------

   type Primitive is access all Primitive_Data;

   function Deep_Equals
     (Left, Right : Primitive) return Boolean;
   --  Perform a deep equality check between 'Left' and 'Right'.
   --  An Unsupported exception will be raised if Left and Right have different
   --  kinds.

   function Equals (Left, Right : Primitive) return Boolean;

   package Primitive_Vectors is new
     Ada.Containers.Vectors (Index_Type   => Positive,
                             Element_Type => Primitive,
                             "="          => Equals);
   --  Vector of Primitive values

   type Primitive_Vector_Access is access all Primitive_Vectors.Vector;
   --  Pointer to a vector of Primitive values

   type Primitive_List is record
      Elements      : aliased Primitive_Vectors.Vector;
   end record;
   --  List of primitive values.

   type Primitive_Array is array (Positive range <>) of Primitive;

   function Hash (Vec : Primitive_Vectors.Vector) return Hash_Type;

   procedure Free_Primitive_List is
     new Ada.Unchecked_Deallocation (Primitive_List, Primitive_List_Access);

   procedure Free_Primitive_Vector is new Ada.Unchecked_Deallocation
     (Primitive_Vectors.Vector, Primitive_Vector_Access);

   package Positive_Vectors
   is new Ada.Containers.Vectors (Positive, Positive);

   type Primitive_Pool_Data is record
      Primitives : Primitive_Vectors.Vector;
      --  List of primitives allocated in this pool
   end record;

   type Primitive_Pool is access all Primitive_Pool_Data;
   --  Pool of primitives: In LKQL, primitives are not managed by hand, but are
   --  instead allocated in a pool. The pool can then be freed all at once, or
   --  released from time to time.
   --
   --  This allows us to create pool for "analysis frames", and free them,
   --  which is a kind of poor man's garbage collection, but works well enough
   --  for our purpose.

   package Primitive_Pool_Vectors
   is new Ada.Containers.Vectors (Positive, Primitive_Pool);

   type Primitive_Pool_Stack is access all Primitive_Pool_Vectors.Vector;

   function Create return Primitive_Pool_Stack;

   procedure Destroy (Self : in out Primitive_Pool_Stack);

   procedure Mark (Pool_Stack : Primitive_Pool_Stack);
   --  Add a new mark in the pool. The next call to release will call every
   --  object allocated *after* mark has been called.

   procedure Release (Pool_Stack : Primitive_Pool_Stack);
   --  Remove all the objects from the pool up until the last mark.

   function Create_Primitive
     (Data : Primitive_Data) return Primitive;

   package Callable_Caches is
      type Cache is private;

      function Create (Pool : Primitive_Pool) return Cache;

      function Query
        (Self : Cache; Args : Primitive_Vectors.Vector) return Primitive;

      procedure Insert
        (Self : Cache; Args : Primitive_Vectors.Vector; Value : Primitive);

      No_Cache : constant Cache;

   private

      package Cache_Maps is new Ada.Containers.Hashed_Maps
        (Primitive_Vectors.Vector,
         Primitive,
         Hash => Hash,
         Equivalent_Keys => Primitive_Vectors."=");

      type Cache_Data is record
         Pool  : Primitive_Pool;
         Cache : Cache_Maps.Map;
      end record;

      type Cache is access all Cache_Data;

      No_Cache : constant Cache := null;

   end Callable_Caches;

   type Cached_Sel_Node is record
      Node : H.AST_Node_Holder;
      Mode : L.Selector_Expr_Mode;
   end record;

   pragma Warnings (Off, "padded");
   package Nodes_Vectors
   is new Ada.Containers.Vectors (Positive, Cached_Sel_Node);
   pragma Warnings (On, "padded");

   type Node_Vector is access all Nodes_Vectors.Vector;

   package Node_To_Nodes is new Ada.Containers.Hashed_Maps
    (H.AST_Node_Holder,
     Node_Vector,
     Hash            => H.Hash,
     Equivalent_Keys => H."=");

   type Sel_Cache_Type is access all Node_To_Nodes.Map;

   type Text_Type_Access is access all Text_Type;
   procedure Free
   is new Ada.Unchecked_Deallocation (Text_Type, Text_Type_Access);

   type Primitive_Data (Kind : Valid_Primitive_Kind) is record
      Pool : Primitive_Pool;
      case Kind is
         when Kind_Unit =>
            null;
         when Kind_Int =>
            Int_Val           : Adaptive_Integer;
         when Kind_Str =>
            Str_Val           : Text_Type_Access;
         when Kind_Bool =>
            Bool_Val          : Boolean;
         when Kind_Node =>
            Node_Val          : H.AST_Node_Holder;
         when Kind_Token =>
            Token_Val         : H.AST_Token_Holder;
         when Kind_Analysis_Unit =>
            Analysis_Unit_Val : H.AST_Unit_Holder;
         when Kind_Iterator =>
            Iter_Val          : Iterator_Primitive_Access;
            Iter_Cache        : Primitive_List_Access;
         when Kind_List | Kind_Tuple =>
            List_Val          : Primitive_List_Access;
         when Kind_Object =>
            Obj_Assocs        : Primitive_Assocs_Access;
         when Kind_Selector_List =>
            Selector_List_Val : Selector_List;
         when Kind_Builtin_Function =>
            Builtin_Fn        : Builtin_Function;
         when Kind_Property_Reference =>
            Ref               : H.AST_Node_Member_Ref_Holder;
            Property_Node     : H.AST_Node_Holder;
         when Kind_Namespace =>
            Namespace         : Environment_Access;
            Module            : L.LKQL_Node;
         when Kind_Function | Kind_Selector =>
            Frame             : Environment_Access;
            case Kind is
               when Kind_Function =>
                  Fun_Node    : L.Base_Function;
                  Call_Cache  : Callable_Caches.Cache;
               when Kind_Selector =>
                  Sel_Node    : L.Selector_Decl;
                  Sel_Cache   : Sel_Cache_Type;
               when others => null;
            end case;
      end case;
   end record;
   --  Store a primitive value, which can be an atomic type (Bool, Int, ...),
   --  an AST node, or a list of Primitive values.

   procedure Release (Data : in out Primitive_Data);
   --  Release if data is of Kind Kind_List, free the list's memory

   function Hash (Self : Primitive) return Hash_Type;
   --  Hash function for LKQL's primitive type

   function Copy (Self : Primitive; Pool : Primitive_Pool) return Primitive;
   --  Deep copy of a primitive value

   subtype Introspectable_Primitive is Primitive
     with Predicate =>
       Introspectable_Primitive.Kind
         in Introspectable_Kind
         or else raise Constraint_Error
           with "Wrong kind for Introspectable_Primitive: "
           & Introspectable_Primitive.Kind'Image;

   package Primitive_Options is new Options (Primitive);
   --  Optional Primitive values

   subtype Primitive_Option is Primitive_Options.Option;
   --  Optional primitive value

   type Native_Function_Access is access function
     (Ctx  : LKQL.Eval_Contexts.Eval_Context;
      Args : Primitive_Array) return Primitive;

   type Builtin_Param_Description is record
      Name          : Unbounded_Text_Type;
      Expected_Kind : Base_Primitive_Kind := No_Kind;
      Default_Value : Primitive_Option := Primitive_Options.None;
   end record;

   type Builtin_Function_Profile is
     array (Positive range <>) of Builtin_Param_Description;

   Empty_Profile : constant Builtin_Function_Profile (1 .. 0) :=
     (others => <>);

   type Builtin_Function_Description (N : Natural) is record
      Name           : Unbounded_Text_Type;
      --  Name of the built-in function

      Params         : Builtin_Function_Profile (1 .. N);
      --  Parameters descriptions

      Fn_Access      : Native_Function_Access;
      --  Access to the native function to call for this function

      Doc            : Unbounded_Text_Type;
      --  Documentation for the builtin

      Only_Dot_Calls : Boolean;
      --  Whether this builtin can be called via the regular call syntax, or
      --  only via dot calls on entities with the same kind as the function's
      --  first argument.
   end record;

   -----------------------
   --  Primitive_Assocs --
   -----------------------

   package Primitive_Maps is new
     Ada.Containers.Hashed_Maps
       (Key_Type        => Symbol_Type,
        Element_Type    => Primitive,
        Hash            => Hash,
        Equivalent_Keys => "=");

   type Primitive_Assocs is record
      Elements : Primitive_Maps.Map;
   end record;

   procedure Free_Primitive_Assocs is new Ada.Unchecked_Deallocation
     (Primitive_Assocs, Primitive_Assocs_Access);

   --------------
   -- Iterator --
   --------------

   package Primitive_Iters is new Iters.Iterators (Primitive);
   --  Iterator over Primitive values

   package Primitive_Vec_Iters is
     new Iters.Vec_Iterators (Primitive_Vectors, Primitive_Iters);
   --  Iterators over Primitive vectors

   subtype Primitive_Iter is Primitive_Iters.Iterator_Interface;

   subtype Primitive_Iter_Access is Primitive_Iters.Iterator_Access;
   --  Pointer to an iterator over Primitive values

   type Iterator_Primitive is record
      Iter          : Primitive_Iter_Access;
   end record;
   --  Lazy stream of Primitive_values

   function Get_Iter (Value : Iterator_Primitive) return Primitive_Iter_Access;
   --  Return a deep copy of the wrapped iterator

   procedure Consume (Iter : Primitive; Num_Elements : Integer := -1)
      with Pre => Iter.Kind = Kind_Iterator;

   function To_Iterator
     (Value : Primitive; Pool : Primitive_Pool) return Primitive_Iter'Class;
   --  Create an iterator that yields the elements of a List or
   --  Iterator Primitive.
   --  Raise an Unsupported_Error if the value isn't a List or an Iterator

   procedure Free_Iterator_Primitive is
     new Ada.Unchecked_Deallocation
       (Iterator_Primitive, Iterator_Primitive_Access);

   -------------------
   -- Selector list --
   -------------------

   function To_List
     (Value : Selector_List; Pool : Primitive_Pool) return Primitive;
   --  Create a List Primitive value from a Selector_List Primitive value

   ---------------
   -- Accessors --
   ---------------

   function Kind (Value : Primitive) return Valid_Primitive_Kind;
   --  Return the kind of a primitive

   function Int_Val (Value : Primitive) return Adaptive_Integer;
   --  Return the value of an Int primitive

   function Str_Val (Value : Primitive) return Text_Type;
   --  Return the value of a Str primitive

   function Bool_Val (Value : Primitive) return Boolean;
   --  Return the value of a Bool primitive

   function Node_Val (Value : Primitive) return H.AST_Node_Holder;
   --  Return the value of a Node primitive

   function List_Val (Value : Primitive) return Primitive_List_Access;
   --  Return the value of a list primitive

   function Selector_List_Val (Value : Primitive) return Selector_List;
   --  Return the value of a selector list primitive

   function Iter_Val (Value : Primitive) return Iterator_Primitive_Access;
   --  Return the value of an iterator primitive

   function Elements
     (Value : Primitive) return not null Primitive_Vector_Access;
   --  Return a pointer to the elements of a list or iterable primitive. If the
   --  primitive is an iterator, consume it beforehand.

   function Data
     (Value       : Primitive;
      Member_Name : Text_Type;
      Pool        : Primitive_Pool) return Primitive;
   --  Return the value of 'Value's member named 'Member_Name'.
   --  This member can be either a built_in member (ex: length), or a
   --  Langkit field/property.
   --  Raise an Unsupported_Error if there is no member named
   --  'Member_Name'.

   function Is_Nullish (Value : Primitive) return Boolean;
   --  Return whether the value is null or unit.

   function Booleanize (Value : Primitive) return Boolean;
   --  Turn the value into an Ada boolean value, according to the logic:
   --
   --  * ``false``, the unit value and the null node are False
   --  * Everything else is True

   ----------------------------------
   -- Creation of Primitive values --
   ----------------------------------

   function Make_Unit_Primitive return Primitive;
   --  Create a Unit Primitive value

   function To_Primitive
     (Val : Integer; Pool : Primitive_Pool) return Primitive;
   --  Create a Primitive value from the Integer value

   function To_Primitive
     (Val : Adaptive_Integer; Pool : Primitive_Pool) return Primitive;
   --  Create a Primitive value from the Adaptive_Integer value

   function To_Primitive
     (Val : Text_Type; Pool : Primitive_Pool) return Primitive;
   --  Create a Primitive value from the String value

   function To_Primitive (Val : Boolean) return Primitive;
   --  Create a Bool primitive

   function To_Primitive
     (Node : H.AST_Node_Holder; Pool : Primitive_Pool) return Primitive;
   --  Create a Primitive value from the LKQL_Node value

   function To_Primitive
     (Token : H.AST_Token_Holder; Pool : Primitive_Pool) return Primitive;
   --  Create a primitive value from the AST_Token value

   function To_Primitive
     (Unit : H.AST_Unit_Holder; Pool : Primitive_Pool) return Primitive;
   --  Create a primitive value from the AST_Unit value

   function To_Primitive
     (Val : Primitive_Iter'Class; Pool : Primitive_Pool) return Primitive;

   function To_Primitive
     (Val : Selector_List; Pool : Primitive_Pool) return Primitive;
   --  Create a Primitive value from a Selector_List;

   function Make_Empty_List (Pool : Primitive_Pool) return Primitive;
   --  Return a primitive value storing an empty list.

   function Make_Empty_Object (Pool : Primitive_Pool) return Primitive;
   --  Return a primitive value storing an empty object.

   function Make_Empty_Tuple (Pool : Primitive_Pool) return Primitive;

   function Make_Namespace
     (N : Environment_Access;
      Module : L.LKQL_Node;
      Pool : Primitive_Pool) return Primitive;

   ---------------------
   -- Function values --
   ---------------------

   function Make_Builtin_Function
     (Fn : Builtin_Function; Pool : Primitive_Pool) return Primitive;

   function Make_Function
     (Node            : L.Base_Function;
      Env             : Environment_Access;
      Pool            : Primitive_Pool;
      With_Call_Cache : Boolean := False) return Primitive;

   function Make_Selector
     (Node            : L.Selector_Decl;
      Env             : Environment_Access;
      Pool            : Primitive_Pool;
      With_Call_Cache : Boolean := False) return Primitive;

   function Make_Property_Reference
     (Node_Val     : Primitive;
      Property_Ref : H.AST_Node_Member_Ref_Holder;
      Pool         : Primitive_Pool) return Primitive;

   function Profile (Obj : Primitive) return Text_Type;
   --  For a callable object, return its profile (name + arguments) as text.

   --------------------
   -- List Functions --
   --------------------

   procedure Append (List, Element : Primitive);
   --  Add `Element` to the end of `List`.
   --  An Unsupported_Error will be raised if `List` is not a value of kind
   --  Kind_List, or if the kind of `Element` doesn't match the kind of the
   --  values stored in `List`.

   procedure Extend (List, New_Value : Primitive);
   --  Push 'New_value' to the end of 'List'.
   --  If 'New_value' is a list or iterator, it's elements will be pushed
   --  in order.
   --  An Unsupported_Error will be raise if 'List' is not a value of kind
   --  Kind_List.

   function Contains
     (List, Value : Primitive) return Boolean;
   --  Check whether `List` contains `Value`.
   --  An Unsupported_Error will be raised if `List` is not a value of kind
   --  Kind_List, or if the kind of `Value` doesn't match the kind of the
   --  values stored in `List`.

   function Get
     (List : Primitive; Index : Integer;
      Raise_If_OOB : Boolean := True) return Primitive;
   --  Return the element of 'List' at 'Index'.
   --  Raise an Unsupported_Error exception if 'Index' is out of bounds.

   function Get
     (List : Primitive_List_Access; Index : Integer;
      Raise_If_OOB : Boolean := True) return Primitive;
   --  Return the element of 'List' at 'Index'.
   --  Raise an Unsupported_Error exception if 'Index' is out of bounds.

   function Length (List : Primitive) return Natural;
   --  Return the length of the list.
   --  An Unsupported_Error will be raised if List is not a value of kind
   --  Kind_List.

   ----------------------
   -- Object functions --
   ----------------------

   function Extract_Value
     (Obj           : Primitive;
      Key           : Text_Type;
      Ctx           : LKQL.Eval_Contexts.Eval_Context;
      Expected_Kind : Base_Primitive_Kind := No_Kind;
      Location      : L.LKQL_Node := L.No_LKQL_Node) return Primitive;
   --  Extract a value from a primitive object, with the given key, doing the
   --  appropriate checks along the way.

   ------------------------------
   -- Text conversion & output --
   ------------------------------

   function To_Unbounded_Text (Val : Primitive) return Unbounded_Text_Type;
   --  Return a unicode String representation of `Val`

   function To_String (Val : Primitive) return String;
   --  Return a string representation of ``Val``

   function To_String (Val : Valid_Primitive_Kind) return String;
   --  Return a String representation of `Val`

   function Kind_Name (Value : Primitive) return String;
   --  Return a String representing the kind of `Value`

   procedure Display
     (Value         : Primitive;
      New_Line      : Boolean);
   --  Print a Primitive value onto the console.

   ---------------
   -- Operators --
   ---------------

   function Equals
     (Left, Right : Primitive) return Primitive;
   --  Perform a deep equality check between 'Left' and 'Right'.
   --  An Unsupported exception will be raised if Left and Right have different
   --  kinds.

   function Deep_Equals
     (Left, Right : Primitive_List_Access) return Boolean;
   --  Perform a deep equality check between two primitive_List_Access values.
   --  An Unsupported exception will be raised if Left and Right have different
   --  kinds.

   function Concat
     (Left, Right : Primitive;
      Pool : Primitive_Pool) return Primitive;
   --  Concatenate a Primitive value to a Str Primitive.
   --  The supported operations are: Str & Int, Str & Str, Str & Bool.
   --  Unsupported operations will raise an Unsupported_Error exception.

   function Lt
     (Left, Right : Primitive) return Primitive;
   --  Tests that 'Left' is strictly lower than 'Right'.
   --  The supported operations are: Int < Int, String < String.
   --  Unsupported operations will raise an Unsupported_Error exception.

   function Lte
     (Left, Right : Primitive) return Primitive;
   --  Tests that 'Left' is equal to, or lower than than 'Right'.
   --  The supported operations are: Int <= Int, String <= String.
   --  Unsupported operations will raise an Unsupported_Error exception.

   function Gt
     (Left, Right : Primitive) return Primitive;
   --  Tests that 'Left' is strictly higher than 'Right'.
   --  The supported operations are: Int > Int, String > String.
   --  Unsupported operations will raise an Unsupported_Error exception.

   function Gte
     (Left, Right : Primitive) return Primitive;
   --  Tests that 'Left' is equal to, or higher than than 'Right'.
   --  The supported operations are: Int >= Int, String >= String.
   --  Unsupported operations will raise an Unsupported_Error exception.

   procedure Check_Kind
     (Expected_Kind : Valid_Primitive_Kind; Value : Primitive);
   --  Raise an Unsupporter_Error exception if Value.Kind is different than
   --  Expected_Kind.

end LKQL.Primitives;

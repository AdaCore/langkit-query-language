with Langkit_Support.Text; use Langkit_Support.Text;

with Ada.Containers.Vectors;
with Ada.Unchecked_Deallocation;

with GNATCOLL.Refcount; use GNATCOLL.Refcount;

package Interpreter.Primitives is

   type Primitive_List;
   --  List of Primitive Values

   type Primitive_List_Access is access Primitive_List;
   --  Pointer to a list of Primitive values

   type Primitive_Kind is
     (Kind_Unit,
      --  Unit value: representation of the result of a computation that
      --  doesn't produce a meaningful result.

      Kind_Int,
      --  Integer value, encoded as an Ada Integer

      Kind_Str,
      --  Unicode String value

      Kind_Bool,
      --  Either 'true' or 'false'

      Kind_Node,
      --  Libadalang node

      Kind_List
      --  List of libadalang nodes
   );
   --  Denotes the kind of a primitive value.

   type Primitive_Data (Kind : Primitive_Kind) is
     new Refcounted with record
      case Kind is
         when Kind_Unit =>
            null;
         when Kind_Int =>
            Int_Val : Integer;
         when Kind_Str =>
            Str_Val : Unbounded_Text_Type;
         when Kind_Bool =>
            Bool_Val : Boolean;
         when Kind_Node =>
            Node_Val : LAL.Ada_Node;
         when Kind_List =>
            List_Val : Primitive_List_Access;
      end case;
   end record;
   --  Store a primitive value, which can be an atomic type
   --  (Bool, Int, ...), an AST node, or a list of Primitive values.

   procedure Release (Data : in out Primitive_Data);
   --  Release if data is of Kind Kind_List, free the list's memory

   package Primitive_Ptrs is
     new GNATCOLL.Refcount.Shared_Pointers
       (Element_Type => Primitive_Data,
        Release      => Release);
   use Primitive_Ptrs;

   subtype Primitive is Primitive_Ptrs.Ref;

   package Primitive_Vectors is new
     Ada.Containers.Vectors (Index_Type   => Positive,
                             Element_Type => Primitive);

   type Primitive_Vector_Access is access all Primitive_Vectors.Vector;

   type Primitive_List is record
      Elements_Kind : Primitive_Kind;
      --  Kind of the elemnts stored in the list
      Elements      : aliased Primitive_Vectors.Vector;
      --  Vector that holds the actual elemnts
   end record;
   --  List of primitive values.

   procedure Free_Primitive_List is
     new Ada.Unchecked_Deallocation (Primitive_List, Primitive_List_Access);

   Unsupported_Error : exception;

   function Property
     (Value : Primitive; Property_Name : Text_Type) return Primitive;
   --  Return the value of the property named 'Property_Name' of the given
   --  Primitive value.
   --  Raise an Unsupported_Error if there is no property named
   --  'Property_Name'.

   ---------------
   -- Accessors --
   ---------------

   function Kind (Value : Primitive) return Primitive_Kind;
   --  Return the kind of a primitive

   function Int_Val (Value : Primitive) return Integer;
   --  Return the value of an Int primitive

   function Str_Val (Value : Primitive) return Unbounded_Text_Type;
   --  Return the value of a Str primitive

   function Bool_Val (Value : Primitive) return Boolean;
   --  Return the value of a Bool primitive

   function Node_Val (Value : Primitive) return LAL.Ada_Node;
   --  Return the value of a Node primitive

   function List_Val (Value : Primitive) return Primitive_List_Access;
   --  Return the value of a list primitive

   function Elements_Kind (Value : Primitive) return Primitive_Kind;
   --  Return the kind of the elements of a list

   function Elements
     (Value : Primitive) return not null Primitive_Vector_Access;
   --  Return a pointer to the elements of a list primitive

   ----------------------------------
   -- Creation of Primitive values --
   ----------------------------------

   function Make_Unit_Primitive return Primitive_Ptrs.Ref;
   --  Create a Unit Primitive value

   function To_Primitive (Val : Integer) return Primitive;
   --  Create a Primitive value from the Integer value

   function To_Primitive (Val : Unbounded_Text_Type) return Primitive;
   --  Create a Primitive value from the String value

   function To_Primitive (Val : Boolean) return Primitive;
   --  Create a Bool primitive

   function To_Primitive (Val : LAL.Ada_Node) return Primitive;
   --  Create a Primitive value from the LKQL_Node value

   function Make_Empty_List (Kind : Primitive_Kind) return Primitive;
   --  Return a Primitive value storing an empty list of Primitive values
   --  of kind `Kind`.

   --------------------
   -- List Functions --
   --------------------

   procedure Append (List, Element : Primitive);
   --  Add `Element` to the end of `List`.
   --  An Unsupported_Error will be raised if `List` is not a value of kind
   --  Kind_List, or if the kind of `Element` doesn't match the kind of the
   --  values stored in `List`.

   function Contains (List, Value : Primitive) return Boolean;
   --  Check whether `List` contains `Value`.
   --  An Unsupported_Error will be raised if `List` is not a value of kind
   --  Kind_List, or if the kind of `Value` doesn't match the kind of the
   --  values stored in `List`.

   function Get (List : Primitive; Index : Integer) return Primitive;
   --  Return the element of 'List' at 'Index'.
   --  Raise an Unsupported_Error exception if 'Index' is out of bounds.

   function Length (List : Primitive) return Natural;
   --  Return the length of the list.
   --  An Unsupported_Error will be raised if List is not a value of kind
   --  Kind_List.

   ------------------------------
   -- Text conversion & output --
   ------------------------------

   function To_Unbounded_Text (Val : Primitive) return Unbounded_Text_Type;
   --  Return a unicode String representation of `Val`

   function To_String (Val : Primitive_Kind) return String;
   --  Return a String representation of `Val`

   function Kind_Name (Value : Primitive) return String;
   --  Return a String representing the kind of `Value`

   procedure Display (Value : Primitive);
   --  Print a Primitive value onto the console

   ---------------
   -- Operators --
   ---------------

   function "+" (Left, Right : Primitive) return Primitive;
   --  Add two Primitive values together.
   --  The only supported operation is Int + Int.
   --  Unsupported operations will raise an Unsupported_Error exception.

   function "-" (Left, Right : Primitive) return Primitive;
   --  Substract two Primitive values.
   --  The only supported operation is Int - Int.
   --  Unsupported operations will raise an Unsupported_Error exception.

   function "*" (Left, Right : Primitive) return Primitive;
   --  Multiply two Primitive values.
   --  The only supported operation is Int * Int.
   --  Unsupported operations will raise an Unsupported_Error exception.

   function "/" (Left, Right : Primitive) return Primitive;
   --  Divide two Primitive values.
   --  The only supported operation is Int / Int, with a non-zero denominator.
   --  Unsupported operations will raise an Unsupported_Error exception.

   function "=" (Left, Right : Primitive) return Primitive;
   --  Perform a deep equality check between 'Left' and 'Right'.
   --  An Unsupported exception will be raised if Left and Right have different
   --  kinds.

   function "/=" (Left, Right : Primitive) return Primitive;
   --  Test inequality between two Primitive values.
   --  An Unsupported exception will be raised if Left and Right have different
   --  kinds.

   function "&" (Left, Right : Primitive) return Primitive;
   --  Concatenate a Primitive value to a Str Primitive.
   --  The supported operations are: Str & Int, Str & Str, Str & Bool.
   --  Unsupported operations will raise an Unsupported_Error exception.

end Interpreter.Primitives;

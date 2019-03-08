with Langkit_Support.Text; use Langkit_Support.Text;

with Ada.Containers.Vectors;

package Interpreter.Primitives is

   type Primitive_List;

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

   type Primitive (Kind : Primitive_Kind := Kind_Unit) is record
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
            List_Val : access Primitive_List;
      end case;
   end record;
   --  Store a primitive value, which can be an atomic type
   --  (Bool, Int, ...), an AST node, or a list of Primitive values.

   package Primitive_Vectors is new
     Ada.Containers.Vectors (Index_Type   => Positive,
                             Element_Type => Primitive);

   type Primitive_List is record
      Elements_Kind : Primitive_Kind;
      Elements      : Primitive_Vectors.Vector;
   end record;

   Unit_Constant : constant Primitive := (Kind => Kind_Unit);
   --  Unit constant, used to materialize the absence of meaningful value

   Unsupported_Error : exception;

   ----------------------------------
   -- Creation of Primitive values --
   ----------------------------------

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
   --
   --  The supported operations are: Int + Int, String + Int,
   --  String + String and String + Bool.
   --
   --  Unsupported operations will rase an Unsupported_Error exception.

   function "-" (Left, Right : Primitive) return Primitive;
   --  Subract two Primitive values.
   --  The only supported operation is Int - Int.
   --  Unsupported operations will rase an Unsupported_Error exception.

   function "*" (Left, Right : Primitive) return Primitive;
   --  Multiply two Primitive values.
   --  The only supported operation is Int * Int.
   --  Unsupported operations will rase an Unsupported_Error exception.

   function "/" (Left, Right : Primitive) return Primitive;
   --  Divide two Primitive values.
   --  The only supported operation is Int / Int, with a non-zero denominator.
   --  Unsupported operations will rase an Unsupported_Error exception.

   function "=" (Left, Right : Primitive) return Primitive;
   --  Test equality between two Primitive values.
   --  An Unsupported exception will be raised if Left and Right have different
   --  kinds.

   function "/=" (Left, Right : Primitive) return Primitive;
   --  Test inequality between two Primitive values.
   --  An Unsupported exception will be raised if Left and Right have different
   --  kinds.

private

   function "+" (Left : Integer; Right : Primitive) return Primitive;
   function "+"
     (Left : Unbounded_Text_Type; Right : Primitive) return Primitive;

end Interpreter.Primitives;

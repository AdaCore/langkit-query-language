with Langkit_Support.Text; use Langkit_Support.Text;

package Interpreter.Types.Atoms is

   Unsupported_Error : exception;

   type Atom_Kind is
     (Kind_Unit,
      --  Unit value: representation of the result of a computation that
      --  doesn't produce a meanigfull result.

      Kind_Int,
      --  Integer value, encoded as an Ada Integer.

      Kind_Str,
      --  Unicode String value.

      Kind_Bool
      --  Either 'true' or 'false'
     );
   --  Denotes the kind of an Atom value.

   type Atom (Kind : Atom_Kind := Kind_Unit) is record
      case Kind is
         when Kind_Unit =>
            null;
         when Kind_Int =>
            Int_Val : Integer;
         when Kind_Str =>
            Str_Val : Unbounded_Text_Type;
         when Kind_Bool =>
            Bool_Val : Boolean;
      end case;
   end record;
   --  Store an atomic value.

   ------------------------------
   -- Text conversion & output --
   ------------------------------

   function To_String (Kind : Atom_Kind) return String;
   --  Return a String representation of an Atom_Kind.

   function To_String (Value : Atom) return Unbounded_Text_Type;
   --  Return a Unicode String representation of an Atom.

   procedure Display (Value : Atom);
   --  Print an Atom value onto the console.

   ---------------
   -- Operators --
   ---------------

   function "+" (Left, Right : Atom) return Atom;
   --  Add two Atom values together.
   --
   --  The supported operations are: Int + Int, String + Int,
   --  String + String and String + Bool.
   --
   --  Unsupported operations will rase an Unsupported exception.

   function "and" (Left, Right : Atom) return Atom;
   --  Compute the logical 'and' between two Atom values.
   --  Both values must be Boolean values.
   --  Unsupported operations will raise an Unsupported exception.

   function "or" (Left, Right : Atom) return Atom;
   --  Compute the logical 'or' between two Atom values.
   --  Both values must be Boolean values.
   --  Unsupported operations will raise an Unsupported exception.

   function "=" (Left, Right : Atom) return Atom;
   --  Test equality between two Atom values.
   --  An Unsupported exception will be raised if Left and Right have different
   --  kinds.

private

   function "+" (Left : Integer; Right : Atom) return Atom;
   function "+" (Left : Unbounded_Text_Type; Right : Atom) return Atom;

   procedure Check_Both_Bool (Left, Right : Atom);

end Interpreter.Types.Atoms;

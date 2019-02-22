with Langkit_Support.Text; use Langkit_Support.Text;

package Interpreter.Types.Atoms is

   Unsupported : exception;

   type Atom_Kind is (Kind_Unit, Kind_Number, Kind_Int, Kind_Str, Kind_Bool);

   function To_String (Kind : Atom_Kind) return String;

   type Atom (Kind : Atom_Kind := Kind_Unit) is record
      case Kind is
         when Kind_Unit =>
            null;
         when Kind_Number =>
            Number_Val : Float;
         when Kind_Int =>
            Int_Val : Integer;
         when Kind_Str =>
            Str_Val : Unbounded_Text_Type;
         when Kind_Bool =>
            Bool_Val : Boolean;
      end case;
   end record;

   type Atom_Access is access Atom;

   procedure Display (Value : Atom);

   function To_String (Value : Atom) return Unbounded_Text_Type;

   function "+" (Left, Right : Atom) return Atom;
   function "and" (Left, Right : Atom) return Atom;
   function "or" (Left, Right : Atom) return Atom;
   function "=" (Left, Right : Atom) return Atom;

private

   function "+" (Left : Float; Right : Atom) return Atom;
   function "+" (Left : Integer; Right : Atom) return Atom;
   function "+" (Left : Unbounded_Text_Type; Right : Atom) return Atom;

   procedure Check_Both_Bool (Left, Right : Atom);

   function Format_Float (N : Float) return Text_Type;

end Interpreter.Types.Atoms;

with Langkit_Support.Text; use Langkit_Support.Text;

package Interpreter.Expr is
   
   Unsupported: exception;
   
   type Atom_Kind is ( Unit,
                        Number,
                        Int,
                        Str,
                        Bool);
   
   function To_String (Kind: Atom_Kind) return String;
   
   type Atom(Kind: Atom_Kind) is record
      case Kind is
         when Unit => null;
         when Number =>
            Number_Val: Float;
         when Int =>
            Int_Val: Integer;
         when Str =>
            Str_Val: Unbounded_Text_Type;
         when Bool =>
            Bool_Val: Boolean;
      end case;
   end record;
   
   procedure Display (Value: in Atom);
   
   function To_String (Value: in Atom) return Unbounded_Text_Type;
   
   function "+" (Left, Right: Atom) return Atom;
   function "and" (Left, Right: Atom) return Atom;
   function "or" (Left, Right: Atom) return Atom;
   function "=" (Left, Right: Atom) return Atom;
   
private
   
   function "+" (Left: Float; Right: Atom) return Atom;
   function "+" (Left: Integer; Right: Atom) return Atom;
   function "+" (Left: Unbounded_Text_Type; Right: Atom) return Atom;
   
   procedure Check_Both_Bool (Left, Right: Atom);
   
end Interpreter.Expr;

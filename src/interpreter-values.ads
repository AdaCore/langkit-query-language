with Langkit_Support.Text; use Langkit_Support.Text;

package Interpreter.Values is
   
   Unsupported: exception;
   
   type Value_Kind is ( Unit,
                        Number,
                        Int,
                        Str);
   
   function To_String (Kind: Value_Kind) return String;
   
   type ExprVal(Kind: Value_Kind) is record
      case Kind is
         when Unit => null;
         when Number =>
            Number_Val: Float;
         when Int =>
            Int_Val: Integer;
         when Str =>
            Str_Val: Unbounded_Text_Type;
      end case;
   end record;
   
   procedure Display (Value: in ExprVal);
   
   function To_String (Value: in ExprVal) return Unbounded_Text_Type;
   
   function "+" (Left, Right: ExprVal) return ExprVal;
   
private
   
   function "+" (Left: Float; Right: ExprVal) return ExprVal;
   function "+" (Left: Integer; Right: ExprVal) return ExprVal;
   function "+" (Left: Unbounded_Text_Type; Right: ExprVal) return ExprVal;
   
end Interpreter.Values;

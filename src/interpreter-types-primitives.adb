package body Interpreter.Types.Primitives is

   function To_Primitive (A: in Atom) return Primitive is
   begin
      return (Kind => Kind_Atom, Atom_Val => A);
   end To_Primitive;

   procedure Display (Value: Primitive) is
   begin
      case Value.Kind is
         when Kind_Atom => Display (Value.Atom_Val);
         when Kind_NodeList => Display (Value.NodeList_Val);
      end case;
   end Display;

end Interpreter.Types.Primitives;

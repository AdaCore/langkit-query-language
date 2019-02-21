with Interpreter.Types.Atoms; use Interpreter.Types.Atoms;
with Interpreter.Types.NodeLists; use Interpreter.Types.NodeLists;

package Interpreter.Types.Primitives is  
   
   type Primitive_Kind is (Kind_Atom,
                           Kind_NodeList);
   
   type Primitive(Kind: Primitive_Kind := Kind_Atom) is record
      case Kind is
         when Kind_Atom => Atom_Val: Atom;
         when Kind_NodeList => NodeList_Val: NodeList;
      end case;
   end record;
   
   procedure Display (Value: Primitive);
   
   function To_Primitive (A: in Atom) return Primitive;

end Interpreter.Types.Primitives;

with Ada.Wide_Wide_Text_IO; use Ada.Wide_Wide_Text_IO;

package body Interpreter.Types.Primitives is

   -------------
   -- Display --
   -------------

   procedure Display (Value : Primitive) is
   begin
      case Value.Kind is
         when Kind_Atom =>
            Display (Value.Atom_Val);
         when Kind_Node =>
            Put_Line (Value.Node_Val.Text);
         when Kind_Node_List =>
            Display (Value.Node_List_Val);
      end case;
   end Display;

   ---------------
   -- Kind_Name --
   ---------------

   function Kind_Name (Value : Primitive) return String is
   begin
      case Value.Kind is
         when Kind_Atom => return To_String (Value.Atom_Val.Kind);
         when Kind_Node_List => return "Node_List";
         when Kind_Node => return LAL.Kind_Name (Value.Node_Val);
      end case;
   end Kind_Name;

   ------------------
   -- To_Primitive --
   ------------------

   function To_Primitive (A : Atom) return Primitive is
   begin
      return (Kind => Kind_Atom, Atom_Val => A);
   end To_Primitive;

   ------------------
   -- To_Primitive --
   ------------------

   function To_Primitive (N : LAL.Ada_Node) return Primitive is
   begin
      return (Kind => Kind_Node, Node_Val => N);
   end To_Primitive;

   ------------------
   -- To_Primitive --
   ------------------

   function To_Primitive (B : Boolean) return Primitive is
      Atom_Bool : constant Atom := (Kind => Kind_Bool, Bool_Val => B);
   begin
      return (Kind => Kind_Atom, Atom_Val => Atom_Bool);
   end To_Primitive;

   ------------------
   -- To_Primitive --
   ------------------

   function To_Primitive (I : Integer) return Primitive is
      Atom_Int : constant Atom := (Kind => Kind_Int, Int_Val => I);
   begin
      return (Kind => Kind_Atom, Atom_Val => Atom_Int);
   end To_Primitive;

   ------------------
   -- To_Primitive --
   ------------------

   function To_Primitive (Val : Unbounded_Text_Type) return Primitive is
      Atom_String : constant Atom := (Kind => Kind_Str, Str_Val => Val);
   begin
      return (Kind => Kind_Atom, Atom_Val => Atom_String);
   end To_Primitive;

end Interpreter.Types.Primitives;

with Interpreter.Errors; use Interpreter.Errors;
with Interpreter.Types.Primitives; use Interpreter.Types.Primitives;

with Langkit_Support.Text; use Langkit_Support.Text;

with Ada.Containers.Hashed_Maps;
with Ada.Strings.Wide_Wide_Unbounded.Wide_Wide_Hash;
with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;

package Interpreter.Eval_Contexts is
   package String_Value_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Unbounded_Text_Type,
      Element_Type    => Primitive,
      Hash            => Ada.Strings.Wide_Wide_Unbounded.Wide_Wide_Hash,
      Equivalent_Keys => "=");

   type Eval_Context is record
      Env      : String_Value_Maps.Map;
      --  Store the value associated with variable names.

      AST_Root : LAL.Ada_Node := LAL.No_Ada_Node;
      --  Root node of the tree in wich node queries will run.

      Last_Error : Error_Data := (Kind => Kind_Empty_Error);
      --  Store data about the last error, if any.
   end record;
   --  Store the evaluation context.

   procedure Add_Error (Ctx : in out Eval_Context; Error : Error_Data);
   --  Add the given error to the evaluation context.

end Interpreter.Eval_Contexts;

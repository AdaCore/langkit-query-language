with Interpreter.Errors;     use Interpreter.Errors;
with Interpreter.Primitives; use Interpreter.Primitives;
use  Interpreter.Primitives.Primitive_Ptrs;

with Langkit_Support.Text; use Langkit_Support.Text;

with Ada.Containers.Hashed_Maps;
with Ada.Unchecked_Deallocation;
with Ada.Strings.Wide_Wide_Unbounded.Wide_Wide_Hash;
with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;

package Interpreter.Eval_Contexts is
   package String_Value_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Unbounded_Text_Type,
      Element_Type    => Primitive,
      Hash            => Ada.Strings.Wide_Wide_Unbounded.Wide_Wide_Hash,
      Equivalent_Keys => "=");
   use String_Value_Maps;

   subtype Environment is String_Value_Maps.Map;

   function Backup_Env (Parent_Env : Environment;
                        Local_Env  : Environment)
                        return Environment;
   --  Return the key-value pairs from Parent_Env which have a key that belongs
   --  to the key set of Local_Env.

   procedure Update_Env (Env        : in out Environment;
                         New_Values : Environment);
   --  Insert all the key-value pairs from New_Values into Env. In case of
   --  a conflict, the value from Env will be overriden.

   type Eval_Context_Value is record
      Env : String_Value_Maps.Map;
      --  Store the value associated with variable names.

      AST_Root : LAL.Ada_Node := LAL.No_Ada_Node;
      --  Root node of the tree in wich node queries will run.

      Last_Error : Error_Data := Make_Empty_Error;
      --  Store data about the last error, if any.

      Error_Recovery_Enabled : Boolean := False;
      --  If true, the user will be asked if he wants to resume execution uppon
      --  encountering an error.
   end record;
   --  Store the evaluation context.

   type Eval_Context is access all Eval_Context_Value;
   --  Pointer to an Eval_Context

   procedure Free_Eval_Context is new Ada.Unchecked_Deallocation
     (Eval_Context_Value, Eval_Context);

   procedure Add_Error (Ctx : in out Eval_Context_Value; Error : Error_Data);
   --  Add the given error to the evaluation context.

end Interpreter.Eval_Contexts;

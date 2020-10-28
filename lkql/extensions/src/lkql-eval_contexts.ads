with LKQL.Errors;     use LKQL.Errors;
with LKQL.AST_Nodes;  use LKQL.AST_Nodes;
with LKQL.Primitives; use LKQL.Primitives;
use  LKQL.Primitives.Primitive_Ptrs;

with Langkit_Support.Text; use Langkit_Support.Text;

with Ada.Containers.Hashed_Maps;
with Ada.Unchecked_Deallocation;
with Ada.Strings.Wide_Wide_Unbounded.Wide_Wide_Hash;
with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;

package LKQL.Eval_Contexts is
   package String_Value_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Unbounded_Text_Type,
      Element_Type    => Primitive,
      Hash            => Ada.Strings.Wide_Wide_Unbounded.Wide_Wide_Hash,
      Equivalent_Keys => "=");
   use String_Value_Maps;

   subtype Environment_Map is String_Value_Maps.Map;

   procedure Add_Bindings
     (Env : in out Environment_Map; New_Bindings : Environment_Map);
   --  Add the key-value pairs from 'New_Bindings' to 'Env'

   procedure Merge_Into
     (Env : in out Environment_Map; Other : Environment_Map);
   --  Merge 'Other' into 'Env'. All the key-value pairs from 'Other' will be
   --  added to 'Env'. In case of key conflict, the values will be stored
   --  into a list Primitive.

   type Global_Data is private;

   type Global_Data_Access is access all Global_Data;

   type Environment is private;

   type Environment_Access is access all Environment;

   ------------------
   -- Eval_Context --
   ------------------

   type Eval_Context is tagged record
      Kernel : Global_Data_Access;
      --  Global structured shared by every Eval_Context instance

      Frames : Environment_Access;
      --  Chain of environments from the local frame to the global env
   end record;
   --  Store the evaluation context.

   procedure Add_Error (Ctx : Eval_Context; Error : Error_Data);
   --  Add the given error to the evaluation context.

   procedure Release_Current_Frame (Ctx : in out Eval_Context);
   --  Free the memory allocated for the local frame.

   function Last_Error (Ctx : Eval_Context) return Error_Data;
   --  Return the value of the last registered error

   function Exists_In_Local_Env (Ctx : Eval_Context;
                                 Key : Text_Type) return Boolean;
   --  Return wether the given name is associated to a value in the local
   --  environment.

   function Exists_In_Local_Env (Ctx : Eval_Context;
                                 Key : Unbounded_Text_Type) return Boolean;
   --  Return wether the given name is associated to a value in the local
   --  environment.

   function Null_Node (Ctx : Eval_Context) return AST_Node_Rc;
   --  Return the node produced by a "null" litteral

   function Error_Recovery_Enabled (Ctx : Eval_Context) return Boolean;
   --  Return wether the error recovery mecanism is enabled

   function AST_Root (Ctx : Eval_Context) return AST_Node_Rc;
   --  Return the evaluation context's AST root

   function Clone_Frame (Ctx : Eval_Context) return Eval_Context;
   --  Make a deep copy of the current frame

   function Create_New_Frame (Ctx            : Eval_Context;
                              Local_Bindings : Environment_Map := Empty_Map)
                              return Eval_Context;
   --  Create a new evaluation context with the current environment as parent
   --  environment.
   --  If the bindings from 'Local_Bindings' will be added to the local
   --  environment.

   function Lookup (Ctx : Eval_Context;
                    Key : Unbounded_Text_Type) return String_Value_Maps.Cursor;
   --  Return a cursor to the element associated with the given key in the
   --  evaluation context's frames.

   procedure Add_Binding (Ctx   : Eval_Context;
                          Key   : Text_Type;
                          Value : Primitive);
   --  Associate 'Value' to the given key in the local frame.

   function Is_Root_Context (Ctx : Eval_Context) return Boolean;
   --  Return whether the current context is the root context. I.e, it's
   --  environment doesn't have a parent environment.

   function Parent_Context (Ctx : Eval_Context) return Eval_Context
     with Pre => not Ctx.Is_Root_Context;
   --  Return the parent of the current local context.
   --  An Assertion_Error will be raised is 'Ctx' is the root context.

   function Make_Eval_Context (Ast_Root     : AST_Node_Rc;
                               Null_Node    : AST_Node_Rc;
                               Err_Recovery : Boolean := False)
                               return Eval_Context;
   --  Create a new Eval_Context with the given Ast_Root and error recovery
   --  flag.

   procedure Free_Eval_Context (Ctx : in out Eval_Context);
   --  Release the memory allocated for the evaluation context.
   --  Raise an assertion error if Ctx is not the root context.
   --  Use Release_Local_Frame to release the memory allocated for a local
   --  environment.

private

   -----------------
   -- Global_Data --
   -----------------

   type Global_Data is record
      Ast_Root : AST_Node_Rc;
      --  Root node of the tree in wich node queries will run.

      Null_Node : AST_Node_Rc;
      --  Value produced by a "null" litteral

      Last_Error : Error_Data := Make_Empty_Error;
      --  Store data about the last error, if any.

      Error_Recovery_Enabled : Boolean := False;
      --  If true, the user will be asked if he wants to resume execution uppon
      --  encountering an error.
   end record;
   --  Stores the global data structures shared by every evaluation context

   procedure Free_Global_Data is new Ada.Unchecked_Deallocation
     (Global_Data, Global_Data_Access);

   -----------------
   -- Environment --
   -----------------

   type Environment is record
      Local_Bindings : Environment_Map;
      --  Map containing the local

      Parent : Environment_Access;
      --  Parent environment
      --  If this environment is non-null, it will be used as a fallback uppon
      --  lookup failures.
   end record;
   --  Chainable map for symbol lookups

   function Lookup (Env : Environment;
                    Key : Unbounded_Text_Type) return String_Value_Maps.Cursor;
   --  Lookup the given key in the local environment.
   --  If the local environment doesn't contain the given key, the lookup will
   --  be attempted on the parent env, if any.

   procedure Free_Environment is new Ada.Unchecked_Deallocation
     (Environment, Environment_Access);

   function Make_Empty_Environment
     (Parent : Environment_Access := null) return Environment;
   --  Return an empty map from Unbounded_Text_Type to Primitive values

end LKQL.Eval_Contexts;

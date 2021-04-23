------------------------------------------------------------------------------
--                                                                          --
--                                   LKQL                                   --
--                                                                          --
--                     Copyright (C) 2019-2021, AdaCore                     --
--                                                                          --
-- LKQL is free software;  you can redistribute it and/or modify  it        --
-- under terms of the GNU General Public License  as published by the Free  --
-- Software Foundation;  either version 3,  or (at your option)  any later  --
-- version.   This  software  is distributed in the hope that it  will  be  --
-- useful but  WITHOUT ANY WARRANTY;  without even the implied warranty of  --
-- MERCHANTABILITY  or  FITNESS  FOR  A PARTICULAR PURPOSE.                 --
--                                                                          --
-- As a special  exception  under  Section 7  of  GPL  version 3,  you are  --
-- granted additional  permissions described in the  GCC  Runtime  Library  --
-- Exception, version 3.1, as published by the Free Software Foundation.    --
--                                                                          --
-- You should have received a copy of the GNU General Public License and a  --
-- copy of the GCC Runtime Library Exception along with this program;  see  --
-- the files COPYING3 and COPYING.RUNTIME respectively.  If not, see        --
-- <http://www.gnu.org/licenses/>.                                          --
------------------------------------------------------------------------------

with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with LKQL.Errors;     use LKQL.Errors;
with LKQL.AST_Nodes;  use LKQL.AST_Nodes;
with LKQL.Primitives; use LKQL.Primitives;
use  LKQL.Primitives.Primitive_Ptrs;

with Langkit_Support.Text; use Langkit_Support.Text;

with Ada.Containers.Hashed_Maps;
with Ada.Unchecked_Deallocation;

package LKQL.Eval_Contexts is

   package String_Value_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Symbol_Type,
      Element_Type    => Primitive,
      Hash            => Hash,
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

   function Get_Context (Self : Global_Data) return L.Analysis_Context;

   type Environment is private;

   function Lookup (Env : Environment;
                    Key : Symbol_Type) return String_Value_Maps.Cursor;
   --  Lookup the given key in the local environment.
   --  If the local environment doesn't contain the given key, the lookup will
   --  be attempted on the parent env, if any.

   type Environment_Access is access all Environment;
   pragma No_Heap_Finalization (Environment_Access);
   --  U315-023: This is needed because it works around obscure library level
   --  finalization issues that cause double free/use after free issues. At
   --  some point, we need to investigate those issues a bit further.

   procedure Inc_Ref (Self : Environment_Access);
   procedure Dec_Ref (Self : in out Environment_Access);
   function Env_Map_Image (Self : Environment_Map) return String;

   function Get_Env_Map (Self : Environment_Access) return Environment_Map;
   --  Get the env map for this env.

   function Get_Parent (Self : Environment_Access) return Environment_Access;
   --  Get the parent env for this env.

   function Env_Image (Env : Environment_Access) return String;
   --  Return a structured debug image of the env passed in parameter.

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
                                 Key : Symbol_Type) return Boolean;
   --  Return whether the given name is associated to a value in the local
   --  environment.

   function Null_Node (Ctx : Eval_Context) return AST_Node_Rc;
   --  Return the node produced by a "null" literal

   function Error_Recovery_Enabled (Ctx : Eval_Context) return Boolean;
   --  Return whether the error recovery mechanism is enabled

   function AST_Roots (Ctx : Eval_Context) return AST_Node_Array_Access;
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
                    Key : Symbol_Type) return String_Value_Maps.Cursor;
   --  Return a cursor to the element associated with the given key in the
   --  evaluation context's frames.

   procedure Add_Binding (Ctx   : Eval_Context;
                          Key   : Symbol_Type;
                          Value : Primitive);
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

   function Make_Eval_Context
     (Ast_Roots    : AST_Node_Array;
      Null_Node    : AST_Node_Rc;
      Analysis_Ctx : L.Analysis_Context := L.No_Analysis_Context;
      Err_Recovery : Boolean := False) return Eval_Context;
   --  Create a new Eval_Context with the given Ast_Root and error recovery
   --  flag. If passed an analysis context, use this instead of creating one.

   procedure Free_Eval_Context (Ctx : in out Eval_Context);
   --  Release the memory allocated for the evaluation context.
   --  Raise an assertion error if Ctx is not the root context.
   --  Use Release_Local_Frame to release the memory allocated for a local
   --  environment.

   function Get_LKQL_Unit
     (Ctx          : Eval_Context;
      Package_Name : String;
      From         : L.Analysis_Unit := L.No_Analysis_Unit)
      return L.Analysis_Unit;
   --  Get a LKQL unit, searching on the context's LKQL_PATH

   procedure Add_LKQL_Path (Ctx : in out Eval_Context; Path : String);
   --  Add a path to the LKQL_PATH

private

   package String_Vectors
   is new Ada.Containers.Vectors (Positive, Unbounded_String);

   -----------------
   -- Global_Data --
   -----------------

   type Global_Data is record
      Ast_Roots : AST_Node_Array_Access;
      --  Root node for each libadalang analysis unit that will be analysed in
      --  the context.

      Null_Node : AST_Node_Rc;
      --  Value produced by a "null" literal

      Last_Error : Error_Data := Make_Empty_Error;
      --  Store data about the last error, if any.

      Error_Recovery_Enabled : Boolean := False;
      --  If true, the user will be asked if he wants to resume execution upon
      --  encountering an error.

      Context : L.Analysis_Context;
      --  LKQL analysis context, used to hold data of the prelude

      LKQL_Path_List : String_Vectors.Vector;
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
      --  If this environment is non-null, it will be used as a fallback upon
      --  lookup failures.

      Ref_Count : Natural := 1;
   end record;
   --  Chainable map for symbol lookups

   function Make_Empty_Environment
     (Parent : Environment_Access := null) return Environment;
   --  Return an empty map from Unbounded_Text_Type to Primitive values

end LKQL.Eval_Contexts;

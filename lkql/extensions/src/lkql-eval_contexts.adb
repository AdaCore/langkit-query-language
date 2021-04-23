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

with Ada.Environment_Variables;
with Ada.Directories;
with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;

with GNAT.OS_Lib;
with GNATCOLL.Utils;

with Liblkqllang.Prelude; use Liblkqllang.Prelude;

with LKQL.Evaluation; use LKQL.Evaluation;
with LKQL.Builtin_Functions; use LKQL.Builtin_Functions;
with LKQL.String_Utils; use LKQL.String_Utils;
with LKQL.Unit_Utils; use LKQL.Unit_Utils;

package body LKQL.Eval_Contexts is

   procedure Free_Environment (Self : in out Environment_Access);

   ----------------------
   -- Free_Environment --
   ----------------------

   procedure Free_Environment (Self : in out Environment_Access) is
      procedure Free is new Ada.Unchecked_Deallocation
        (Environment, Environment_Access);
   begin
      --  Give up the reference we have on the parent env.
      Dec_Ref (Self.Parent);
      Free (Self);
   end Free_Environment;

   ---------------
   -- Add_Error --
   ---------------

   procedure Add_Error (Ctx : Eval_Context; Error : Error_Data) is
   begin
      Ctx.Kernel.Last_Error := Error;
   end Add_Error;

   ---------------------------
   -- Release_Current_Frame --
   ---------------------------

   procedure Release_Current_Frame (Ctx : in out Eval_Context) is
   begin
      Dec_Ref (Ctx.Frames);
   end Release_Current_Frame;

   ----------------
   -- Last_Error --
   ----------------

   function Last_Error (Ctx : Eval_Context) return Error_Data is
     (Ctx.Kernel.Last_Error);

   -------------------------
   -- Exists_In_Local_Env --
   -------------------------

   function Exists_In_Local_Env (Ctx : Eval_Context;
                                 Key : Symbol_Type) return Boolean
   is (Ctx.Frames.Local_Bindings.Contains (Key));

   ---------------
   -- Null_Node --
   ---------------

   function Null_Node (Ctx : Eval_Context) return AST_Node_Rc is
      (Ctx.Kernel.Null_Node);

   ----------------------------
   -- Error_Recovery_Enabled --
   ----------------------------

   function Error_Recovery_Enabled (Ctx : Eval_Context) return Boolean is
     (Ctx.Kernel.Error_Recovery_Enabled);

   ---------------
   -- AST_Roots --
   ---------------

   function AST_Roots (Ctx : Eval_Context) return AST_Node_Array_Access is
     (Ctx.Kernel.Ast_Roots);

   -----------
   -- Clone --
   -----------

   function Clone_Frame (Ctx : Eval_Context) return Eval_Context is
   begin
      --  The cloned env holds a reference to its parent, so increment the ref
      --  count.
      if Ctx.Frames.Parent /= null then
         Inc_Ref (Ctx.Frames.Parent);
      end if;

      return
        (Kernel => Ctx.Kernel,
         Frames => new Environment'(Ctx.Frames.all));
   end Clone_Frame;

   ----------------------
   -- Create_New_Frame --
   ----------------------

   function Create_New_Frame (Ctx            : Eval_Context;
                              Local_Bindings : Environment_Map := Empty_Map)
                              return Eval_Context
   is
      New_Env     : constant Environment_Access :=
        new Environment'
          (Local_Bindings => Local_Bindings,
           Parent         => Ctx.Frames,
           Ref_Count      => <>);
   begin
      --  The new env holds a reference to its parent, so increment the
      --  reference count.
      Inc_Ref (New_Env.Parent);
      return Eval_Context'(Ctx.Kernel, New_Env);
   end Create_New_Frame;

   ------------
   -- Lookup --
   ------------

   function Lookup (Ctx : Eval_Context;
                    Key : Symbol_Type) return String_Value_Maps.Cursor
   is (Lookup (Ctx.Frames.all, Key));

   -----------------
   -- Add_Binding --
   -----------------

   procedure Add_Binding (Ctx   : Eval_Context;
                          Key   : Text_Type;
                          Value : Primitive)
   is
   begin
      Add_Binding
        (Ctx, Find (Ctx.Kernel.Context.Get_Symbol_Table, Key), Value);
   end Add_Binding;

   -----------------
   -- Add_Binding --
   -----------------

   procedure Add_Binding (Ctx   : Eval_Context;
                          Key   : Symbol_Type;
                          Value : Primitive)
   is
   begin
      Ctx.Frames.Local_Bindings.Include (Key, Value);
   end Add_Binding;

   ---------------------
   -- Is_Root_Context --
   ---------------------

   function Is_Root_Context (Ctx : Eval_Context) return Boolean is
     (Ctx.Frames.Parent = null);

   --------------------
   -- Parent_Context --
   --------------------

   function Parent_Context (Ctx : Eval_Context) return Eval_Context is
      Parent_Env : constant Environment_Access :=
        Ctx.Frames.Parent;
   begin
      return Eval_Context'(Ctx.Kernel, Parent_Env);
   end Parent_Context;

   -----------------------
   -- Make_Eval_Context --
   -----------------------

   function Make_Eval_Context
     (Ast_Roots    : AST_Node_Array;
      Null_Node    : AST_Node_Rc;
      Analysis_Ctx : L.Analysis_Context := L.No_Analysis_Context;
      Err_Recovery : Boolean := False) return Eval_Context
   is
      use L;

      Roots : constant AST_Node_Array_Access := new AST_Node_Array'(Ast_Roots);
      Kernel : constant Global_Data_Access :=
        new Global_Data'
          (Roots, Null_Node, Make_Empty_Error, Err_Recovery,
           (if Analysis_Ctx = L.No_Analysis_Context
            then L.Create_Context
            else Analysis_Ctx), LKQL_Path_List => <>);
      Env    : constant Environment_Access :=
        new Environment'(Make_Empty_Environment);
      Ret    : Eval_Context := Eval_Context'(Kernel, Env);

      package E renames Ada.Environment_Variables;
   begin
      --  Adding Ada built-in functions + prelude to the context.
      declare
         U      : constant L.Analysis_Unit := Prelude_Unit (Kernel.Context);
         Dummy  : constant Primitive := Eval (Ret, U.Root);
      begin
         for Fn_Desc of Builtin_Functions.All_Builtins loop
            Ret.Add_Binding
              (To_Text (Fn_Desc.Name), Make_Builtin_Function (Fn_Desc));
         end loop;
      end;

      --  Set up LKQL_PATH for the context
      if E.Exists ("LKQL_PATH") then
         declare
            function Add_Path (Path : String) return Boolean;

            LKQL_Path_Content : constant String := E.Value ("LKQL_PATH");

            function Add_Path (Path : String) return Boolean is
            begin
               Add_LKQL_Path (Ret, Path);
               return True;
            end Add_Path;
         begin
            GNATCOLL.Utils.Split
              (LKQL_Path_Content,
               GNAT.OS_Lib.Path_Separator & "",
               Add_Path'Access);
         end;
      end if;

      return Ret;
   end Make_Eval_Context;

   -----------------------
   -- Free_Eval_Context --
   -----------------------

   procedure Free_Eval_Context (Ctx : in out Eval_Context) is
   begin
      pragma Assert (Ctx.Frames.Parent = null,
                     "Cannot free a non-root evaluation context");
      for Root of Ctx.Kernel.Ast_Roots.all loop
         Free_AST_Node (Root);
      end loop;

      Free_Ast_Node_Array (Ctx.Kernel.Ast_Roots);
      Dec_Ref (Ctx.Frames);
      Free_Global_Data (Ctx.Kernel);
   end Free_Eval_Context;

   ------------
   -- Lookup --
   ------------

   function Lookup (Env : Environment;
                    Key : Symbol_Type) return String_Value_Maps.Cursor
   is
      Lookup_Result : constant Cursor := Env.Local_Bindings.Find (Key);
   begin
      if not Has_Element (Lookup_Result) and then Env.Parent /= null then
         return Lookup (Env.Parent.all, Key);
      end if;

      return Lookup_Result;
   end Lookup;

   ----------------------------
   -- Make_Empty_Environment --
   ----------------------------

   function Make_Empty_Environment (Parent : Environment_Access := null)
                                    return Environment
   is (Environment'(String_Value_Maps.Empty_Map, Parent, Ref_Count => 1));

   ------------------
   -- Add_Bindings --
   ------------------

   procedure Add_Bindings
     (Env : in out Environment_Map; New_Bindings : Environment_Map)
   is
   begin
      for C in Iterate (New_Bindings) loop
         Env.Include (Key (C), Element (C));
      end loop;
   end Add_Bindings;

   procedure Merge_Item_Into (Env   : in out Environment_Map;
                              Key   : Symbol_Type;
                              Value : Primitive);

   ----------------
   -- Merge_Into --
   ----------------

   procedure Merge_Into
     (Env : in out Environment_Map; Other : Environment_Map)
   is
   begin
      for C in Iterate (Other) loop
         Merge_Item_Into (Env, Key (C), Element (C));
      end loop;
   end Merge_Into;

   ---------------------
   -- Merge_Item_Into --
   ---------------------

   procedure Merge_Item_Into (Env   : in out Environment_Map;
                              Key   : Symbol_Type;
                              Value : Primitive)
   is
      Pos : constant Cursor := Env.Find (Key);
   begin
      if not Has_Element (Pos) then
         Env.Insert (Key, Value);
      elsif Kind (Element (Pos)) = Kind_List then
         Extend (Element (Pos), Value);
      elsif Kind (Value) = Kind_List then
         Extend (Value, Element (Pos));
         Env.Include (Key, Value);
      else
         declare
            List : constant Primitive := Make_Empty_List;
         begin
            Append (List, Element (Pos));
            Append (List, Value);
            Env.Insert (Key, List);
         end;
      end if;
   end Merge_Item_Into;

   -------------
   -- Inc_Ref --
   -------------

   procedure Inc_Ref (Self : Environment_Access) is
   begin
      Self.Ref_Count := Self.Ref_Count + 1;
   end Inc_Ref;

   -----------------
   -- Get_Context --
   -----------------

   function Get_Context (Self : Global_Data) return L.Analysis_Context is
   begin
      return Self.Context;
   end Get_Context;

   -------------
   -- Dec_Ref --
   -------------

   procedure Dec_Ref (Self : in out Environment_Access) is
   begin
      if Self /= null then
         --  TODO U315-023: Because of the fact that we have non-handled cycles
         --  in the environments, due to closures, package level finalization
         --  will try to free those cycles and might reach a point where this
         --  is called twice in a cycle, so Ref_Count is already equal to zero.
         --  In those cases, just no-op.
         if Self.Ref_Count > 0 then
            Self.Ref_Count := Self.Ref_Count - 1;

            if Self.Ref_Count = 0 then
               Free_Environment (Self);
            end if;
         end if;
      end if;
   end Dec_Ref;

   ---------------
   -- Env_Image --
   ---------------

   function Env_Image (Env : Environment_Access) return String is
   begin
      return "<Env: " & Address_Image (Env.all'Address)
        & " rc=" & Env.Ref_Count'Image & " "
        & " " & Env_Map_Image (Env.Local_Bindings)
        & (if Env.Parent /= null
           then " parent:" & Address_Image (Env.Parent.all'Address)
           & " rc=" & Env.Parent.Ref_Count'Image & " "
           else "")
        & " " & (if Env.Parent /= null
           then (if Env.Parent.Parent = null
             then "root"
             else Env_Map_Image (Env.Parent.Local_Bindings))
           else "");
   end Env_Image;

   -------------------
   -- Env_Map_Image --
   -------------------

   function Env_Map_Image (Self : Environment_Map) return String is
      Ret : Unbounded_Text_Type;
      Cur : String_Value_Maps.Cursor := Self.First;
      Is_First : Boolean := True;
   begin
      Append (Ret, "{");
      loop
         exit when not String_Value_Maps.Has_Element (Cur);
         if not Is_First then
            Append (Ret, ", ");
         else
            Is_First := False;
         end if;

         Append (Ret, """" & String_Value_Maps.Key (Cur).all & """: ");
         Append (Ret, To_Unbounded_Text (String_Value_Maps.Element (Cur)));
         String_Value_Maps.Next (Cur);
      end loop;
      Append (Ret, "}");
      return Image (To_Text (Ret));
   end Env_Map_Image;

   -------------------
   -- Get_LKQL_Unit --
   -------------------

   function Get_LKQL_Unit
     (Ctx          : Eval_Context;
      Package_Name : String;
      From         : L.Analysis_Unit := L.No_Analysis_Unit)
      return L.Analysis_Unit
   is
      function Get_Unit_From_Dir (Dir : String) return L.Analysis_Unit;

      package D renames Ada.Directories;

      From_Path : constant String :=
        D.Containing_Directory (From.Get_Filename);

      function Get_Unit_From_Dir (Dir : String) return L.Analysis_Unit is
         Tentative_File_Name : constant String :=
           D.Compose (Dir, Package_Name, "lkql");
      begin
         if D.Exists (Tentative_File_Name) then
            return Make_LKQL_Unit (Ctx.Kernel.Context, Tentative_File_Name);
         else
            return L.No_Analysis_Unit;
         end if;
      end Get_Unit_From_Dir;

      Unit : L.Analysis_Unit;

      use type L.Analysis_Unit;
   begin
      --  First, check into the current directory (directory containing the
      --  lkql file from which the package is requested).
      Unit := Get_Unit_From_Dir (From_Path);

      if Unit /= L.No_Analysis_Unit then
         return Unit;
      end if;

      --  Then check the LKQL path.
      for Path of Ctx.Kernel.LKQL_Path_List loop
         Unit := Get_Unit_From_Dir (To_String (Path));
         if Unit /= L.No_Analysis_Unit then
            return Unit;
         end if;
      end loop;

      return L.No_Analysis_Unit;
   end Get_LKQL_Unit;

   -------------------
   -- Add_LKQL_Path --
   -------------------

   procedure Add_LKQL_Path (Ctx : in out Eval_Context; Path : String) is
   begin
      Ctx.Kernel.LKQL_Path_List.Append (To_Unbounded_String (Path));
   end Add_LKQL_Path;

   -----------------
   -- Get_Env_Map --
   -----------------

   function Get_Env_Map (Self : Environment_Access) return Environment_Map is
   begin
      return Self.Local_Bindings;
   end Get_Env_Map;

   ----------------
   -- Get_Parent --
   ----------------

   function Get_Parent (Self : Environment_Access) return Environment_Access is
   begin
      return Self.Parent;
   end Get_Parent;

end LKQL.Eval_Contexts;

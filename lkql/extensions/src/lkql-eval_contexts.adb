------------------------------------------------------------------------------
--                                                                          --
--                                   LKQL                                   --
--                                                                          --
--                     Copyright (C) 2019-2022, AdaCore                     --
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

with Liblkqllang.Prelude;    use Liblkqllang.Prelude;

with LKQL.Evaluation;        use LKQL.Evaluation;
with LKQL.Builtin_Functions; use LKQL.Builtin_Functions;
with LKQL.String_Utils;      use LKQL.String_Utils;
with LKQL.Unit_Utils;        use LKQL.Unit_Utils;
with LKQL.Error_Handling;    use LKQL.Error_Handling;

package body LKQL.Eval_Contexts is

   procedure Free_Environment (Self : in out Environment_Access);

   procedure Free_Lk_Node_Array
   is new Ada.Unchecked_Deallocation
     (LK.Lk_Node_Array, Lk_Node_Array_Access);

   ----------------------
   -- Free_Environment --
   ----------------------

   procedure Free_Environment (Self : in out Environment_Access) is
      procedure Free is new Ada.Unchecked_Deallocation
        (Environment, Environment_Access);
   begin
      --  Give up the reference we have on the parent env.
      Dec_Ref (Self.Parent);

      --  If this environment owns a memory pool, release it.
      if Self.Pools /= null
         and then Self.Is_Pools_Owner
      then
         Destroy (Self.Pools);
      end if;
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

   -------------------------------
   -- Attach_Node_To_Last_Error --
   -------------------------------

   procedure Attach_Node_To_Last_Error
     (Ctx  : Eval_Context; Node : L.Lkql_Node) is
   begin
      Ctx.Kernel.Last_Error.AST_Node := Node;
   end Attach_Node_To_Last_Error;

   -------------------------
   -- Exists_In_Local_Env --
   -------------------------

   function Exists_In_Local_Env (Ctx : Eval_Context;
                                 Key : Symbol_Type) return Boolean
   is (Ctx.Frames.Local_Bindings.Contains (Key));

   ---------------
   -- AST_Roots --
   ---------------

   function AST_Roots (Ctx : Eval_Context) return Lk_Node_Array_Access is
     (Ctx.Kernel.Ast_Roots);

   -------------------
   -- Set_AST_Roots --
   -------------------

   procedure Set_Units
     (Ctx   : Eval_Context;
      Units : LK_Unit_Array)
   is
   begin
      Free_Lk_Node_Array (Ctx.Kernel.Ast_Roots);

      Ctx.Kernel.Ast_Roots :=
        new LK.Lk_Node_Array (Units'Range);

      for J in Ctx.Kernel.Ast_Roots'Range loop
         Ctx.Kernel.Ast_Roots (J) := Units (J).Root;
      end loop;
   end Set_Units;

   -----------
   -- Clone --
   -----------

   function Ref_Frame (Ctx : Eval_Context) return Eval_Context is
   begin
      Inc_Ref (Ctx.Frames);
      return Ctx;
   end Ref_Frame;

   ----------------------
   -- Create_New_Frame --
   ----------------------

   function Create_New_Frame
     (Ctx            : Eval_Context;
      Local_Bindings : Environment_Map := Empty_Map) return Eval_Context
   is
      New_Env     : constant Environment_Access :=
        new Environment'
          (Local_Bindings  => Local_Bindings,
           Parent          => Ctx.Frames,
           Ref_Count       => <>,
           Pools           => Ctx.Pools,
           Is_Pools_Owner  => False);
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
     (Ast_Roots    : LK.Lk_Node_Array;
      Lang_Id      : Langkit_Support.Generic_API.Language_Id;
      Analysis_Ctx : L.Analysis_Context := L.No_Analysis_Context)
      return Eval_Context
   is
      use L;

      Roots : constant Lk_Node_Array_Access :=
        new Lk_Node_Array'(Ast_Roots);
      Kernel : constant Global_Data_Access :=
        new Global_Data'
          (Roots, Make_Empty_Error,
           (if Analysis_Ctx = L.No_Analysis_Context
            then L.Create_Context
            else Analysis_Ctx),
           Lkql_Path_List     => <>,
           Builtin_Methods    => <>,
           Lang_Id            => Lang_Id,
           Name_Map           => null);
      Env    : constant Environment_Access :=
        new Environment'(Make_Empty_Environment (Create_Pool => True));
      Ret    : Eval_Context := Eval_Context'(Kernel, Env);

      package E renames Ada.Environment_Variables;
   begin
      --  Adding Ada built-in functions + prelude to the context.
      declare
         U      : constant L.Analysis_Unit := Prelude_Unit (Ret);

         Dummy  : constant Primitive := Eval (Ret, U.Root);
      begin
         for Fn_Desc of Builtin_Functions.All_Builtins loop

            --  Add bindings to the toplevel scope, if the function is not
            --  marked as being accessible only via dot notation calls on
            --  objects.
            if not Fn_Desc.Only_Dot_Calls then
               Ret.Add_Binding
                 (To_Text (Fn_Desc.Name),
                  Make_Builtin_Function (Fn_Desc, Ret.Pool));
            end if;

            --  For applicable functions, register them in the builtin
            --  properties table, which will allow them to be called via
            --  the dot notation on objects.
            if Fn_Desc.Params'Length > 0 then
               declare
                  Name : constant Symbol_Type := Find
                    (Kernel.Context.Get_Symbol_Table,
                     To_Text (Fn_Desc.Name));
               begin
                  Kernel.Builtin_Methods.Include
                    ((Fn_Desc.Params (1).Expected_Kind, Name),
                     Fn_Desc);
               end;
            end if;
         end loop;
      end;

      --  Set up LKQL_PATH for the context
      if E.Exists ("LKQL_PATH") then
         declare
            function Add_Path (Path : String) return Boolean;

            Lkql_Path_Content : constant String := E.Value ("LKQL_PATH");

            function Add_Path (Path : String) return Boolean is
            begin
               Add_Lkql_Path (Ret, Path);
               return True;
            end Add_Path;
         begin
            GNATCOLL.Utils.Split
              (Lkql_Path_Content,
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

      Free_Lk_Node_Array (Ctx.Kernel.Ast_Roots);
      Free_Environment (Ctx.Frames);
      Free_Global_Data (Ctx.Kernel);
   end Free_Eval_Context;

   ------------
   -- Lookup --
   ------------

   function Lookup
     (Env   : Environment;
      Key   : Symbol_Type;
      Local : Boolean := False) return String_Value_Maps.Cursor
   is
      Lookup_Result : constant Cursor := Env.Local_Bindings.Find (Key);
   begin
      if not Has_Element (Lookup_Result)
         and then not Local and then Env.Parent /= null
      then
         return Lookup (Env.Parent.all, Key);
      end if;

      return Lookup_Result;
   end Lookup;

   ----------------------------
   -- Make_Empty_Environment --
   ----------------------------

   function Make_Empty_Environment
     (Parent      : Environment_Access := null;
      Create_Pool : Boolean := False) return Environment
   is
   begin
      return Environment'
        (String_Value_Maps.Empty_Map,
         Parent,
         Ref_Count      => 1,
         Pools          => (if Create_Pool then Create else null),
         Is_Pools_Owner => Create_Pool);
   end Make_Empty_Environment;

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

   -------------------------
   -- Get_Builtin_Methods --
   -------------------------

   function Get_Builtin_Methods
     (Self : Global_Data_Access) return Builtin_Methods_Map_Access is
   begin
      return Self.Builtin_Methods'Unrestricted_Access;
   end Get_Builtin_Methods;

   ------------------
   -- Get_Name_Map --
   ------------------

   function Get_Name_Map (Ctx : Eval_Context) return Name_Map_Access is
   begin
      if Ctx.Kernel.Name_Map = null then
         Ctx.Kernel.Name_Map := new LKI.Name_Map'
           (LKI.Create_Name_Map
             (Ctx.Kernel.Lang_Id,
              Ctx.Kernel.Context.Get_Symbol_Table,
              LKN.Camel, LKN.Lower, LKN.Camel, LKN.Lower));
      end if;

      return Ctx.Kernel.Name_Map;
   end Get_Name_Map;

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

   --------------
   -- Get_Pool --
   --------------

   function Get_Pools (Self : Environment_Access) return Primitive_Pool_Stack
   is
   begin
      return Self.Pools;
   end Get_Pools;

   -------------------
   -- Get_Lkql_Unit --
   -------------------

   function Get_Lkql_Unit
     (Ctx          : Eval_Context;
      Package_Name : String;
      From         : L.Analysis_Unit := L.No_Analysis_Unit)
      return L.Analysis_Unit
   is
      function Get_Unit_From_Dir (Dir : String) return L.Analysis_Unit;

      package D renames Ada.Directories;

      -----------------------
      -- Get_Unit_From_Dir --
      -----------------------

      function Get_Unit_From_Dir (Dir : String) return L.Analysis_Unit is
         Tentative_File_Name : constant String :=
           D.Compose (Dir, Package_Name, "lkql");
      begin
         if D.Exists (Tentative_File_Name) then
            return Make_Lkql_Unit (Ctx, Tentative_File_Name);
         else
            return L.No_Analysis_Unit;
         end if;
      end Get_Unit_From_Dir;

      Unit : L.Analysis_Unit;

      use type L.Analysis_Unit;
   begin
      --  First, check into the current directory (directory containing the
      --  lkql file from which the package is requested).
      if From /= L.No_Analysis_Unit then
         declare
            From_Path : constant String :=
              D.Containing_Directory (From.Get_Filename);
         begin
            Unit := Get_Unit_From_Dir (From_Path);
         end;
      end if;

      if Unit /= L.No_Analysis_Unit then
         return Unit;
      end if;

      --  Then check the LKQL path.
      for Path of Ctx.Kernel.Lkql_Path_List loop
         Unit := Get_Unit_From_Dir (To_String (Path));
         if Unit /= L.No_Analysis_Unit then
            return Unit;
         end if;
      end loop;

      return L.No_Analysis_Unit;
   end Get_Lkql_Unit;

   -------------------
   -- Add_Lkql_Path --
   -------------------

   procedure Add_Lkql_Path (Ctx : in out Eval_Context; Path : String) is
   begin
      Ctx.Kernel.Lkql_Path_List.Append (To_Unbounded_String (Path));
   end Add_Lkql_Path;

   -----------------
   -- Raise_Error --
   -----------------

   procedure Raise_Error
     (Ctx : Eval_Context;
      N : L.Lkql_Node'Class;
      Err : Text_Type)
   is
   begin
      Raise_And_Record_Error
        (Ctx, Make_Eval_Error (N, Err));
   end Raise_Error;

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

   ------------
   -- Symbol --
   ------------

   function Symbol (Ctx : Eval_Context; Str : Text_Type) return Symbol_Type is
   begin
      return Find (Ctx.Kernel.Context.Get_Symbol_Table, Str);
   end Symbol;

   ----------
   -- Pool --
   ----------

   function Pool (Ctx : Eval_Context) return Primitive_Pool is
   begin
      return Get_Pools (Ctx.Frames).Last_Element;
   end Pool;

   -----------
   -- Pools --
   -----------

   function Pools (Ctx : Eval_Context) return Primitive_Pool_Stack is
   begin
      return Get_Pools (Ctx.Frames);
   end Pools;

   -------------
   -- Lang_Id --
   -------------

   function Lang_Id
     (Ctx : Eval_Context) return Langkit_Support.Generic_API.Language_Id
   is
   begin
      return Ctx.Kernel.Lang_Id;
   end Lang_Id;

end LKQL.Eval_Contexts;

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

with Ada.Strings.Wide_Wide_Unbounded;
with Ada.Wide_Wide_Characters.Handling;
with Ada.Wide_Wide_Text_IO;

with Langkit_Support.Text; use Langkit_Support.Text;

with LKQL.AST_Nodes;

with Ada_AST_Nodes; use Ada_AST_Nodes;
with LKQL.Evaluation; use LKQL.Evaluation;
with LKQL.Eval_Contexts; use LKQL.Eval_Contexts;
with LKQL.String_Utils; use LKQL.String_Utils;

package body LKQL.Builtin_Functions is

   function Get_Doc (Ctx : Eval_Context; Obj : Primitive) return Text_Type;

   function Eval_Print
     (Ctx : Eval_Context; Args : Primitive_Array) return Primitive;

   function Eval_To_List
     (Ctx : Eval_Context; Args : Primitive_Array) return Primitive;

   function Eval_Dump
     (Ctx : Eval_Context; Args : Primitive_Array) return Primitive;

   function Eval_Image
     (Ctx : Eval_Context; Args : Primitive_Array) return Primitive;

   function Eval_Children_Count
     (Ctx : Eval_Context; Args : Primitive_Array) return Primitive;

   function Eval_Text
     (Ctx : Eval_Context; Args : Primitive_Array) return Primitive;

   function Eval_Starts_With
     (Ctx : Eval_Context; Args : Primitive_Array) return Primitive;

   function Eval_Ends_With
     (Ctx : Eval_Context; Args : Primitive_Array) return Primitive;

   function Eval_Is_Lower_Case
     (Ctx : Eval_Context; Args : Primitive_Array) return Primitive;

   function Eval_Is_Upper_Case
     (Ctx : Eval_Context; Args : Primitive_Array) return Primitive;

   function Eval_Is_Mixed_Case
     (Ctx : Eval_Context; Args : Primitive_Array) return Primitive;

   function Eval_Doc
     (Ctx : Eval_Context; Args : Primitive_Array) return Primitive;

   function Eval_Profile
     (Ctx : Eval_Context; Args : Primitive_Array) return Primitive;

   function Eval_Get_Symbols
     (Ctx : Eval_Context; Args : Primitive_Array) return Primitive;

   function Eval_Help
     (Ctx : Eval_Context; Args : Primitive_Array) return Primitive;

   function Create
     (Name      : Text_Type;
      Params    : Builtin_Function_Profile;
      Fn_Access : Native_Function_Access;
      Doc       : Text_Type) return Builtin_Function;
   --  Create a builtin function given a name, a description of its
   --  parameters and an access to the native code that implements it.

   function Param
     (Name          : Text_Type;
      Expected_Kind : Base_Primitive_Kind := No_Kind)
      return Builtin_Param_Description;
   --  Create a builtin parameter description given its name and its expected
   --  kind. The expected kind can be "No_Kind" if no particular kind is
   --  expected. This parameter will not have a default value.

   function Param
     (Name          : Text_Type;
      Expected_Kind : Base_Primitive_Kind;
      Default_Value : Primitive)
      return Builtin_Param_Description;
   --  Create a builtin parameter description given its name, expected
   --  kind and default value. The expected kind can be "No_Kind" if no
   --  particular kind is expected.

   ------------
   -- Create --
   ------------

   function Create
     (Name      : Text_Type;
      Params    : Builtin_Function_Profile;
      Fn_Access : Native_Function_Access;
      Doc       : Text_Type) return Builtin_Function
   is
   begin
      return new Builtin_Function_Description'
        (N         => Params'Length,
         Name      => To_Unbounded_Text (Name),
         Params    => Params,
         Fn_Access => Fn_Access,
         Doc       => To_Unbounded_Text (Doc));
   end Create;

   -----------
   -- Param --
   -----------

   function Param
     (Name          : Text_Type;
      Expected_Kind : Base_Primitive_Kind := No_Kind)
      return Builtin_Param_Description
   is
   begin
      return Builtin_Param_Description'
        (Name          => To_Unbounded_Text (Name),
         Expected_Kind => Expected_Kind,
         Default_Value => Primitive_Options.None);
   end Param;

   -----------
   -- Param --
   -----------

   function Param
     (Name          : Text_Type;
      Expected_Kind : Base_Primitive_Kind;
      Default_Value : Primitive)
      return Builtin_Param_Description
   is
   begin
      return Builtin_Param_Description'
        (Name          => To_Unbounded_Text (Name),
         Expected_Kind => Expected_Kind,
         Default_Value => Primitive_Options.To_Option (Default_Value));
   end Param;

   ----------------
   -- Eval_Print --
   ----------------

   function Eval_Print
     (Ctx : Eval_Context; Args : Primitive_Array) return Primitive
   is
      pragma Unreferenced (Ctx);
   begin
      Display (Args (1), Bool_Val (Args (2)));
      return Make_Unit_Primitive;
   end Eval_Print;

   ------------------
   -- Eval_To_List --
   ------------------

   function Eval_To_List
     (Ctx : Eval_Context; Args : Primitive_Array) return Primitive
   is
      pragma Unreferenced (Ctx);
   begin
      return To_List (Args (1).Get.Iter_Val.all);
   end Eval_To_List;

   ---------------
   -- Eval_Dump --
   ---------------

   function Eval_Dump
     (Ctx : Eval_Context; Args : Primitive_Array) return Primitive
   is
      pragma Unreferenced (Ctx);
   begin
      Ada_AST_Node (Args (1).Get.Node_Val.Unchecked_Get.all).Node.Print;
      return Make_Unit_Primitive;
   end Eval_Dump;

   ----------------
   -- Eval_Image --
   ----------------

   function Eval_Image
     (Ctx : Eval_Context; Args : Primitive_Array) return Primitive
   is
      pragma Unreferenced (Ctx);
   begin
      return To_Primitive (To_Unbounded_Text (Args (1)));
   end Eval_Image;

   -------------------------
   -- Eval_Children_Count --
   -------------------------

   function Eval_Children_Count
     (Ctx : Eval_Context; Args : Primitive_Array) return Primitive
   is
      pragma Unreferenced (Ctx);
      Node : constant AST_Nodes.AST_Node'Class :=
         Node_Val (Args (1)).Unchecked_Get.all;
   begin
      return To_Primitive
        (if Node.Is_Null_Node then 0 else Node.Children_Count);
   end Eval_Children_Count;

   ---------------
   -- Eval_Text --
   ---------------

   function Eval_Text
     (Ctx : Eval_Context; Args : Primitive_Array) return Primitive
   is
      pragma Unreferenced (Ctx);
      Node : constant AST_Nodes.AST_Node'Class :=
         Node_Val (Args (1)).Unchecked_Get.all;
   begin
      return To_Primitive (if Node.Is_Null_Node then "" else Node.Text);
   end Eval_Text;

   -----------------
   -- Starts_With --
   -----------------

   function Eval_Starts_With
     (Ctx : Eval_Context; Args : Primitive_Array) return Primitive
   is
      pragma Unreferenced (Ctx);
      use Ada.Strings.Wide_Wide_Unbounded;

      Str    : constant Unbounded_Text_Type := Str_Val (Args (1));
      Prefix : constant Unbounded_Text_Type := Str_Val (Args (2));
      Len    : constant Natural := Length (Prefix);
   begin
      return To_Primitive
         (Length (Str) >= Len
          and then Unbounded_Slice (Str, 1, Len) = Prefix);
   end Eval_Starts_With;

   ---------------
   -- Ends_With --
   ---------------

   function Eval_Ends_With
     (Ctx : Eval_Context; Args : Primitive_Array) return Primitive
   is
      pragma Unreferenced (Ctx);
      use Ada.Strings.Wide_Wide_Unbounded;

      Str    : constant Unbounded_Text_Type := Str_Val (Args (1));
      Suffix : constant Unbounded_Text_Type := Str_Val (Args (2));

      Str_Len    : constant Natural := Length (Str);
      Suffix_Len : constant Natural := Length (Suffix);
   begin
      return To_Primitive
         (Str_Len >= Suffix_Len
          and then Unbounded_Slice
            (Str, Str_Len - Suffix_Len + 1, Str_Len) = Suffix);
   end Eval_Ends_With;

   ------------------------
   -- Eval_Is_Lower_Case --
   ------------------------

   function Eval_Is_Lower_Case
     (Ctx : Eval_Context; Args : Primitive_Array) return Primitive
   is
      pragma Unreferenced (Ctx);
      use Ada.Strings.Wide_Wide_Unbounded;
      use Ada.Wide_Wide_Characters.Handling;

      Str : constant Text_Type := To_Text (Str_Val (Args (1)));
   begin
      for C of Str loop
         if Is_Upper (C) then
            return To_Primitive (False);
         end if;
      end loop;
      return To_Primitive (True);
   end Eval_Is_Lower_Case;

   ------------------------
   -- Eval_Is_Upper_Case --
   ------------------------

   function Eval_Is_Upper_Case
     (Ctx : Eval_Context; Args : Primitive_Array) return Primitive
   is
      pragma Unreferenced (Ctx);
      use Ada.Strings.Wide_Wide_Unbounded;
      use Ada.Wide_Wide_Characters.Handling;

      Str : constant Text_Type := To_Text (Str_Val (Args (1)));
   begin
      for C of Str loop
         if Is_Lower (C) then
            return To_Primitive (False);
         end if;
      end loop;
      return To_Primitive (True);
   end Eval_Is_Upper_Case;

   ------------------------
   -- Eval_Is_Mixed_Case --
   ------------------------

   function Eval_Is_Mixed_Case
     (Ctx : Eval_Context; Args : Primitive_Array) return Primitive
   is
      pragma Unreferenced (Ctx);
      use Ada.Strings.Wide_Wide_Unbounded;
      use Ada.Wide_Wide_Characters.Handling;

      Str : constant Text_Type := To_Text (Str_Val (Args (1)));

      Must_Be_Upper_Case : Boolean := True;
   begin
      for C of Str loop
         if Must_Be_Upper_Case then
            if Is_Lower (C) then
               return To_Primitive (False);
            else
               Must_Be_Upper_Case := False;
            end if;
         elsif Is_Upper (C) then
            return To_Primitive (False);
         elsif C = '_' then
            Must_Be_Upper_Case := True;
         end if;
      end loop;
      return To_Primitive (True);
   end Eval_Is_Mixed_Case;

   -------------
   -- Get_Doc --
   -------------

   function Get_Doc (Ctx : Eval_Context; Obj : Primitive) return Text_Type is
   begin
      case Kind (Obj) is
         when Kind_Builtin_Function =>
            return To_Text (Obj.Unchecked_Get.Builtin_Fn.Doc);
         when Kind_Function =>
            declare
               Doc_Obj : constant L.Base_String_Literal :=
                 Obj.Unchecked_Get.Fun_Node.P_Doc;
            begin
               return
                 (if Doc_Obj.Is_Null
                  then ""
                  else To_Text (Str_Val (Eval (Ctx, Doc_Obj))));
            end;
         when Kind_Selector =>
            declare
               Doc : constant L.Base_String_Literal :=
                 Obj.Unchecked_Get.Sel_Node.P_Doc;
            begin
               if not Doc.Is_Null then
                  return To_Text (Str_Val (Eval (Ctx, Doc)));
               end if;
               return "";
            end;
         when Kind_Namespace =>
            if Obj.Unchecked_Get.Module.Children_Count = 0 then
               return "";
            end if;

            declare
               First_Child : constant L.LKQL_Node :=
                 Obj.Unchecked_Get.Module.Child (1);
            begin
               if First_Child.Kind in LCO.LKQL_Base_String_Literal then
                  return To_Text (Str_Val (Eval (Ctx, First_Child)));
               end if;
               return "";
            end;
         when others =>
            return "";
      end case;
   end Get_Doc;

   --------------
   -- Eval_Doc --
   --------------

   function Eval_Doc
     (Ctx : Eval_Context; Args : Primitive_Array) return Primitive is
   begin
      return To_Primitive (Get_Doc (Ctx, Args (1)));
   end Eval_Doc;

   ------------------
   -- Eval_Profile --
   ------------------

   function Eval_Profile
     (Ctx : Eval_Context; Args : Primitive_Array) return Primitive
   is
      pragma Unreferenced (Ctx);
   begin
      return To_Primitive (Profile (Args (1)));
   end Eval_Profile;

   ---------------
   -- Eval_Help --
   ---------------

   function Eval_Help
     (Ctx : Eval_Context; Args : Primitive_Array) return Primitive
   is
      Obj : constant Primitive := Args (1);
      Doc : constant Text_Type := Get_Doc (Ctx, Obj);
   begin
      Ada.Wide_Wide_Text_IO.Put_Line (Profile (Obj));
      Ada.Wide_Wide_Text_IO.New_Line;
      Ada.Wide_Wide_Text_IO.Set_Col (4);
      Ada.Wide_Wide_Text_IO.Put_Line (Doc);
      Ada.Wide_Wide_Text_IO.Set_Col (1);

      return Make_Unit_Primitive;
   end Eval_Help;

   ----------------------------
   -- Eval_Get_Local_Symbols --
   ----------------------------

   function Eval_Get_Symbols
     (Ctx : Eval_Context; Args : Primitive_Array) return Primitive
   is
      procedure Get_Symbols_In_Frame
        (C : Eval_Contexts.Environment_Access; Recurse : Boolean);

      S : Symbol_Set;

      procedure Get_Symbols_In_Frame
        (C : Eval_Contexts.Environment_Access; Recurse : Boolean)
      is
      begin
         if C = null then
            return;
         end if;
         for I in Get_Env_Map (C).Iterate loop
            S.Include (String_Value_Maps.Key (I));
         end loop;

         if Recurse then
            Get_Symbols_In_Frame (Get_Parent (C), True);
         end if;
      end Get_Symbols_In_Frame;

      Pkg : constant Primitive := Args (1);
      Ret : constant Primitive := Make_Empty_List;
   begin

      if Booleanize (Pkg) then
         Get_Symbols_In_Frame
           (Eval_Contexts.Environment_Access (Pkg.Unchecked_Get.Namespace),
            Recurse => False);
      else
         Get_Symbols_In_Frame (Ctx.Frames, Recurse => True);
      end if;

      for El of S loop
         Ret.Get.List_Val.Elements.Append (To_Primitive (El.all));
      end loop;
      return Ret;

   end Eval_Get_Symbols;

   -----------------------
   -- Builtin_Functions --
   -----------------------

   Builtin_Functions : constant Builtin_Function_Array :=
     (Create
        ("print",
         (Param ("val"),
          Param ("new_line", Kind_Bool, To_Primitive (True))),
         Eval_Print'Access,
         "Built-in print function. Prints whatever is passed as an argument"),

      Create
        ("img",
         (1 => Param ("val")),
         Eval_Image'Access,
         "Return a string representation of an object"),

      Create
        ("dump",
         (1 => Param ("node", Kind_Node)),
         Eval_Dump'Access,
         "Given an ast node, return a structured dump of the subtree"),

      Create
        ("text",
         (1 => Param ("node", Kind_Node)),
         Eval_Text'Access,
         "Given an ast node, return its text"),

      Create
        ("to_list",
         (1 => Param ("it", Kind_Iterator)),
         Eval_To_List'Access,
         "Transform an iterator into a list"),

      Create
        ("children_count",
         (1 => Param ("node", Kind_Node)),
         Eval_Children_Count'Access,
         "Given a node, return the count of its children"),

      --  String builtins

      Create
        ("starts_with",
         (Param ("str", Kind_Str), Param ("prefix", Kind_Str)),
         Eval_Starts_With'Access,
         "Given a string, returns whether it starts with the given prefix"),

      Create
        ("ends_with",
         (Param ("str", Kind_Str), Param ("suffix", Kind_Str)),
         Eval_Ends_With'Access,
         "Given a string, returns whether it ends with the given suffix"),

      Create
        ("is_lower_case",
         (1 => Param ("str", Kind_Str)),
         Eval_Is_Lower_Case'Access,
         "Return whether the given string contains lower case characters "
         & "only"),

      Create
        ("is_upper_case",
         (1 => Param ("str", Kind_Str)),
         Eval_Is_Upper_Case'Access,
         "Return whether the given string contains upper case characters "
         & "only"),

      Create
        ("is_mixed_case",
         (1 => Param ("str", Kind_Str)),
         Eval_Is_Mixed_Case'Access,
         "Return whether the given string is written in mixed case, that is, "
         & "with only lower case characters except the first one and every "
         & "character following an underscore"),

      Create
        ("doc",
         (1 => Param ("obj")),
         Eval_Doc'Access,
         "Given any object, return the documentation associated with it"),

      Create
        ("profile",
         (1 => Param ("obj")),
         Eval_Profile'Access,
         "Given any object, if it is a callable, return its profile as text"),

      Create
        ("get_symbols",
         (1 => Param ("package", Kind_Namespace, Make_Unit_Primitive)),
         Eval_Get_Symbols'Access,
         "Given a module, return the symbols stored in it. If given no module"
         & ", return the local symbols"),

        Create
          ("help",
           (1 => Param ("obj")),
           Eval_Help'Access,
           "Given any object, return formatted help for it")

      );

   ------------------
   -- All_Builtins --
   ------------------

   function All_Builtins return Builtin_Function_Array is
     (Builtin_Functions);

end LKQL.Builtin_Functions;

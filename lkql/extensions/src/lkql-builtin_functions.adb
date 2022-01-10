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

with Ada.Characters.Conversions; use Ada.Characters.Conversions;
with Ada.Containers.Hashed_Sets;
with Ada.Directories;
with Ada.Finalization;
with Ada.Strings.Wide_Wide_Fixed;
with Ada.Unchecked_Deallocation;
with Ada.Wide_Wide_Characters.Handling;
with Ada.Wide_Wide_Text_IO;

with GNAT.Array_Split;
with GNAT.Regpat;

with Langkit_Support.Text; use Langkit_Support.Text;

with LKQL.AST_Nodes;

with Ada_AST_Nodes; use Ada_AST_Nodes;
with LKQL.Adaptive_Integers; use LKQL.Adaptive_Integers;
with LKQL.Evaluation; use LKQL.Evaluation;
with LKQL.Eval_Contexts; use LKQL.Eval_Contexts;
with LKQL.String_Utils; use LKQL.String_Utils;
with LKQL.Partial_AST_Nodes; use LKQL.Partial_AST_Nodes;
with LKQL.Errors; use LKQL.Errors;
with LKQL.Error_Handling; use LKQL.Error_Handling;

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

   function Eval_To_Lower_Case
     (Ctx : Eval_Context; Args : Primitive_Array) return Primitive;

   function Eval_Concat
     (Ctx : Eval_Context; Args : Primitive_Array) return Primitive;

   function Eval_Split
     (Ctx : Eval_Context; Args : Primitive_Array) return Primitive;

   function Eval_Substring
     (Ctx : Eval_Context; Args : Primitive_Array) return Primitive;

   function Eval_Base_Name
     (Ctx : Eval_Context; Args : Primitive_Array) return Primitive;

   function Eval_Doc
     (Ctx : Eval_Context; Args : Primitive_Array) return Primitive;

   function Eval_Profile
     (Ctx : Eval_Context; Args : Primitive_Array) return Primitive;

   function Eval_Get_Symbols
     (Ctx : Eval_Context; Args : Primitive_Array) return Primitive;

   function Eval_Get_Builtin_Methods_Info
     (Ctx : Eval_Context; Args : Primitive_Array) return Primitive;

   function Eval_Help
     (Ctx : Eval_Context; Args : Primitive_Array) return Primitive;

   function Eval_Token_Next
     (Ctx : Eval_Context; Args : Primitive_Array) return Primitive;

   function Eval_Token_Previous
     (Ctx : Eval_Context; Args : Primitive_Array) return Primitive;

   function Eval_Tokens
     (Ctx : Eval_Context; Args : Primitive_Array) return Primitive;

   function Eval_Units
     (Ctx : Eval_Context; Args : Primitive_Array) return Primitive;

   function Eval_Unit_Tokens
     (Ctx : Eval_Context; Args : Primitive_Array) return Primitive;

   function Eval_Token_Text
     (Ctx : Eval_Context; Args : Primitive_Array) return Primitive;

   function Eval_Token_Kind
     (Ctx : Eval_Context; Args : Primitive_Array) return Primitive;

   function Eval_Find
     (Ctx  : Eval_Context; Args : Primitive_Array) return Primitive;

   function Find_Pattern_Or_String
     (Ctx : Eval_Context;
      Str : Text_Type;
      To_Find : Primitive) return Natural;
   --  Find ``To_Find`` in ``Str``. ``To_Find`` can be either a string or a
   --  regex value. Return position of match or 0 if no match.

   function Eval_Contains
     (Ctx  : Eval_Context; Args : Primitive_Array) return Primitive;

   function Create
     (Name           : Text_Type;
      Params         : Builtin_Function_Profile;
      Fn_Access      : Native_Function_Access;
      Doc            : Text_Type;
      Only_Dot_Calls : Boolean := False) return Builtin_Function;
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
     (Name           : Text_Type;
      Params         : Builtin_Function_Profile;
      Fn_Access      : Native_Function_Access;
      Doc            : Text_Type;
      Only_Dot_Calls : Boolean := False) return Builtin_Function
   is
   begin
      return new Builtin_Function_Description'
        (N              => Params'Length,
         Name           => To_Unbounded_Text (Name),
         Params         => Params,
         Fn_Access      => Fn_Access,
         Doc            => To_Unbounded_Text (Doc),
         Only_Dot_Calls => Only_Dot_Calls);
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
      Result : constant Primitive := Make_Empty_List (Ctx.Pool);
      Lst    : constant Primitive := Args (1);
      Els    : Primitive_Vector_Access;
   begin
      case Lst.Kind is
      when Kind_List | Kind_Iterator =>
         Els := Elements (Lst);
      when Kind_Selector_List =>
         Els := To_List (Lst.Selector_List_Val, Ctx.Pool)
                .List_Val.Elements'Access;
      when others =>
         Raise_Invalid_Type
           (Ctx, L.No_LKQL_Node, "iterable", Lst);
      end case;

      for El of Els.all loop
         Append (Result, El);
      end loop;
      return Result;
   end Eval_To_List;

   ---------------
   -- Eval_Dump --
   ---------------

   function Eval_Dump
     (Ctx : Eval_Context; Args : Primitive_Array) return Primitive
   is
      pragma Unreferenced (Ctx);
   begin
      Ada_AST_Node (Args (1).Node_Val.Unchecked_Get.all).Node.Print;
      return Make_Unit_Primitive;
   end Eval_Dump;

   ----------------
   -- Eval_Image --
   ----------------

   function Eval_Image
     (Ctx : Eval_Context; Args : Primitive_Array) return Primitive
   is
   begin
      return To_Primitive (To_Text (To_Unbounded_Text (Args (1))), Ctx.Pool);
   end Eval_Image;

   -------------------------
   -- Eval_Children_Count --
   -------------------------

   function Eval_Children_Count
     (Ctx : Eval_Context; Args : Primitive_Array) return Primitive
   is
      Node : constant AST_Nodes.AST_Node'Class :=
         Node_Val (Args (1)).Unchecked_Get.all;
   begin
      return To_Primitive
        ((if Node.Is_Null_Node then 0 else Node.Children_Count), Ctx.Pool);
   end Eval_Children_Count;

   ---------------
   -- Eval_Text --
   ---------------

   function Eval_Text
     (Ctx : Eval_Context; Args : Primitive_Array) return Primitive
   is
      Node : constant AST_Nodes.AST_Node'Class :=
         Node_Val (Args (1)).Unchecked_Get.all;
   begin
      return To_Primitive
        ((if Node.Is_Null_Node then "" else Node.Text), Ctx.Pool);
   end Eval_Text;

   -----------------
   -- Starts_With --
   -----------------

   function Eval_Starts_With
     (Ctx : Eval_Context; Args : Primitive_Array) return Primitive
   is
      pragma Unreferenced (Ctx);

      Str    : constant Text_Type := Str_Val (Args (1));
      Prefix : constant Text_Type := Str_Val (Args (2));
      Len    : constant Natural := Prefix'Length;
   begin
      return To_Primitive
         (Str'Length >= Len
          and then Str (Str'First .. Str'First + Len - 1) = Prefix);
   end Eval_Starts_With;

   ---------------
   -- Ends_With --
   ---------------

   function Eval_Ends_With
     (Ctx : Eval_Context; Args : Primitive_Array) return Primitive
   is
      pragma Unreferenced (Ctx);

      Str    : constant Text_Type := Str_Val (Args (1));
      Suffix : constant Text_Type := Str_Val (Args (2));

      Str_Len    : constant Natural := Str'Length;
      Suffix_Len : constant Natural := Suffix'Length;
   begin
      return To_Primitive
         (Str_Len >= Suffix_Len
          and then Str (Str'Last - Suffix_Len + 1 .. Str'Last) = Suffix);
   end Eval_Ends_With;

   ------------------------
   -- Eval_Is_Lower_Case --
   ------------------------

   function Eval_Is_Lower_Case
     (Ctx : Eval_Context; Args : Primitive_Array) return Primitive
   is
      pragma Unreferenced (Ctx);
      use Ada.Wide_Wide_Characters.Handling;

      Str : constant Text_Type := Str_Val (Args (1));
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
      use Ada.Wide_Wide_Characters.Handling;

      Str : constant Text_Type := Str_Val (Args (1));
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
      use Ada.Wide_Wide_Characters.Handling;

      Str : constant Text_Type := Str_Val (Args (1));

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

   -----------------
   -- Eval_Concat --
   -----------------

   function Eval_Concat
     (Ctx : Eval_Context; Args : Primitive_Array) return Primitive
   is
      Res : constant Primitive := Make_Empty_List (Ctx.Pool);
   begin
      for List of List_Val (Args (1)).Elements loop
         for El of List.List_Val.Elements loop
            Res.List_Val.Elements.Append (El);
         end loop;
      end loop;
      return Res;
   end Eval_Concat;

   -------------------
   -- To_Lower_Case --
   -------------------

   function Eval_To_Lower_Case
     (Ctx : Eval_Context; Args : Primitive_Array) return Primitive
   is
      use Ada.Wide_Wide_Characters.Handling;

      Str : constant Text_Type := Str_Val (Args (1));
   begin
      return To_Primitive (To_Lower (Str), Ctx.Pool);
   end Eval_To_Lower_Case;

   -------------------
   -- Eval_Contains --
   -------------------

   function Eval_Contains
     (Ctx : Eval_Context; Args : Primitive_Array) return Primitive
   is
   begin
      return To_Primitive
        (Find_Pattern_Or_String (Ctx, Str_Val (Args (1)), Args (2)) > 0);
   end Eval_Contains;

   ----------------
   -- Eval_Split --
   ----------------

   function Eval_Split
     (Ctx : Eval_Context; Args : Primitive_Array) return Primitive
   is
      Str       : constant Text_Type := Str_Val (Args (1));
      Separator : constant Text_Type := Str_Val (Args (2));
      Ret       : constant Primitive := Make_Empty_List (Ctx.Pool);

      function To_Set (Element : Wide_Wide_String) return Wide_Wide_Character
      is (Element (Element'First));

      function Is_In
        (Item : Wide_Wide_Character; Set : Wide_Wide_Character)
         return Boolean is (Item = Set);

      package String_Split is new GNAT.Array_Split
        (Element          => Wide_Wide_Character,
         Element_Sequence => Wide_Wide_String,
         Element_Set      => Wide_Wide_Character,
         To_Set           => To_Set,
         Is_In            => Is_In);

   begin
      for Word of String_Split.Create (Str, Separator (Separator'First)) loop
         Ret.List_Val.Elements.Append (To_Primitive (Word, Ctx.Pool));
      end loop;

      return Ret;
   end Eval_Split;

   --------------------
   -- Eval_Substring --
   --------------------

   function Eval_Substring
     (Ctx : Eval_Context; Args : Primitive_Array) return Primitive
   is
      Str  : constant Text_Type := Str_Val (Args (1));
      From : constant Integer := Str'First + (+Int_Val (Args (2))) - 1;
      To   : constant Integer := Str'First + (+Int_Val (Args (3))) - 1;
   begin
      return To_Primitive (Text_Type'(Str (From .. To)), Ctx.Pool);
   end Eval_Substring;

   --------------------
   -- Eval_Base_Name --
   --------------------

   function Eval_Base_Name
     (Ctx : Eval_Context; Args : Primitive_Array) return Primitive
   is
      Str  : constant Text_Type := Str_Val (Args (1));
   begin
      return To_Primitive
        (To_Text (Ada.Directories.Simple_Name (Image (Str))),
         Ctx.Pool);
   exception
      when Ada.Directories.Name_Error =>
         return To_Primitive ("", Ctx.Pool);
   end Eval_Base_Name;

   -------------
   -- Get_Doc --
   -------------

   function Get_Doc (Ctx : Eval_Context; Obj : Primitive) return Text_Type is
   begin
      case Kind (Obj) is
         when Kind_Builtin_Function =>
            return To_Text (Obj.Builtin_Fn.Doc);
         when Kind_Function =>
            declare
               Doc_Obj : constant L.Base_String_Literal :=
                 Obj.Fun_Node.P_Doc;
            begin
               return
                 (if Doc_Obj.Is_Null
                  then ""
                  else Str_Val (Eval (Ctx, Doc_Obj)));
            end;
         when Kind_Selector =>
            declare
               Doc : constant L.Base_String_Literal :=
                 Obj.Sel_Node.P_Doc;
            begin
               if not Doc.Is_Null then
                  return Str_Val (Eval (Ctx, Doc));
               end if;
               return "";
            end;
         when Kind_Namespace =>
            if Obj.Module.Children_Count = 0 then
               return "";
            end if;

            declare
               First_Child : constant L.LKQL_Node :=
                 Obj.Module.Child (1);
            begin
               if First_Child.Kind in LCO.LKQL_Base_String_Literal then
                  return Str_Val (Eval (Ctx, First_Child));
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
      return To_Primitive (Get_Doc (Ctx, Args (1)), Ctx.Pool);
   end Eval_Doc;

   ------------------
   -- Eval_Profile --
   ------------------

   function Eval_Profile
     (Ctx : Eval_Context; Args : Primitive_Array) return Primitive
   is
   begin
      return To_Primitive (Profile (Args (1)), Ctx.Pool);
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
      Ret : constant Primitive := Make_Empty_List (Ctx.Pool);
   begin

      if Booleanize (Pkg) then
         Get_Symbols_In_Frame
           (Eval_Contexts.Environment_Access (Pkg.Namespace),
            Recurse => False);
      else
         Get_Symbols_In_Frame (Ctx.Frames, Recurse => True);
      end if;

      for El of S loop
         Ret.List_Val.Elements.Append (To_Primitive (El.all, Ctx.Pool));
      end loop;
      return Ret;

   end Eval_Get_Symbols;

   -----------------------------------
   -- Eval_Get_Builtin_Methods_Info --
   -----------------------------------

   function Eval_Get_Builtin_Methods_Info
     (Ctx : Eval_Context; Args : Primitive_Array) return Primitive
   is
      pragma Unreferenced (Args);
      Ret : constant Primitive := Make_Empty_Object (Ctx.Pool);
   begin
      for Method in Get_Builtin_Methods (Ctx.Kernel).Iterate loop
         declare
            Sub_Obj : constant Primitive := Make_Empty_Object (Ctx.Pool);
            Builtin_Func : constant Builtin_Function :=
               Builtin_Methods_Maps.Element (Method);
         begin
            Ret.Obj_Assocs.Elements.Include
              (Builtin_Methods_Maps.Key (Method).Name,
               Sub_Obj);
            Sub_Obj.Obj_Assocs.Elements.Include
              (Ctx.Symbol ("doc"),
               To_Primitive (To_Text (Builtin_Func.Doc), Ctx.Pool));
            Sub_Obj.Obj_Assocs.Elements.Include
              (Ctx.Symbol ("name"),
               To_Primitive (To_Text (Builtin_Func.Name), Ctx.Pool));

            declare
               Params : constant Primitive := Make_Empty_List (Ctx.Pool);
            begin
               for Param of Builtin_Func.Params loop
                  declare
                     Param_Info : constant Primitive :=
                       Make_Empty_Tuple (Ctx.Pool);
                  begin
                     Param_Info.List_Val.Elements.Append
                       (To_Primitive (To_Text (Param.Name), Ctx.Pool));
                     Param_Info.List_Val.Elements.Append
                       (To_Primitive
                         (Param.Expected_Kind'Wide_Wide_Image, Ctx.Pool));
                     Params.List_Val.Elements.Append (Param_Info);
                  end;
               end loop;

               Sub_Obj.Obj_Assocs.Elements.Include
                 (Ctx.Symbol ("params"), Params);
            end;
         end;
      end loop;
      return Ret;
   end Eval_Get_Builtin_Methods_Info;

   ---------------------
   -- Eval_Token_Next --
   ---------------------

   function Eval_Token_Next
     (Ctx : Eval_Context; Args : Primitive_Array) return Primitive
   is
      Next_Token : constant LKQL.AST_Nodes.AST_Token'Class :=
        Args (1).Token_Val.Unchecked_Get.Next;
   begin
      return To_Primitive (H.Create_Token_Ref (Next_Token), Ctx.Pool);
   end Eval_Token_Next;

   -------------------------
   -- Eval_Token_Previous --
   -------------------------

   function Eval_Token_Previous
     (Ctx : Eval_Context; Args : Primitive_Array) return Primitive
   is
      Previous_Token : constant LKQL.AST_Nodes.AST_Token'Class :=
         Args (1).Token_Val.Unchecked_Get.Previous;
   begin
      return To_Primitive (H.Create_Token_Ref (Previous_Token), Ctx.Pool);
   end Eval_Token_Previous;

   ----------------------
   -- Eval_Unit_Tokens --
   ----------------------

   function Eval_Unit_Tokens
     (Ctx : Eval_Context; Args : Primitive_Array) return Primitive
   is
      Tokens     : Primitive_Vectors.Vector;
      Unit       : constant H.AST_Unit_Holder :=
        Args (1).Analysis_Unit_Val;
      Token      : H.AST_Token_Holder :=
        H.Create_Token_Ref (Unit.Unchecked_Get.Token_Start);
      Last_Token : constant H.AST_Token_Holder :=
        H.Create_Token_Ref (Unit.Unchecked_Get.Token_End);
      use LKQL.AST_Nodes;
   begin
      while Token.Unchecked_Get.all /= Last_Token.Unchecked_Get.all loop
         Tokens.Append (To_Primitive (Token, Ctx.Pool));
         Token := H.Create_Token_Ref (Token.Unchecked_Get.Next);
      end loop;

      declare
         Iter : Primitive_Vec_Iters.Vec_Iterator :=
           Primitive_Vec_Iters.To_Iterator (Tokens);
         Ret : constant Primitive := To_Primitive (Iter, Ctx.Pool);
      begin
         Iter.Release;
         return Ret;
      end;

   end Eval_Unit_Tokens;

   -----------------
   -- Eval_Tokens --
   -----------------

   function Eval_Tokens
     (Ctx : Eval_Context; Args : Primitive_Array) return Primitive
   is
      Tokens     : Primitive_Vectors.Vector;
      Node       : constant H.AST_Node_Holder := Args (1).Node_Val;
      Token      : H.AST_Token_Holder :=
        H.Create_Token_Ref (Node.Unchecked_Get.Token_Start);
      Last_Token : constant H.AST_Token_Holder :=
        H.Create_Token_Ref (Node.Unchecked_Get.Token_End);

      use LKQL.AST_Nodes;
   begin
      while Token.Unchecked_Get.all /= Last_Token.Unchecked_Get.all loop
         Tokens.Append (To_Primitive (Token, Ctx.Pool));
         Token := H.Create_Token_Ref (Token.Unchecked_Get.Next);
      end loop;

      return To_Primitive
        (Primitive_Iter (Primitive_Vec_Iters.To_Iterator (Tokens)), Ctx.Pool);
   end Eval_Tokens;

   ----------------
   -- Eval_Units --
   ----------------

   function Eval_Units
     (Ctx : Eval_Context; Args : Primitive_Array) return Primitive
   is
      pragma Unreferenced (Args);
      Units : Primitive_Vectors.Vector;
   begin
      for Root of Ctx.AST_Roots.all loop
         Units.Append
           (To_Primitive
              (H.Create_Unit_Ref (Root.Unchecked_Get.Unit), Ctx.Pool));
      end loop;

      return To_Primitive
        (Primitive_Iter (Primitive_Vec_Iters.To_Iterator (Units)), Ctx.Pool);
   end Eval_Units;

   ---------------------
   -- Eval_Token_Text --
   ---------------------

   function Eval_Token_Text
     (Ctx : Eval_Context; Args : Primitive_Array) return Primitive
   is
   begin
      return To_Primitive (Args (1).Token_Val.Unchecked_Get.Text, Ctx.Pool);
   end Eval_Token_Text;

   ---------------------
   -- Eval_Token_Kind --
   ---------------------

   function Eval_Token_Kind
     (Ctx : Eval_Context; Args : Primitive_Array) return Primitive
   is
   begin
      return To_Primitive (Args (1).Token_Val.Unchecked_Get.Kind, Ctx.Pool);
   end Eval_Token_Kind;

   ---------------------
   -- Eval_Start_Line --
   ---------------------

   function Eval_Start_Line
     (Ctx : Eval_Context; Args : Primitive_Array) return Primitive
   is
      (To_Primitive
         (Integer (Args (1)
            .Token_Val.Unchecked_Get.Sloc_Range.Start_Line), Ctx.Pool));

   ---------------------
   -- Eval_Token_Unit --
   ---------------------

   function Eval_Token_Unit
     (Ctx : Eval_Context; Args : Primitive_Array) return Primitive
   is
      (To_Primitive
        (H.Create_Unit_Ref (Args (1).Token_Val.Unchecked_Get.Unit),
         Ctx.Pool));

   -------------------
   -- Eval_End_Line --
   -------------------

   function Eval_End_Line
     (Ctx : Eval_Context; Args : Primitive_Array) return Primitive
   is
      (To_Primitive
         (Integer (Args (1)
            .Token_Val.Unchecked_Get.Sloc_Range.End_Line), Ctx.Pool));

   --------------------
   -- Eval_Start_Col --
   --------------------

   function Eval_Start_Col
     (Ctx : Eval_Context; Args : Primitive_Array) return Primitive
   is
      (To_Primitive
         (Integer (Args (1)
            .Token_Val.Unchecked_Get.Sloc_Range.Start_Column), Ctx.Pool));

   ------------------
   -- Eval_End_Col --
   ------------------

   function Eval_End_Col
     (Ctx : Eval_Context; Args : Primitive_Array) return Primitive
   is
      (To_Primitive
         (Integer (Args (1)
            .Token_Val.Unchecked_Get.Sloc_Range.End_Column), Ctx.Pool));

   --------------------
   -- Eval_Unit_Text --
   --------------------

   function Eval_Unit_Text
     (Ctx : Eval_Context; Args : Primitive_Array) return Primitive
   is
     (To_Primitive
       (Args (1).Analysis_Unit_Val.Unchecked_Get.Text, Ctx.Pool));

   --------------------
   -- Eval_Unit_Root --
   --------------------

   function Eval_Unit_Name
     (Ctx : Eval_Context; Args : Primitive_Array) return Primitive
   is
     (To_Primitive
       (Args (1).Analysis_Unit_Val.Unchecked_Get.Name, Ctx.Pool));

   --------------------
   -- Eval_Unit_Root --
   --------------------

   function Eval_Unit_Root
     (Ctx : Eval_Context; Args : Primitive_Array) return Primitive
   is
     (To_Primitive
       (H.Create_Node (Args (1)
        .Analysis_Unit_Val.Unchecked_Get.Root), Ctx.Pool));

   --------------------
   -- Eval_Reduce --
   --------------------

   function Eval_Reduce
     (Ctx : Eval_Context; Args : Primitive_Array) return Primitive;

   function Eval_Reduce
     (Ctx : Eval_Context; Args : Primitive_Array) return Primitive
   is
      List     : constant Primitive_Vector_Access := Elements (Args (1));
      Fn       : constant Primitive := Args (2);
      Init_Val : constant Primitive := Args (3);
   begin
      case Fn.Kind is
      when Kind_Function =>
         declare
            D        : L.Base_Function;
            First_Arg_Name, Second_Arg_Name : Symbol_Type;
            Env : constant LKQL.Primitives.Environment_Access :=
              Fn.Frame;
            Eval_Ctx : constant Eval_Context :=
              Eval_Context'
                (Ctx.Kernel, Eval_Contexts.Environment_Access (Env));

            function Eval_Call
              (Accumulator_Value : Primitive;
               Current_Value     : Primitive) return Primitive;

            function Eval_Call
              (Accumulator_Value : Primitive;
               Current_Value     : Primitive) return Primitive
            is
               Args_Bindings : Environment_Map;
            begin
               Args_Bindings.Insert (First_Arg_Name, Accumulator_Value);
               Args_Bindings.Insert (Second_Arg_Name, Current_Value);
               return Eval
                 (Eval_Ctx, D.F_Body_Expr, Local_Bindings => Args_Bindings);
            end Eval_Call;

         begin
            D := Fn.Fun_Node;

            if D.F_Parameters.Children_Count /= 2 then
               Raise_And_Record_Error
                 (Ctx,
                  Make_Eval_Error
                    (L.No_LKQL_Node,
                     "Function passed to reduce should have arity of two"));
            end if;

            First_Arg_Name :=
              Symbol (D.F_Parameters.Child (1).As_Parameter_Decl.P_Identifier);

            Second_Arg_Name :=
              Symbol (D.F_Parameters.Child (2).As_Parameter_Decl.P_Identifier);

            declare
               Accum : Primitive := Init_Val;
            begin
               for El of List.all loop
                  Accum := Eval_Call (Accum, El);
               end loop;
               return Accum;
            end;
         end;

      when others =>
         Raise_Invalid_Type
           (Ctx, L.No_LKQL_Node, "function", Fn);
      end case;
   end Eval_Reduce;

   package Primitive_Sets is new Ada.Containers.Hashed_Sets
    (Primitive, Hash, Equals, Equals);

   -----------------
   -- Eval_Unique --
   -----------------

   function Eval_Unique
     (Ctx : Eval_Context;
      Args : Primitive_Array) return Primitive;

   function Eval_Unique
     (Ctx : Eval_Context;
      Args : Primitive_Array) return Primitive
   is
      S    : Primitive_Sets.Set;
      List : constant Primitive_Vector_Access := Elements (Args (1));
   begin
      for El of List.all loop
         S.Include (El);
      end loop;

      declare
         Ret : constant Primitive :=
           LKQL.Primitives.Make_Empty_List (Ctx.Pool);
      begin
         for El of S loop
            Ret.List_Val.Elements.Append (El);
         end loop;
         return Ret;
      end;
   end Eval_Unique;

   -------------------------
   -- Eval_Create_Pattern --
   -------------------------

   function Eval_Create_Pattern
     (Ctx  : Eval_Context;
      Args : Primitive_Array) return Primitive
   is
     (Make_Regex
        (GNAT.Regpat.Compile (To_String (Str_Val (Args (1)))), Ctx.Pool));

   ----------------
   -- Eval_Find --
   ----------------

   function Eval_Find
     (Ctx  : Eval_Context;
      Args : Primitive_Array) return Primitive
   is
   begin
      return To_Primitive
        (Find_Pattern_Or_String (Ctx, Str_Val (Args (1)), Args (2)), Ctx.Pool);
   end Eval_Find;

   function Find_Pattern_Or_String
     (Ctx : Eval_Context;
      Str : Text_Type;
      To_Find : Primitive) return Natural
   is
      use Ada.Strings.Wide_Wide_Fixed;
   begin
      if To_Find.Kind = Kind_Regex then
         return GNAT.Regpat.Match (To_Find.Regex_Val.all, Image (Str));
      elsif To_Find.Kind = Kind_Str then
         return Index (Str, Str_Val (To_Find));
      else
         Raise_Invalid_Type
           (Ctx, L.No_LKQL_Node, "string or pattern", To_Find);
      end if;
   end Find_Pattern_Or_String;

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
        ("reduce",
        (Param ("indexable"), Param ("fn"), Param ("init")),
         Eval_Reduce'Access,
         "Given a collection, a reduction function, and an initial value" &
         " reduce the result"),

      Create
        ("unique",
        (1 => Param ("indexable")),
         Eval_Unique'Access,
         ""),

      Create
        ("img",
         (1 => Param ("val")),
         Eval_Image'Access,
         "Return a string representation of an object"),

      Create
        ("dump",
         (1 => Param ("node", Kind_Node)),
         Eval_Dump'Access,
         "Given an ast node, return a structured dump of the subtree",
         Only_Dot_Calls => True),

      Create
        ("text",
         (1 => Param ("node", Kind_Node)),
         Eval_Text'Access,
         "Given an ast node, return its text",
         Only_Dot_Calls => True),

      Create
        ("to_list",
         (1 => Param ("it")),
         Eval_To_List'Access,
         "Transform an iterator into a list",
         Only_Dot_Calls => True),

      Create
        ("children_count",
         (1 => Param ("node", Kind_Node)),
         Eval_Children_Count'Access,
         "Given a node, return the count of its children",
         Only_Dot_Calls => True),

      Create
        ("tokens",
         (1 => Param ("node", Kind_Node)),
         Eval_Tokens'Access,
         "Given a node, return an iterator on its tokens",
         Only_Dot_Calls => True),

      --  Token builtins

      Create
        ("text",
         (1 => Param ("token", Kind_Token)),
         Eval_Token_Text'Access,
         "Return the text for this token",
         Only_Dot_Calls => True),

      Create
        ("kind",
         (1 => Param ("token", Kind_Token)),
         Eval_Token_Kind'Access,
         "Return the kind for this token, as a string",
         Only_Dot_Calls => True),

      Create
        ("next",
         (1 => Param ("token", Kind_Token)),
         Eval_Token_Next'Access,
         "Return the next token",
         Only_Dot_Calls => True),

      Create
        ("previous",
         (1 => Param ("token", Kind_Token)),
         Eval_Token_Previous'Access,
         "Return the previous token",
         Only_Dot_Calls => True),

      Create
        ("start_column",
         (1 => Param ("token", Kind_Token)),
         Eval_Start_Col'Access,
         "Return the column start",
         Only_Dot_Calls => True),

      Create
        ("end_column",
         (1 => Param ("token", Kind_Token)),
         Eval_End_Col'Access,
         "Return the column end",
         Only_Dot_Calls => True),

      Create
        ("start_line",
         (1 => Param ("token", Kind_Token)),
         Eval_Start_Line'Access,
         "Return the line start",
         Only_Dot_Calls => True),

      Create
        ("end_line",
         (1 => Param ("token", Kind_Token)),
         Eval_End_Line'Access,
         "Return the line end",
         Only_Dot_Calls => True),

      Create
        ("unit",
         (1 => Param  ("token", Kind_Token)),
         Eval_Token_Unit'Access,
         "Return the unit for this token",
         Only_Dot_Calls => True),

      --  Unit builtins

      Create
        ("text",
         (1 => Param ("unit", Kind_Analysis_Unit)),
         Eval_Unit_Text'Access,
         "Return the text for the whole unit",
         Only_Dot_Calls => True),

      Create
        ("root",
         (1 => Param ("unit", Kind_Analysis_Unit)),
         Eval_Unit_Root'Access,
         "Return the root for this unit",
         Only_Dot_Calls => True),

      Create
        ("name",
         (1 => Param ("unit", Kind_Analysis_Unit)),
         Eval_Unit_Name'Access,
         "Return the name of this unit",
         Only_Dot_Calls => True),

      Create
        ("tokens",
         (1 => Param ("unit", Kind_Analysis_Unit)),
         Eval_Unit_Tokens'Access,
         "Given an unit, return an iterator on its tokens",
         Only_Dot_Calls => True),

      Create
        ("pattern",
         (1 => Param ("string_pattern", Kind_Str)),
        Eval_Create_Pattern'Access,
        "Given a regex pattern string, create a pattern object",
        Only_Dot_Calls => False),

      Create
        ("find",
         (1 => Param ("string", Kind_Str),
          2 => Param ("to_find")),
          Eval_Find'Access,
          "Search for `to_find` in the given string. "
          & "Return position of the match, or -1 if no match. "
          & "``to_find`` can be either a pattern or a string",
         Only_Dot_Calls => True),

      --  String builtins

      Create
        ("starts_with",
         (Param ("str", Kind_Str), Param ("prefix", Kind_Str)),
         Eval_Starts_With'Access,
         "Given a string, returns whether it starts with the given prefix",
         Only_Dot_Calls => True),

      Create
        ("ends_with",
         (Param ("str", Kind_Str), Param ("suffix", Kind_Str)),
         Eval_Ends_With'Access,
         "Given a string, returns whether it ends with the given suffix",
         Only_Dot_Calls => True),

      Create
        ("is_lower_case",
         (1 => Param ("str", Kind_Str)),
         Eval_Is_Lower_Case'Access,
         "Return whether the given string contains lower case characters "
         & "only",
         Only_Dot_Calls => True),

      Create
        ("is_upper_case",
         (1 => Param ("str", Kind_Str)),
         Eval_Is_Upper_Case'Access,
         "Return whether the given string contains upper case characters "
         & "only",
         Only_Dot_Calls => True),

      Create
        ("is_mixed_case",
         (1 => Param ("str", Kind_Str)),
         Eval_Is_Mixed_Case'Access,
         "Return whether the given string is written in mixed case, that is, "
         & "with only lower case characters except the first one and every "
         & "character following an underscore",
         Only_Dot_Calls => True),

      Create
        ("to_lower_case",
         (1 => Param ("str", Kind_Str)),
         Eval_To_Lower_Case'Access,
         "Return the given string written with lower case characters only",
         Only_Dot_Calls => True),

      Create
        ("concat",
         (1 => Param ("lists", Kind_List)),
         Eval_Concat'Access,
         "Given a list of lists, return a concatenated list"),

      Create
        ("contains",
         (Param ("str", Kind_Str), Param ("substr")),
         Eval_Contains'Access,
          "Search for `to_find` in the given string. "
          & "Return whether a match is found. "
          & "``to_find`` can be either a pattern or a string",
         Only_Dot_Calls => True),

      Create
        ("split",
         (Param ("str", Kind_Str), Param ("separator", Kind_Str)),
         Eval_Split'Access,
         "Given a string, return an iterator on the words contained by "
         & "str separated by separator",
         Only_Dot_Calls => True),

      Create
        ("substring",
         (Param ("str", Kind_Str),
          Param ("from", Kind_Int),
          Param ("to", Kind_Int)),
         Eval_Substring'Access,
         "Given a string and two indices (from and to), return the substring "
         & "contained between indices from and to (both included)",
         Only_Dot_Calls => True),

      Create
        ("base_name",
         (1 => Param ("str", Kind_Str)),
         Eval_Base_Name'Access,
         "Given a string that represents a file name, returns the basename"),

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
        ("units",
         Empty_Profile,
         Eval_Units'Access,
         "Return an iterator on all units"),

      Create
        ("get_builtin_methods_info",
         Empty_Profile,
         Eval_Get_Builtin_Methods_Info'Access,
         "Return information about builtin methods"),

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

   type Free_Builtins is new Ada.Finalization.Controlled with record
      Freed : Boolean := False;
   end record;

   overriding procedure Finalize (Self : in out Free_Builtins);

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Self : in out Free_Builtins) is
      procedure Free_Builtin_Fun is new Ada.Unchecked_Deallocation
        (Builtin_Function_Description, Builtin_Function);
   begin
      if not Self.Freed then
         for Fun of Builtin_Functions loop
            declare
               F : Builtin_Function := Fun;
            begin
               Free_Builtin_Fun (F);
            end;
         end loop;
         Self.Freed := True;
      end if;
   end Finalize;

   Free_Builtins_Singleton : aliased Free_Builtins;
   pragma Unreferenced (Free_Builtins_Singleton);

end LKQL.Builtin_Functions;

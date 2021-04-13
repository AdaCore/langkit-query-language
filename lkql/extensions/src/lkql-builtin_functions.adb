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

with Langkit_Support.Text; use Langkit_Support.Text;

with LKQL.AST_Nodes;

with Ada_AST_Nodes; use Ada_AST_Nodes;

package body LKQL.Builtin_Functions is
   function Create
     (Name      : Text_Type;
      Params    : Builtin_Function_Profile;
      Fn_Access : Native_Function_Access) return Builtin_Function;
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
      Fn_Access : Native_Function_Access) return Builtin_Function
   is
   begin
      return new Builtin_Function_Description'
        (N         => Params'Length,
         Name      => To_Unbounded_Text (Name),
         Params    => Params,
         Fn_Access => Fn_Access);
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

   -----------------------
   -- Builtin_Functions --
   -----------------------

   Builtin_Functions : constant Builtin_Function_Array :=
     (Create ("print",
              (Param ("val"),
               Param ("new_line", Kind_Bool, To_Primitive (True))),
              Eval_Print'Access),
      Create ("img",   (1 => Param ("val")),             Eval_Image'Access),
      Create ("dump",  (1 => Param ("node", Kind_Node)), Eval_Dump'Access),
      Create ("text",  (1 => Param ("node", Kind_Node)), Eval_Text'Access),
      Create ("to_list",
              (1 => Param ("it", Kind_Iterator)),
              Eval_To_List'Access),
      Create ("children_count",
              (1 => Param ("node", Kind_Node)),
              Eval_Children_Count'Access));

   ------------------
   -- All_Builtins --
   ------------------

   function All_Builtins return Builtin_Function_Array is
     (Builtin_Functions);

end LKQL.Builtin_Functions;

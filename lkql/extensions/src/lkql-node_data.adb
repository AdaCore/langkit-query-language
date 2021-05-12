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

with LKQL.Errors;         use LKQL.Errors;
with LKQL.Evaluation;     use LKQL.Evaluation;
with LKQL.Error_Handling; use LKQL.Error_Handling;

with Ada.Exceptions; use Ada.Exceptions;
with Langkit_Support.Errors;

with Langkit_Support.Text; use Langkit_Support.Text;

package body LKQL.Node_Data is

   -----------------------
   -- Access_Node_Field --
   -----------------------

   function Access_Node_Field (Ctx        : Eval_Context;
                               Receiver   : H.AST_Node_Holder;
                               Field_Name : L.Identifier) return Primitive
   is
      Result : Primitive;

      Real_Name : constant Text_Type := Field_Name.Text;
   begin
      if Real_Name = "image" then
         return To_Primitive (Receiver.Unchecked_Get.Text_Image);
      end if;

      if Receiver.Unchecked_Get.Is_Field_Name (Real_Name) then

         Result := Receiver.Unchecked_Get.Access_Field
           (Receiver.Unchecked_Get.Get_Member_Reference (Real_Name), Ctx);

         return Result;

      elsif Receiver.Unchecked_Get.Is_Property_Name (Real_Name) then
         return Make_Property_Reference
           (To_Primitive (Receiver),
            H.Create_Member_Ref
              (Receiver.Unchecked_Get.Get_Member_Reference (Real_Name)));
      else
         Raise_No_Such_Field (Ctx, Receiver, Field_Name);
      end if;

   exception
      when Error : Introspection_Error =>
         Raise_And_Record_Error
           (Ctx, Make_Eval_Error (Field_Name,
                                  To_Text (Exception_Message (Error))));
   end Access_Node_Field;

   ------------------------
   -- Eval_Node_Property --
   ------------------------

   function Eval_Node_Property (Ctx           : Eval_Context;
                                Receiver      : H.AST_Node_Holder;
                                Property_Name : L.Identifier;
                                Args          : L.Arg_List) return Primitive
   is
      Real_Name     : constant Text_Type := Property_Name.Text;
   begin
      if not Receiver.Unchecked_Get.Is_Property_Name (Real_Name) then
         Raise_No_Such_Property (Ctx, Receiver, Property_Name);
      end if;

      declare
         Ref : constant AST_Node_Member_Reference'Class :=
           Receiver.Unchecked_Get.Get_Member_Reference (Real_Name);
      begin
         return Eval_Node_Property
           (Ctx, Receiver.Unchecked_Get.all, Ref, Args);
      end;
   end Eval_Node_Property;

   ------------------------
   -- Eval_Node_Property --
   ------------------------

   function Eval_Node_Property
     (Ctx           : Eval_Context;
      Receiver      : AST_Node'Class;
      Property_Ref  : AST_Node_Member_Reference'Class;
      Args          : L.Arg_List) return Primitive
   is

      function Primitive_List_From_Args
        (Ctx           : Eval_Context;
         Ref           : AST_Node_Member_Reference'Class;
         Args          : L.Arg_List)
         return Primitive_List;
      --  Evaluate the given arguments and convert them to Value_Type values.

      ----------------------------------------
      -- Introspection_Value_Array_From_Arg --
      ----------------------------------------

      function Primitive_List_From_Args
        (Ctx           : Eval_Context;
         Ref           : AST_Node_Member_Reference'Class;
         Args          : L.Arg_List)
         return Primitive_List
      is
         Arity  : constant Natural := Ref.Property_Arity;
         Result : Primitive_List;
      begin
         for I in 1 .. Arity loop
            Result.Elements.Append
              (if I <= Args.Children_Count
               then Eval (Ctx, Args.List_Child (I).P_Expr)
               else Ref.Default_Arg_Value (I, Ctx));
         end loop;

         return Result;
      end Primitive_List_From_Args;

      Result        : Primitive;
      Property_Args : constant Primitive_List :=
        Primitive_List_From_Args
          (Ctx, Property_Ref, Args);

   begin
      if Args.Children_Count > Property_Ref.Property_Arity then
         Raise_Invalid_Arity (Ctx, Property_Ref.Property_Arity, Args);
      end if;

      Result := Property_Ref.Evaluate_Property (Receiver, Property_Args, Ctx);

      return Result;
   exception
      when Error : Introspection_Error =>
         Raise_And_Record_Error
           (Ctx, Make_Eval_Error (Args.Parent,
            To_Text (Exception_Message (Error))));
      when Error : Langkit_Support.Errors.Property_Error =>

         --  Wrap property errors in regular LKQL eval errors.
         Raise_And_Record_Error
           (Ctx,
            Make_Eval_Error
              (Args.Parent,
               To_Text ("PROPERTY_ERROR:" & Exception_Message (Error)),
               --  We embed the property error information in the eval error
               --  data.
               Property_Error_Info => Save_Occurrence (Error)));
   end Eval_Node_Property;

end LKQL.Node_Data;

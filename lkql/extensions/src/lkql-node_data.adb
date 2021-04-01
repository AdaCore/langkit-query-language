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

with Langkit_Support.Text; use Langkit_Support.Text;

package body LKQL.Node_Data is

   function To_Primitive (Value : in out Introspection_Value) return Primitive;
   --  Create a Primitive value from the given Introspection value and
   --  deallocate the introspection value's memory.

   function To_Primitive (Value : AST_Node_Array_Access) return Primitive;
   --  Create a Primitive value from the given node array

   function To_Primitive
     (Value : Unbounded_Text_Array_Access) return Primitive;
   --  Create a Primitive value from the given text array

   ------------------
   -- To_Primitive --
   ------------------

   function To_Primitive (Value : in out Introspection_Value) return Primitive
   is
      Result : constant Primitive :=
        (case Value.Kind is
            when Kind_Node =>
              To_Primitive
                 (Make_AST_Node_Rc (Value.Node_Val), Nullable => True),
            when Kind_Node_Array =>
              To_Primitive (Value.Node_Array_Val),
            when Kind_Bool =>
              To_Primitive (Value.Bool_Val),
            when Kind_Int =>
              To_Primitive (Value.Int_Val),
            when Kind_Text =>
              To_Primitive (Value.Text_Val),
            when Kind_Text_Array =>
              To_Primitive (Value.Text_Array_Val),
            when Kind_Empty_List =>
              Make_Empty_List);
   begin
      Release_Introspection_Value (Value);
      return Result;
   end To_Primitive;

   ------------------
   -- To_Primitive --
   ------------------

   function To_Primitive (Value : AST_Node_Array_Access) return Primitive is
   begin
      return Result : constant Primitive := Make_Empty_List do
         for V of Value.all loop
            Append (Result, To_Primitive (Make_AST_Node_Rc (V)));
         end loop;
      end return;
   end To_Primitive;

   ------------------
   -- To_Primitive --
   ------------------

   function To_Primitive
     (Value : Unbounded_Text_Array_Access) return Primitive
   is
   begin
      return Result : constant Primitive := Make_Empty_List do
         for V of Value.all loop
            Append (Result, To_Primitive (V));
         end loop;
      end return;
   end To_Primitive;

   -----------------------
   -- Access_Node_Field --
   -----------------------

   function Access_Node_Field (Ctx        : Eval_Context;
                               Receiver   : AST_Node_Rc;
                               Field_Name : L.Identifier) return Primitive
   is
      Result : Introspection_Value;

      Real_Name : constant Text_Type := Field_Name.Text;
   begin
      if Real_Name = "image" then
         return To_Primitive (Receiver.Get.Text_Image);
      elsif Real_Name = "text" then
         return To_Primitive (Receiver.Get.Text);
      end if;

      if Receiver.Get.Is_Field_Name (Real_Name) then

         Result := Receiver.Get.Access_Field
           (Receiver.Get.Get_Member_Reference (Real_Name));

         return To_Primitive (Result);

      elsif Receiver.Get.Is_Property_Name (Real_Name) then
         return Make_Property_Reference
           (To_Primitive (Receiver),
           Receiver.Unchecked_Get.Get_Member_Reference (Real_Name));
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
                                Receiver      : AST_Node_Rc;
                                Property_Name : L.Identifier;
                                Args          : L.Arg_List) return Primitive
   is
      Real_Name     : constant Text_Type := Property_Name.Text;
   begin
      if not Receiver.Get.Is_Property_Name (Real_Name) then
         Raise_No_Such_Property (Ctx, Receiver, Property_Name);
      end if;

      declare
         Ref : constant AST_Node_Member_Reference'Class :=
           Receiver.Get.Get_Member_Reference (Real_Name);
      begin
         return Eval_Node_Property (Ctx, Receiver, Ref, Args);
      end;
   end Eval_Node_Property;

   ------------------------
   -- Eval_Node_Property --
   ------------------------

   function Eval_Node_Property
     (Ctx           : Eval_Context;
      Receiver      : AST_Node_Rc;
      Property_Ref  : AST_Node_Member_Reference'Class;
      Args          : L.Arg_List) return Primitive
   is

      function Introspection_Value_Array_From_Args
        (Ctx           : Eval_Context;
         Ref           : AST_Node_Member_Reference'Class;
         Args          : L.Arg_List)
         return Introspection_Value_Array;
      --  Evaluate the given arguments and convert them to Value_Type values.

      ----------------------------------------
      -- Introspection_Value_Array_From_Arg --
      ----------------------------------------

      function Introspection_Value_Array_From_Args
        (Ctx           : Eval_Context;
         Ref           : AST_Node_Member_Reference'Class;
         Args          : L.Arg_List)
         return Introspection_Value_Array
      is
         Arity  : constant Natural := Ref.Property_Arity;
         Result : Introspection_Value_Array (1 .. Arity);
      begin
         for I in 1 .. Arity loop
            Result (I) :=
              (if I <= Args.Children_Count
               then To_Introspection_Value
                 (Eval (Ctx, Args.List_Child (I).P_Expr))
               else Ref.Default_Arg_Value (I));
         end loop;

         return Result;
      end Introspection_Value_Array_From_Args;

      Result        : Introspection_Value;
         Property_Args : constant Introspection_Value_Array :=
           Introspection_Value_Array_From_Args
             (Ctx, Property_Ref, Args);

   begin
      if Args.Children_Count > Property_Ref.Property_Arity then
         Raise_Invalid_Arity (Ctx, Property_Ref.Property_Arity, Args);
      end if;

      Result := Property_Ref.Evaluate_Property (Receiver.Get, Property_Args);

      return To_Primitive (Result);
   exception
      when Error : Introspection_Error =>
         Raise_And_Record_Error
           (Ctx, Make_Eval_Error (Args.Parent,
            To_Text (Exception_Message (Error))));
   end Eval_Node_Property;

end LKQL.Node_Data;

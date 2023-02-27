------------------------------------------------------------------------------
--                                                                          --
--                                   LKQL                                   --
--                                                                          --
--                     Copyright (C) 2019-2023, AdaCore                     --
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

with LKQL.Adaptive_Integers; use LKQL.Adaptive_Integers;
with LKQL.Errors;            use LKQL.Errors;
with LKQL.Evaluation;        use LKQL.Evaluation;
with LKQL.Error_Handling;    use LKQL.Error_Handling;

with Ada.Exceptions; use Ada.Exceptions;
with Langkit_Support.Errors;

with Langkit_Support.Text; use Langkit_Support.Text;

with GNATCOLL.GMP.Integers;

package body LKQL.Node_Data is

   function Make_Primitive
     (Ctx   : Eval_Context;
      Value : LKI.Value_Ref) return Primitive;

   function Make_Value_Type
     (Value       : Primitive;
      Target_Type : LKI.Type_Ref;
      Ctx         : Eval_Context) return LKI.Value_Ref;

   function List_To_Value_Ref
     (Value        : Primitive_List;
      Array_Type   : LKI.Type_Ref;
      Ctx          : Eval_Context) return LKI.Value_Ref;

   ---------------------------
   -- Get_Struct_Member_Ref --
   ---------------------------

   function Get_Struct_Member_Ref
     (Ctx        : Eval_Context;
      Receiver   : LK.Lk_Node;
      Field_Name : L.Identifier) return LKI.Struct_Member_Ref
   is
      T  : constant LKI.Type_Ref := LKI.Type_Of (Receiver);
   begin
      return Ctx.Get_Name_Map.Lookup_Struct_Member
        (T, Symbol (Field_Name));
   end Get_Struct_Member_Ref;

   -----------------------
   -- Access_Node_Field --
   -----------------------

   function Access_Node_Field (Ctx        : Eval_Context;
                               Receiver   : LK.Lk_Node;
                               Field_Name : L.Identifier) return Primitive
   is
      Ref    : constant LKI.Struct_Member_Ref :=
        Get_Struct_Member_Ref (Ctx, Receiver, Field_Name);

      use LKI;
   begin
      if Ref = LKI.No_Struct_Member_Ref then
         Raise_And_Record_Error
           (Ctx, Make_Eval_Error (Field_Name, "No such field"));
      elsif LKI.Is_Property (Ref) then

         --  TODO: This is a special case for some properties, but that's ugly
         --  as hell.
         if LKI.Member_Last_Argument (Ref) = No_Argument_Index then
            declare
               S : constant Symbol_Type := Symbol (Field_Name);
            begin
               if S.all in "children" | "unit" | "parent" then
                  return Make_Primitive
                    (Ctx, LKI.Eval_Node_Member (Receiver, Ref));
               end if;
            end;
         end if;

         return Make_Property_Reference (Receiver, Ref, Ctx.Pool);
      end if;

      return Make_Primitive (Ctx, LKI.Eval_Node_Member (Receiver, Ref));
   exception
      when Error : LKE.Precondition_Failure =>
         Raise_And_Record_Error
           (Ctx, Make_Eval_Error (Field_Name,
                                  To_Text (Exception_Message (Error))));
   end Access_Node_Field;

   ------------------------
   -- Eval_Node_Property --
   ------------------------

   function Eval_Node_Property (Ctx           : Eval_Context;
                                Receiver      : LK.Lk_Node;
                                Property_Name : L.Identifier;
                                Args          : L.Arg_List) return Primitive
   is

      Ref    : constant LKI.Struct_Member_Ref :=
        Get_Struct_Member_Ref (Ctx, Receiver, Property_Name);

      use LKI;
   begin
      if Ref = LKI.No_Struct_Member_Ref then
         Raise_And_Record_Error
           (Ctx, Make_Eval_Error (Property_Name, "No such field"));
      end if;

      return Eval_Node_Property (Ctx, Receiver, Ref, Args);
   exception
      when Error : LKE.Precondition_Failure =>
         Raise_And_Record_Error
           (Ctx, Make_Eval_Error (Property_Name,
                                  To_Text (Exception_Message (Error))));

   end Eval_Node_Property;

   ------------------------
   -- Eval_Node_Property --
   ------------------------

   function Eval_Node_Property
     (Ctx           : Eval_Context;
      Receiver      : LK.Lk_Node;
      Property_Ref  : LKI.Struct_Member_Ref;
      Args          : L.Arg_List) return Primitive
   is

      Arity  : constant Natural
        := Natural (LKI.Member_Last_Argument (Property_Ref));

      function Value_Ref_Array_From_Args
        (Ctx           : Eval_Context;
         Ref           : LKI.Struct_Member_Ref;
         Args          : L.Arg_List)
         return LKI.Value_Ref_Array;
      --  Evaluate the given arguments and convert them to Value_Type values.

      -------------------------------
      -- Value_Ref_Array_From_Args --
      -------------------------------

      function Value_Ref_Array_From_Args
        (Ctx           : Eval_Context;
         Ref           : LKI.Struct_Member_Ref;
         Args          : L.Arg_List) return LKI.Value_Ref_Array
      is
         Result : LKI.Value_Ref_Array (1 .. Arity);
         use LKI;
      begin
         for I in 1 .. Arity loop
            declare
               Val : constant LKI.Value_Ref :=
                 (if I <= Args.Children_Count
                  then Make_Value_Type
                    (Eval (Ctx, Args.List_Child (I).P_Expr),
                     Member_Argument_Type (Ref, Argument_Index (I)),
                     Ctx)
                  else Member_Argument_Default_Value
                    (Ref, Argument_Index (I)));
            begin
               if Val = No_Value_Ref then
                  Raise_Invalid_Arity (Ctx, Arity, Args);
               else
                  Result (I) := Val;
               end if;
            end;
         end loop;

         return Result;
      end Value_Ref_Array_From_Args;

      Result        : Primitive;
      Property_Args : constant LKI.Value_Ref_Array :=
        Value_Ref_Array_From_Args
          (Ctx, Property_Ref, Args);

   begin
      if Args.Children_Count > Arity then
         Raise_Invalid_Arity
           (Ctx, Natural (LKI.Member_Last_Argument (Property_Ref)), Args);
      end if;

      Result := Make_Primitive
        (Ctx, LKI.Eval_Node_Member (Receiver, Property_Ref, Property_Args));

      return Result;
   exception
      when Error : LKE.Precondition_Failure =>
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

   --------------------
   -- Make_Primitive --
   --------------------

   function Make_Primitive
     (Ctx   : Eval_Context; Value : LKI.Value_Ref) return Primitive
   is
      T : constant LKI.Type_Ref := LKI.Type_Of (Value);
      use LKI;
   begin
      case LKI.Category (T) is
         when Analysis_Unit_Category =>
            return To_Primitive (LKI.As_Unit (Value), Ctx.Pool);
         when Big_Int_Category =>
            return To_Primitive
              (Adaptive_Integers.Create
                 (GNATCOLL.GMP.Integers.Image (LKI.As_Big_Int (Value))),
               Ctx.Pool);
         when Bool_Category =>
            return To_Primitive (LKI.As_Bool (Value));
         when Int_Category =>
            return To_Primitive (LKI.As_Int (Value), Ctx.Pool);
         when String_Category =>
            return To_Primitive (LKI.As_String (Value), Ctx.Pool);
         when Char_Category =>
            return To_Primitive (LKI.As_Char (Value) & "", Ctx.Pool);
         when Token_Category =>
            return To_Primitive (LKI.As_Token (Value), Ctx.Pool);
         when Symbol_Category =>
            return To_Primitive (LKI.As_Symbol (Value), Ctx.Pool);
         when Enum_Category =>
            return To_Primitive
              (LKN.Format_Name
                 (LKI.Enum_Value_Name (LKI.As_Enum (Value)), LKN.Lower),
               Ctx.Pool);
         when Array_Category =>
            declare
               Res : constant Primitive := Make_Empty_List (Ctx.Pool);
               Arr : constant Value_Ref_Array := LKI.As_Array (Value);
            begin
               for J in Arr'Range loop
                  declare
                     V    : constant LKI.Value_Ref := Arr (J);
                     Prim : constant Primitive :=
                       Make_Primitive (Ctx, V);
                  begin
                     Res.List_Val.Elements.Append (Prim);
                  end;
               end loop;

               return Res;
            end;
         when Struct_Category =>
            if LKI.Is_Node_Type (T) then
               return To_Primitive (LKI.As_Node (Value), Ctx.Pool);
            else
               --  Structs are mapped to LKQL objects
               declare
                  Membs : constant Struct_Member_Ref_Array := Members (T);
                  Ret   : constant Primitive := Make_Empty_Object (Ctx.Pool);
               begin
                  for Member of Membs loop
                     Ret.Obj_Assocs.Elements.Include
                       (Find (Get_Context (Ctx.Kernel.all).Get_Symbol_Table,
                              LKN.Format_Name
                                (LKI.Member_Name (Member), LKN.Lower)),
                        Make_Primitive (Ctx, LKI.Eval_Member (Value, Member)));
                  end loop;
                  return Ret;
               end;
            end if;

         when others =>
            Ctx.Raise_Error
              (L.No_Lkql_Node,
               "Unsupported value type from the introspection API: " &
               LKI.Category (T)'Wide_Wide_Image);
      end case;

   end Make_Primitive;

   ---------------------
   -- Make_Value_Type --
   ---------------------

   function Make_Value_Type
     (Value       : Primitive;
      Target_Type : LKI.Type_Ref;
      Ctx         : Eval_Context) return LKI.Value_Ref
   is
      use LKI;

      Target_Cat : constant LKI.Type_Category := Category (Target_Type);

      Id : constant Langkit_Support.Generic_API.Language_Id :=
        Language (Target_Type);
   begin
      case Value.Kind is
         when Kind_List =>
            if Target_Cat = LKI.Array_Category then
               return List_To_Value_Ref
                 (Value.List_Val.all, Target_Type, Ctx);
            end if;

         when Kind_Str =>
            if Target_Cat = LKI.Enum_Category then

               --  TODO: This actually can't be tested with LAL because we have
               --  no properties that take enums as parameters, and we don't
               --  want to add one just for the sake of testing that. Let's see
               --  if we add such a property someday, or when we have the
               --  possibility of testing with various Langkit based languages.

               return LKI.Create_Enum
                 (Ctx.Get_Name_Map.Lookup_Enum_Value
                   (Target_Type,
                    Ctx.Symbol (Value.Str_Val.all)));

            elsif Target_Cat = LKI.Symbol_Category then
               return LKI.From_Symbol
                 (Id, Value.Str_Val.all);

            elsif Target_Cat = LKI.Char_Category then
               if Value.Str_Val'Length > 1 then
                  Ctx.Raise_Error
                    (L.No_Lkql_Node, "String too long for conversion to char");
               end if;

               return LKI.From_Char
                 (Id, Value.Str_Val (Value.Str_Val'First));
            else
               return LKI.From_String
                 (Id, Value.Str_Val.all);
            end if;

         when Kind_Int =>
            if Target_Cat = LKI.Int_Category then
               return LKI.From_Int (Id, +Value.Int_Val);
            elsif Target_Cat = LKI.Big_Int_Category then
               return LKI.From_Big_Int
                 (Id, GNATCOLL.GMP.Integers.Make (Image (Value.Int_Val)));
            end if;

         when Kind_Bool =>
            if Target_Cat = LKI.Bool_Category then
               return LKI.From_Bool (Id, Value.Bool_Val);
            end if;

         when Kind_Node =>
            if LKI.Is_Node_Type (Target_Type) then
               return LKI.From_Node (Id, Value.Node_Val);
            else
               Ctx.Raise_Error (L.No_Lkql_Node, "Cannot pass struct in");
            end if;

         when Kind_Analysis_Unit =>
            if Target_Cat = LKI.Analysis_Unit_Category then
               return LKI.From_Unit (Id, Value.Analysis_Unit_Val);
            end if;

         when others => null;
      end case;

      Ctx.Raise_Error
        (L.No_Lkql_Node,
         To_Text
           ("Cannot convert a " & Valid_Primitive_Kind'Image (Value.Kind)
            & " to a " & Debug_Name (Target_Type)));
   end Make_Value_Type;

   -----------------------
   -- List_To_Value_Ref --
   -----------------------

   function List_To_Value_Ref
     (Value        : Primitive_List;
      Array_Type   : LKI.Type_Ref;
      Ctx          : Eval_Context) return LKI.Value_Ref
   is
      Values : LKI.Value_Ref_Array
        (Value.Elements.First_Index .. Value.Elements.Last_Index);

      Element_Type : constant LKI.Type_Ref :=
        LKI.Array_Element_Type (Array_Type);

   begin
      for I in Value.Elements.First_Index .. Value.Elements.Last_Index loop
         Values (I) := Make_Value_Type (Value.Elements (I), Element_Type, Ctx);
      end loop;

      return LKI.Create_Array (Array_Type, Values);
   end List_To_Value_Ref;

end LKQL.Node_Data;

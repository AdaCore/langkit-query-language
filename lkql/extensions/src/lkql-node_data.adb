with LKQL.Errors;         use LKQL.Errors;
with LKQL.Evaluation;     use LKQL.Evaluation;
with LKQL.Error_Handling; use LKQL.Error_Handling;

with Ada.Exceptions; use Ada.Exceptions;

package body LKQL.Node_Data is

   function To_Primitive (Value : in out Introspection_Value) return Primitive;
   --  Create a Primitive value from the given Introspection value and
   --  deallocate the introspection value's memory.

   function To_Primitive (Value : AST_Node_Array_Access) return Primitive;
   --  Create a Primitive value from the given node array

   function To_Primitive
     (Value : Unbounded_Text_Array_Access) return Primitive;
   --  Create a Primitive value from the given text array

   function Check_And_Remove_Prefix
     (Ctx       : Eval_Context;
      Name_Node : L.Identifier;
      Prefix    : Text_Type;
      White_List : Unbounded_Text_Set) return Text_Type;
   --  Helper function to check a prefix on an LKQL identifier, and return the
   --  text without the prefix.

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

      Real_Name : constant Text_Type :=
        Check_And_Remove_Prefix (Ctx, Field_Name, "f_", Builtin_Fields);
   begin
      if not Receiver.Get.Is_Field_Name (Real_Name) then
         Raise_No_Such_Field (Ctx, Receiver, Field_Name);
      end if;

      Result := Receiver.Get.Access_Field (Real_Name);

      return To_Primitive (Result);

   exception
      when Error : Introspection_Error =>
         Raise_And_Record_Error
           (Ctx, Make_Eval_Error (Field_Name,
                                  To_Text (Exception_Message (Error))));
   end Access_Node_Field;

   -----------------------------
   -- Check_And_Remove_Prefix --
   -----------------------------

   function Check_And_Remove_Prefix
     (Ctx        : Eval_Context;
      Name_Node  : L.Identifier;
      Prefix     : Text_Type;
      White_List : Unbounded_Text_Set) return Text_Type
   is
      Text_Name : constant Text_Type := Name_Node.Text;

      function Name_Prefix return Text_Type is
        (Text_Name (Text_Name'First .. Text_Name'First + Prefix'Length - 1));

   begin
      if White_List.Contains (To_Unbounded_Text (Text_Name)) then
         return Text_Name;
      end if;

      if Text_Name'Length <= Prefix'Length + 1 then
         Raise_And_Record_Error
           (Ctx, Make_Eval_Error (Name_Node, "Invalid name"));

      elsif Name_Prefix /= Prefix then
         Raise_And_Record_Error
           (Ctx,
            Make_Eval_Error
              (Name_Node, "Invalid prefix: " & Name_Prefix));
      end if;

      return Ret : constant Text_Type :=
        Text_Name (Text_Name'First + Prefix'Length .. Text_Name'Last)
      do
         if White_List.Contains (To_Unbounded_Text (Ret)) then
            Raise_And_Record_Error
              (Ctx, Make_Eval_Error (Name_Node, "Invalid name"));
         end if;
      end return;

   end Check_And_Remove_Prefix;

   ------------------------
   -- Eval_Node_Property --
   ------------------------

   function Eval_Node_Property (Ctx           : Eval_Context;
                                Receiver      : AST_Node_Rc;
                                Property_Name : L.Identifier;
                                Args          : L.Arg_List) return Primitive
   is
      Result        : Introspection_Value;

      Real_Name     : constant Text_Type :=
        Check_And_Remove_Prefix (Ctx, Property_Name, "p_", Builtin_Properties);

      Property_Args : constant Introspection_Value_Array :=
        Introspection_Value_Array_From_Args
          (Ctx, Receiver, Real_Name, Args);

   begin
      if not Receiver.Get.Is_Property_Name (Real_Name) then
         Raise_No_Such_Property (Ctx, Receiver, Property_Name);
      end if;

      if Args.Children_Count > Receiver.Get.Property_Arity (Real_Name)
      then
         Raise_Invalid_Arity
           (Ctx, Receiver.Get.Property_Arity (Real_Name), Args);
      end if;

      Result :=
        Receiver.Get.Evaluate_Property (Real_Name, Property_Args);

      return To_Primitive (Result);

   exception
      when Error : Introspection_Error =>
         Raise_And_Record_Error
           (Ctx, Make_Eval_Error (Property_Name,
                                  To_Text (Exception_Message (Error))));
   end Eval_Node_Property;

   ----------------------------------------
   -- Introspection_Value_Array_From_Arg --
   ----------------------------------------

   function Introspection_Value_Array_From_Args
     (Ctx           : Eval_Context;
      Node          : AST_Node_Rc;
      Property_Name : Text_Type;
      Args          : L.Arg_List)
      return Introspection_Value_Array
   is
      Arity  : constant Natural := Node.Get.Property_Arity (Property_Name);
      Result : Introspection_Value_Array (1 .. Arity);
   begin
      for I in 1 .. Arity loop
         Result (I) :=
           (if I <= Args.Children_Count
            then To_Introspection_Value
                    (Eval (Ctx, Args.List_Child (I).P_Expr))
            else Node.Get.Default_Arg_Value (Property_Name, I));
      end loop;

      return Result;
   end Introspection_Value_Array_From_Args;

begin
   Builtin_Fields.Include (To_Unbounded_Text ("parent"));
   Builtin_Fields.Include (To_Unbounded_Text ("previous_sibling"));
   Builtin_Fields.Include (To_Unbounded_Text ("next_sibling"));
   Builtin_Fields.Include (To_Unbounded_Text ("children"));
   Builtin_Fields.Include (To_Unbounded_Text ("text"));
   Builtin_Fields.Include (To_Unbounded_Text ("image"));
   Builtin_Fields.Include (To_Unbounded_Text ("unit"));
end LKQL.Node_Data;

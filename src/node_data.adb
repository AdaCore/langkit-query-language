with Interpreter.Evaluation;     use Interpreter.Evaluation;
with Interpreter.Error_Handling; use Interpreter.Error_Handling;

with Ada.Assertions; use Ada.Assertions;
with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;

package body Node_Data is

   -----------------
   -- Access_Data --
   -----------------

   function Access_Data (Ctx             : Eval_Context;
                         Receiver        : LAL.Ada_Node;
                         Member          : L.Identifier) return Primitive
   is
      Member_Name : constant String := To_UTF8 (Member.Text);
   begin
      return (if Is_Built_In (Member_Name)
              then Built_In_Property (Receiver, Member_Name)
              else Access_Custom_Data (Ctx, Receiver, Member));
   end Access_Data;

   -------------------
   -- Call_Property --
   -------------------

   function Call_Property (Ctx           : Eval_Context;
                           Receiver      : LAL.Ada_Node;
                           Call          : L.Dot_Call) return Primitive
   is
      Property_Ref       : constant Any_Node_Data_Reference :=
        Data_Reference_For_Name (Receiver, Call.F_Member.Text);
   begin
      if Property_Ref = None or not (Property_Ref in Property_Reference'Range)
      then
         Raise_Invalid_Member (Ctx, Call.F_Member, To_Primitive (Receiver));
      end if;

      return Call_Property (Ctx, Receiver, Property_Ref, Call);
   end Call_Property;

   -------------------
   -- Call_Property --
   -------------------

   function Call_Property (Ctx          : Eval_Context;
                           Receiver     : LAL.Ada_Node;
                           Property_Ref : Property_Reference;
                           Call         : L.Dot_Call) return Primitive
   is
      Arguments          : constant L.Expr_List := Call.F_Arguments;
      Arguments_Type     : constant Value_Constraint_Array :=
        Property_Argument_Types (Property_Ref);
      Property_Arguments : Value_Array (1 .. Arguments_Type'Length);
   begin
      if Arguments_Type'Length /= Arguments.Children_Count then
         Raise_Invalid_Arity (Ctx, Arguments_Type'Length, Arguments);
      end if;

      for I in 1 .. Arguments_Type'Length loop
         declare
            Arg       : constant L.LKQL_Node := Arguments.Children (I);
            Arg_Value : constant Primitive := Eval (Ctx, Arg);
            Arg_Type  : constant Value_Kind := Arguments_Type (I).Kind;
         begin
            Property_Arguments (I) :=
              To_Value_Type (Ctx, Arg.As_Expr, Arg_Value, Arg_Type);
         end;
      end loop;

      declare
         Result : constant Value_Type :=
           Evaluate_Node_Data (Receiver, Property_Ref, Property_Arguments);
      begin
         return Create_Primitive (Ctx, Call.As_LKQL_Node, Result);
      end;
   end Call_Property;

   -----------------------------
   -- Data_Reference_For_Name --
   -----------------------------

   function Data_Reference_For_Name
     (Receiver : LAL.Ada_Node; Name : Text_Type) return Any_Node_Data_Reference
   is
      Receiver_Type_Id : constant Node_Type_Id :=
        Id_For_Kind (Receiver.Kind);
   begin
      return Lookup_Node_Data (Receiver_Type_Id, To_UTF8 (Name));
   end Data_Reference_For_Name;

   ---------------------
   -- Create_Primitve --
   ---------------------

   function Create_Primitive (Ctx    : Eval_Context;
                              Member : L.LKQL_Node;
                              Value  : Value_Type) return Primitive
   is
   begin
      case Kind (Value) is
         when Boolean_Value =>
            return To_Primitive (As_Boolean (Value));
         when Integer_Value =>
            return To_Primitive (As_Integer (Value));
         when Character_Value =>
            return To_Primitive
              (Character_Type'Wide_Wide_Image ((As_Character (Value))));
         when Unbounded_Text_Value =>
            return To_Primitive (As_Unbounded_Text (Value));
         when Node_Value =>
            return To_Primitive (As_Node (Value));
         when others =>
            Raise_Unsupported_Value_Type
              (Ctx, Member.As_LKQL_Node, Kind (Value));
      end case;
   end Create_Primitive;

   -------------------
   -- To_Value_Type --
   -------------------

   function To_Value_Type (Ctx         : Eval_Context;
                           Value_Expr  : L.Expr;
                           Value       : Primitive;
                           Target_Kind : Value_Kind) return Value_Type
   is
   begin
      if Target_Kind = Integer_Value and then Kind (Value) = Kind_Int then
         return Create_Integer (Int_Val (Value));
      elsif Target_Kind = Boolean_Value and then Kind (Value) = Kind_Bool then
         return Create_Boolean (Bool_Val (Value));
      elsif Target_Kind = Node_Value and then Kind (Value) = Kind_Node then
         return Create_Node (Node_Val (Value));
      elsif Target_Kind = Text_Type_Value and then Kind (Value) = Kind_Str then
         return Create_Text_Type (To_Text (Str_Val (Value)));
      elsif Target_Kind = Character_Value and then
            Kind (Value) = Kind_Str and then
            Length ((Str_Val (Value))) = 1
      then
         return Create_Character (Element (Str_Val (Value), 1));
      elsif Target_Kind = Unbounded_Text_Value and then
        Kind (Value) = Kind_Bool
      then
         return Create_Unbounded_Text (Str_Val (Value));
      end if;

      Raise_Invalid_Type_Conversion (Ctx, Value_Expr, Value, Target_Kind);
   end To_Value_Type;

   -----------------------
   -- Built_In_Property --
   -----------------------

   function Built_In_Property
     (Receiver : LAL.Ada_Node; Property_Name : String) return Primitive
   is
   begin
      if Property_Name = "image" then
         return To_Primitive (To_Unbounded_Text (To_Text (Receiver.Image)));
      end if;

      raise Assertion_Error with "Invalid built-in property: " & Property_Name;
   end Built_In_Property;

   -----------------
   -- Is_Built_In --
   -----------------

   function Is_Built_In (Name : String) return Boolean is
     (Name = "image");

   ------------------------
   -- Access_Custom_Data --
   ------------------------

   function Access_Custom_Data (Ctx      : Eval_Context;
                                Receiver : LAL.Ada_Node;
                                Member   : L.Identifier) return Primitive
   is
      Data_Ref   : constant Any_Node_Data_Reference :=
        Data_Reference_For_Name (Receiver, Member.Text);
      Empty_Args : constant Value_Array (1 .. 0) := (others => <>);
   begin
      if Data_Ref = None then
         Raise_Invalid_Member (Ctx, Member, To_Primitive (Receiver));
      end if;

      return Create_Primitive
        (Ctx, Member.As_LKQL_Node,
         Evaluate_Node_Data (Receiver, Data_Ref, Empty_Args));
   end Access_Custom_Data;

end Node_Data;

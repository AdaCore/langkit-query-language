with Interpreter.Error_Handling; use Interpreter.Error_Handling;

with Ada.Assertions; use Ada.Assertions;

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
                              Member : L.Identifier;
                              Value  : Value_Type) return Primitive
   is
   begin
      case Kind (Value) is
         when Boolean_Value =>
            return To_Primitive (As_Boolean (Value));
         when Integer_Value =>
            return To_Primitive (As_Integer (Value));
         when Unbounded_Text_Value =>
            return To_Primitive (As_Unbounded_Text (Value));
         when Node_Value =>
            return To_Primitive (As_Node (Value));
         when others =>
            Raise_Unsupported_Value_Type
              (Ctx, Member.As_LKQL_Node, Kind (Value));
      end case;
   end Create_Primitive;

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
        (Ctx, Member, Evaluate_Node_Data (Receiver, Data_Ref, Empty_Args));
   end Access_Custom_Data;

end Node_Data;

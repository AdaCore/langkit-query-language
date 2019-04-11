with Ada.Text_IO;                     use Ada.Text_IO;
with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;
with Ada.Strings.Wide_Wide_Unbounded.Wide_Wide_Text_IO;
use Ada.Strings.Wide_Wide_Unbounded.Wide_Wide_Text_IO;

package body Interpreter.Error_Handling is

   function User_Chooses_Recovery return Boolean;
   --  Ask the user whether he wants to recover from the error or cancel the
   --  execution.

   ---------------------------
   -- User_Chooses_Recovery --
   ---------------------------

   function User_Chooses_Recovery return Boolean is
      Choice : Unbounded_Text_Type;
   begin
      Put_Line ("[R]esume execution / [A]bort execution ?");
      Get_Line (Choice);
      return Choice = "R" or else Choice = "r";
   end User_Chooses_Recovery;

   ----------------------------
   -- Raise_And_Record_Error --
   ----------------------------

   procedure Raise_And_Record_Error
     (Ctx : Eval_Context; Error : Error_Data)
   is
      Error_Message : constant String :=
        To_UTF8 (To_Text (Error.Short_Message));
   begin
      Ctx.Add_Error (Error);

      if Ctx.Error_Recovery_Enabled then
         Put_Line (Error_Description (Error));
         if User_Chooses_Recovery then
            raise Recoverable_Error with Error_Message;
         end if;
      end if;

      raise Stop_Evaluation_Error with Error_Message;
   end Raise_And_Record_Error;

   --------------------------
   -- Raise_Invalid_Member --
   --------------------------

   procedure Raise_Invalid_Member (Ctx      : Eval_Context;
                                   Node     : L.Dot_Access;
                                   Receiver : Primitive)
   is
   begin
      Raise_Invalid_Member (Ctx, Node.F_Member, Receiver);
   end Raise_Invalid_Member;

   --------------------------
   -- Raise_Invalid_Member --
   --------------------------

   procedure Raise_Invalid_Member (Ctx      : Eval_Context;
                                   Node     : L.Identifier;
                                   Receiver : Primitive)
   is
      Message : constant Unbounded_Text_Type :=
        "Cannot get member " & To_Unbounded_Text (Node.Text) &
        " for " & To_Text (Kind_Name (Receiver)) & " value";
   begin
      Raise_And_Record_Error
        (Ctx, Make_Eval_Error (Node.As_LKQL_Node, Message));
   end Raise_Invalid_Member;

   ---------------------
   -- Raise_Null_Root --
   ---------------------

   procedure Raise_Null_Root (Ctx : Eval_Context; Node : L.Query)
   is
      Message : constant Unbounded_Text_Type :=
        To_Unbounded_Text (
           To_Text ("Cannot run a query without a proper AST root"));
   begin
      Raise_And_Record_Error
        (Ctx, Make_Eval_Error (Node.As_LKQL_Node, Message));
   end Raise_Null_Root;

   ------------------------
   -- Raise_Invalid_Kind --
   ------------------------

   procedure Raise_Invalid_Kind (Ctx      : Eval_Context;
                                 Node     : L.LKQL_Node;
                                 Expected : Valid_Primitive_Kind;
                                 Value    : Primitive)
   is
      Message : constant Unbounded_Text_Type :=
        To_Unbounded_Text
          (To_Text
             ("Type error: expected " & To_String (Expected) & " but got " &
                Kind_Name (Value)
           ));
   begin
      Raise_And_Record_Error
        (Ctx, Make_Eval_Error (Node.As_LKQL_Node, Message));
   end Raise_Invalid_Kind;

   ---------------------------------
   -- Raise_Invalid_Selector_Name --
   ---------------------------------

   procedure Raise_Invalid_Selector_Name (Ctx  : Eval_Context;
                                          Node : L.Selector_Pattern'Class)
   is
      Message : constant Unbounded_Text_Type :=
        "Invalid selector name: " &
        To_Unbounded_Text (Node.P_Selector_Name);
   begin
      Raise_And_Record_Error
        (Ctx, Make_Eval_Error (Node.As_LKQL_Node, Message));
   end Raise_Invalid_Selector_Name;

   --------------------------
   -- Raise_Unknown_Symbol --
   --------------------------

   procedure Raise_Unknown_Symbol (Ctx : Eval_Context;
                                   Node : L.Identifier)
   is
      Message : constant Unbounded_Text_Type :=
        "Unknown symbol: " & To_Unbounded_Text (Node.Text);
   begin
      Raise_And_Record_Error
        (Ctx, Make_Eval_Error (Node.As_LKQL_Node, Message));
   end Raise_Unknown_Symbol;

   -----------------------------------
   -- Raise_already_Existing_Symbol --
   -----------------------------------

   procedure Raise_Already_Existing_Symbol (Ctx        : Eval_Context;
                                            Identifier : Unbounded_Text_Type;
                                            Node       : L.LKQL_Node)
   is
      Message : constant Unbounded_Text_Type :=
        "already existing symbol: " & Identifier;
   begin
      Raise_And_Record_Error
        (Ctx, Make_Eval_Error (Node, Message));
   end Raise_Already_Existing_Symbol;

   -------------------------
   -- Raise_Invalid_Arity --
   -------------------------

   procedure Raise_Invalid_Arity (Ctx            : Eval_Context;
                                  Expected_Arity : Positive;
                                  Arguments      : L.Expr_List)
   is
      Expected : constant Text_Type :=
        Integer'Wide_Wide_Image (Expected_Arity);
      Actual_Arity   : constant Text_Type :=
        Integer'Wide_Wide_Image (Arguments.Children_Count);
      Message  : constant Unbounded_Text_Type :=
        To_Unbounded_Text
          ("Expected" & Expected & " arguments but got" & Actual_Arity);
   begin
      Raise_And_Record_Error
        (Ctx, Make_Eval_Error (Arguments.As_LKQL_Node, Message));
   end Raise_Invalid_Arity;

   ----------------------------------
   -- Raise_Unsupported_Value_Type --
   ----------------------------------

   procedure Raise_Unsupported_Value_Type (Ctx       : Eval_Context;
                                           Node      : L.LKQL_Node;
                                           Kind      : Value_Kind)
   is
      Kind_Name : constant Unbounded_Text_Type :=
        To_Unbounded_Text (Value_Kind'Wide_Wide_Image (Kind));
      Message : constant Unbounded_Text_Type :=
        "Unsupported value type: " & Kind_Name;
   begin
      Raise_And_Record_Error (Ctx, Make_Eval_Error (Node, Message));
   end Raise_Unsupported_Value_Type;

   -----------------------------------
   -- Raise_Invalid_Type_Conversion --
   -----------------------------------

   procedure Raise_Invalid_Type_Conversion (Ctx           : Eval_Context;
                                            Value_Expr    : L.Expr;
                                            Value         : Primitive;
                                            Expected_Kind : Value_Kind)
   is
      Expected_Kind_Name : constant Unbounded_Text_Type :=
        To_Unbounded_Text (Value_Kind'Wide_Wide_Image (Expected_Kind));
      Message : constant Unbounded_Text_Type :=
        "Cannot convert a : " & To_Text (Kind_Name (Value)) &
        " to a " & Expected_Kind_Name;
   begin
      Raise_And_Record_Error
        (Ctx, Make_Eval_Error (Value_Expr.As_LKQL_Node, Message));
   end Raise_Invalid_Type_Conversion;

end Interpreter.Error_Handling;

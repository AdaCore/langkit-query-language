with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;
with Langkit_Support.Text; use Langkit_Support.Text;

package body Interpreter.Error_Handling is

   ---------------------
   -- Init_Error_Flow --
   ---------------------

   procedure Init_Error_Flow (Ctx : in out Eval_Context; Error : Error_Data) is
      Error_Message : constant String :=
        To_UTF8 (To_Text (Error.Short_Message));
   begin
      Add_Error (Ctx, Error);
      if Ctx.Error_Recovery_Enabled then
         raise Recoverable_Error with Error_Message;
      else
         raise Eval_Error with Error_Message;
      end if;
   end Init_Error_Flow;

   -----------------
   -- Raise_Error --
   -----------------

   procedure Raise_Error (Ctx : in out Eval_Context;
                          Node : LEL.LKQL_Node;
                          Message : String)
   is
      Unicode_Message : constant Unbounded_Text_Type :=
        To_Unbounded_Text (To_Text (Message));
   begin
      Init_Error_Flow (Ctx, Make_Eval_Error (Node, Unicode_Message));
   end Raise_Error;

   --------------------------
   -- Raise_Invalid_Member --
   --------------------------

   procedure Raise_Invalid_Member (Ctx      : in out Eval_Context;
                                   Node     : LEL.Dot_Access;
                                   Receiver : Primitive)
   is
      Message : constant Unbounded_Text_Type :=
        "Cannot get member " & To_Unbounded_Text (Node.F_Member.Text) &
        " for " & To_Text (Kind_Name (Receiver)) & " value";
   begin
      Init_Error_Flow (Ctx, Make_Eval_Error (Node.As_LKQL_Node, Message));
   end Raise_Invalid_Member;

   ------------------------------
   -- Raise_Invalid_Is_Operand --
   ------------------------------

   procedure Raise_Invalid_Is_Operand (Ctx         : in out Eval_Context;
                                       Node        : LEL.Is_Clause;
                                       Tested_Node : Primitive)
   is
      Message : constant Unbounded_Text_Type :=
        "Invalid kind on the left side of an is clause: expected Node " &
        "but got " & To_Unbounded_Text (To_Text (Kind_Name (Tested_Node)));
   begin
      Init_Error_Flow (Ctx, Make_Eval_Error (Node.As_LKQL_Node, Message));
   end Raise_Invalid_Is_Operand;

   ---------------------
   -- Raise_Null_Root --
   ---------------------

   procedure Raise_Null_Root (Ctx : in out Eval_Context; Node : LEL.Query)
   is
      Message : constant Unbounded_Text_Type :=
        To_Unbounded_Text (
           To_Text ("Cannot run a query without a proper AST root"));
   begin
      Init_Error_Flow (Ctx, Make_Eval_Error (Node.As_LKQL_Node, Message));
   end Raise_Null_Root;

   ------------------------
   -- Raise_Invalid_Type --
   ------------------------

   procedure Raise_Invalid_Type (Ctx      : in out Eval_Context;
                                 Node     : LEL.LKQL_Node;
                                 Expected : String;
                                 Actual   : String)
   is
      Message : constant Unbounded_Text_Type :=
        To_Unbounded_Text (
           To_Text (
              "Type error: expected " & Expected & " but got " & Actual));
   begin
      Init_Error_Flow (Ctx, Make_Eval_Error (Node.As_LKQL_Node, Message));
   end Raise_Invalid_Type;

end Interpreter.Error_Handling;

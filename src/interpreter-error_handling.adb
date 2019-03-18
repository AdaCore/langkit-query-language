with Langkit_Support.Text; use Langkit_Support.Text;

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
     (Ctx : Eval_Context_Ptr; Error : Error_Data)
   is
      Error_Message : constant String :=
        To_UTF8 (To_Text (Error.Short_Message));
   begin
      Add_Error (Ctx.all, Error);

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

   procedure Raise_Invalid_Member (Ctx      : Eval_Context_Ptr;
                                   Node     : LEL.Dot_Access;
                                   Receiver : Primitive)
   is
      Message : constant Unbounded_Text_Type :=
        "Cannot get member " & To_Unbounded_Text (Node.F_Member.Text) &
        " for " & To_Text (Kind_Name (Receiver)) & " value";
   begin
      Raise_And_Record_Error
        (Ctx, Make_Eval_Error (Node.As_LKQL_Node, Message));
   end Raise_Invalid_Member;

   ---------------------
   -- Raise_Null_Root --
   ---------------------

   procedure Raise_Null_Root (Ctx : Eval_Context_Ptr; Node : LEL.Query)
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

   procedure Raise_Invalid_Kind (Ctx      : Eval_Context_Ptr;
                                 Node     : LEL.LKQL_Node;
                                 Expected : Primitive_Kind;
                                 Value   : Primitive)
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

end Interpreter.Error_Handling;

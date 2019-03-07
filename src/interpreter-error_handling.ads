with Interpreter.Errors;           use Interpreter.Errors;
with Interpreter.Eval_Contexts;    use Interpreter.Eval_Contexts;
with Interpreter.Types.Primitives; use Interpreter.Types.Primitives;

package Interpreter.Error_Handling is

   procedure Raise_And_Record_Error
     (Ctx : in out Eval_Context; Error : Error_Data)
      with No_Return;
   --  Add the given error to the evaluation context and:
   --    * raise a Recoverable_Error if error recovery is enabled and the
   --      user chooses to ignore the error
   --    * raise a Stop_Evaluation_Error if error recovery is disabled or the
   --      user chooses not to ignore the error

   procedure Raise_Invalid_Member (Ctx      : in out Eval_Context;
                                   Node     : LEL.Dot_Access;
                                   Receiver : Primitive)
      with No_Return;
   --  Raise an exception signaling an invalid member access, and add an
   --  Error_Data describing the error to the evaluation context.

   procedure Raise_Invalid_Is_Operand (Ctx         : in out Eval_Context;
                                       Node        : LEL.LKQL_Node;
                                       Tested_Node : Primitive)
      with No_Return;
   --  Raise an exception signaling that the left operand of an is clause is
   --  not a node, and add an Error_Data describing the error to the
   --  evaluation context.

   procedure Raise_Null_Root (Ctx : in out Eval_Context; Node : LEL.Query)
      with No_Return;
   --  Raise an exception signaling the absence of a proper AST root while
   --  trying to execute a query, and add an Error_Data describing the error
   --  to the evaluation context.

   procedure Raise_Invalid_Type (Ctx      : in out Eval_Context;
                                 Node     : LEL.LKQL_Node;
                                 Expected : String;
                                 Actual   : String)
      with No_Return;
   --  Raise an exception signaling a type error, and add an Error_Data
   --  describing the error to the evaluation context.

end Interpreter.Error_Handling;

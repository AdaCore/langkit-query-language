with Interpreter.Errors; use Interpreter.Errors;
with Interpreter.Eval_Contexts; use Interpreter.Eval_Contexts;
with Interpreter.Types.Primitives; use Interpreter.Types.Primitives;

package Interpreter.Error_Handling is

   procedure Init_Error_Flow (Ctx : in out Eval_Context; Error : Error_Data);
   pragma No_Return (Init_Error_Flow);

   procedure Raise_Error (Ctx     : in out Eval_Context;
                          Node    : LEL.LKQL_Node;
                          Message : String);
   pragma No_Return (Raise_Error);
   --  Raise a generic exception with the given message, and add an Error_Data
   --  describing the error to the evaluation context.

   procedure Raise_Invalid_Member (Ctx      : in out Eval_Context;
                                   Node     : LEL.Dot_Access;
                                   Receiver : Primitive);
   pragma No_Return (Raise_Invalid_Member);
   --  Raise an exception signaling an invalid member access, and add an
   --  Error_Data describing the error to the evaluation context.

   procedure Raise_Invalid_Is_Operand (Ctx         : in out Eval_Context;
                                       Node        : LEL.Is_Clause;
                                       Tested_Node : Primitive);
   pragma No_Return (Raise_Invalid_Member);
   --  Raise an exception signaling that the left operand of an is clause is
   --  not a node, and add an Error_Data describing the error to the
   --  evaluation context.

   procedure Raise_Null_Root (Ctx : in out Eval_Context; Node : LEL.Query);
   pragma No_Return (Raise_Null_Root);
   --  Raise an exception signaling the absence of a proper AST root while
   --  trying to execute a query, and add an Error_Data describing the error
   --  to the evaluation context.

   procedure Raise_Invalid_Type (Ctx      : in out Eval_Context;
                                 Node     : LEL.LKQL_Node;
                                 Expected : String;
                                 Actual   : String);
   pragma No_Return (Raise_Invalid_Type);
   --  Raise an exception signaling a type error, and add an Error_Data
   --  describing the error to the evaluation context.

end Interpreter.Error_Handling;

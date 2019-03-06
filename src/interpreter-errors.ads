with Langkit_Support.Text; use Langkit_Support.Text;

package Interpreter.Errors is

   Stop_Evaluation_Error : exception;
   --  This type of exception is used to signal that the execution should not
   --  be resumed.

   Recoverable_Error : exception;
   --  This type of exception is used to indicate that the evaluator should
   --  try to resume execution in spite of the error.

   type Error_Kind is (Empty_Error, Eval_Error);
   --  Denotes the kind of an error value.

   type Error_Data (Kind : Error_Kind := Empty_Error) is record
      case Kind is
         when Empty_Error =>
            null;
            --  Represents the absence of error
         when Eval_Error =>
            AST_Node     : LEL.LKQL_Node;
            --  Node whose evaluation triggered this error

            Short_Message : Unbounded_Text_Type;
            --  A short description of the error
      end case;
   end record;
   --  Store an error value.

   function Error_Description (Error : Error_Data) return Unbounded_Text_Type;
   --  Return a detailed description of the given error.

   function Is_Error (Err : Error_Data) return Boolean;
   --  Return whether Err contains an error

   function Make_Empty_Error return Error_Data;

   function Make_Eval_Error (AST_Node      : LEL.LKQL_Node;
                             Short_Message : Unbounded_Text_Type)
                             return Error_Data;
   --  Create an error value of kind Eval_Error

end Interpreter.Errors;

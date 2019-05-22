with Langkit_Support.Text; use Langkit_Support.Text;

package LKQL.Errors is

   Stop_Evaluation_Error : exception;
   --  This type of exception is used to signal that the execution should not
   --  be resumed.

   Recoverable_Error : exception;
   --  This type of exception is used to indicate that the evaluator should
   --  try to resume execution in spite of the error.

   type Error_Kind is
     (No_Error,
      --  Asbence of error
      Eval_Error
      --  Error originating from the execution of the LKQL program
     );
   --  Denotes the kind of an error value.

   type Error_Data (Kind : Error_Kind := No_Error) is record
      case Kind is
         when No_Error =>
            null;
            --  Represents the absence of error
         when Eval_Error =>
            AST_Node     : L.LKQL_Node;
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

   function Make_Eval_Error (AST_Node      : L.LKQL_Node'Class;
                             Short_Message : Text_Type)
                             return Error_Data;
   --  Create an error value of kind Eval_Error

end LKQL.Errors;

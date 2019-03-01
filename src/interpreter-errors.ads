package Interpreter.Errors is

   type Error_Kind is (Empty_Error, Eval_Error);
   --  Denotes the kind of an error value.

   type Error_Data (Kind : Error_Kind := Empty_Error) is record
      case Kind is
         when Empty_Error =>
            null;
            --  Represents the absence of error
         when Eval_Error =>
            AST_Node : LEL.LKQL_Node;
            --  AST node where the error occured
      end case;
   end record;
   --  Store an error value.

   function Is_Error (Err : Error_Data) return Boolean;
   --  Return wether the error value represents an actual error

   function Make_Empty_Error return Error_Data;

   function Make_Eval_Error (AST_Node : LEL.LKQL_Node) return Error_Data;
   --  Create an error value of kind Eval_Error

end Interpreter.Errors;

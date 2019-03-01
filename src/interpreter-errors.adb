package body Interpreter.Errors is

   --------------
   -- Is_Error --
   --------------

   function Is_Error (Err : Error_Data) return Boolean is
   begin
      return Err.Kind /= Empty_Error;
   end Is_Error;

   ----------------------
   -- Make_Empty_Error --
   ----------------------

   function Make_Empty_Error return Error_Data is
   begin
      return (Kind => Empty_Error);
   end Make_Empty_Error;

   ---------------------
   -- Make_Eval_Error --
   ---------------------

   function Make_Eval_Error (AST_Node : LEL.LKQL_Node) return Error_Data
   is
   begin
      return (Kind => Eval_Error, AST_Node => AST_Node);
   end Make_Eval_Error;

end Interpreter.Errors;

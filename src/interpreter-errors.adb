package body Interpreter.Errors is

   --------------
   -- Is_Error --
   --------------

   function Is_Error (Err : Error_Data) return Boolean is
   begin
      return Err.Kind /= Kind_Empty_Error;
   end Is_Error;

   ----------------------
   -- Make_Empty_Error --
   ----------------------

   function Make_Empty_Error return Error_Data is
   begin
      return (Kind => Kind_Empty_Error);
   end Make_Empty_Error;

   ---------------------
   -- Make_Eval_Error --
   ---------------------

   function Make_Eval_Error (AST_Node      : LEL.LKQL_Node;
                             Short_Message : Unbounded_Text_Type)
                             return Error_Data
   is
   begin
      return (Kind => Kind_Eval_Error,
              AST_Node => AST_Node,
              Short_Message => Short_Message);
   end Make_Eval_Error;

end Interpreter.Errors;

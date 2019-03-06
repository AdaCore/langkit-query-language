package body Interpreter.Eval_Contexts is

   procedure Add_Error (Ctx : in out Eval_Context; Error : Error_Data) is
   begin
      Ctx.Last_Error := Error;
   end Add_Error;

end Interpreter.Eval_Contexts;

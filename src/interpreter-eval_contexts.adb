package body Interpreter.Eval_Contexts is

   ---------------
   -- Add_Error --
   ---------------

   procedure Add_Error (Ctx : in out Eval_Context_Value; Error : Error_Data) is
   begin
      Ctx.Last_Error := Error;
   end Add_Error;

   ----------------
   -- Backup_Env --
   ----------------

   function Backup_Env (Parent_Env : Environment;
                        Local_Env  : Environment)
                        return Environment
   is
      Backup : Map;
   begin
      for C in Local_Env.Iterate loop
         if Parent_Env.Contains (Key (C)) then
            Backup.Insert (Key (C), Element (C));
         end if;
      end loop;

      return Backup;
   end Backup_Env;

   ----------------
   -- Update_Env --
   ----------------

   procedure Update_Env (Env        : in out Environment;
                         New_Values : Environment)
   is
   begin
      for C in New_Values.Iterate loop
         Env.Include (Key (C), Element (C));
      end loop;
   end Update_Env;

end Interpreter.Eval_Contexts;

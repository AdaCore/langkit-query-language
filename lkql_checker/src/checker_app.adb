with Ada.Directories; use Ada.Directories;

with Langkit_Support.Diagnostics.Output;
with Langkit_Support.Text; use Langkit_Support.Text;

with Libadalang.Analysis; use Libadalang.Analysis;

with Rules_Factory; use Rules_Factory;
with Rule_Commands; use Rule_Commands;

with LKQL.Eval_Contexts; use LKQL.Eval_Contexts;
with Ada_AST_Nodes; use Ada_AST_Nodes;

package body Checker_App is

   function Rules return Rule_Vector;

   -----------
   -- Rules --
   -----------

   function Rules return Rule_Vector is
      Explicit_Rules_Names : constant Args.Rules.Result_Array
        := Args.Rules.Get;

      Ret : Rule_Vector;
   begin
      if Explicit_Rules_Names'Length = 0 then
         --  No rules passed by the user: return all rules
         return All_Rules;
      else
         --  Some rules passed by the user: only return the ones specified
         for R of All_Rules loop
            for Explicit_Rule_Name of Explicit_Rules_Names loop
               if To_Text (To_String (Explicit_Rule_Name)) = To_Text (R.Name)
               then
                  Ret.Append (R);
               end if;
            end loop;
         end loop;
      end if;

      return Ret;
   end Rules;

   ---------------------
   -- Process_Context --
   ---------------------

   procedure Job_Post_Process (Context : App_Job_Context)
   is
   begin

      --  Set property error recovery with the value of the command line flag.
      LKQL.Errors.Property_Error_Recovery := Args.Property_Error_Recovery.Get;

      declare
         Ctx : constant Eval_Context :=
           Make_Eval_Context (Context.Units_Processed);
      begin
         for Rule of Rules loop
            for Diag of Rule.Evaluate (Ctx) loop
               Langkit_Support.Diagnostics.Output.Print_Diagnostic
                 (Diag.Diag, Diag.Unit, Simple_Name (Diag.Unit.Get_Filename));
            end loop;
         end loop;
      end;
   end Job_Post_Process;

end Checker_App;

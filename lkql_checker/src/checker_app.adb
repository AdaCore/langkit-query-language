with Ada.Directories; use Ada.Directories;
with Ada.Text_IO; use Ada.Text_IO;

with Langkit_Support.Diagnostics.Output;
with Langkit_Support.Text; use Langkit_Support.Text;

with Rules_Factory; use Rules_Factory;
with Rule_Commands; use Rule_Commands;


package body Checker_App is
   
   function Rules return Rule_Vector;
   
   -----------
   -- Rules --
   -----------

   function Rules return Rule_Vector is
      Explicit_Rules_Names : Args.Rules.Result_Array
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
   
   ------------------
   -- Process_Unit --
   ------------------

   procedure Process_Unit (Context : App_Job_Context; Unit : Analysis_Unit) is
   begin
      for Rule of Rules loop
         for Diag of Rule.Evaluate (Unit) loop
            Langkit_Support.Diagnostics.Output.Print_Diagnostic 
              (Diag, Unit, Simple_Name (Unit.Get_Filename));
         end loop;
      end loop; 
   end Process_Unit;

   ---------------------
   -- Process_Context --
   ---------------------

   procedure Process_Context
     (Ctx : Analysis_Context; Units : Unit_Vectors.Vector) is
   begin
      null;
   end Process_Context;
   
end Checker_App;

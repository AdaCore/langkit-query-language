with Run;

with Ada.Text_IO;      use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;

--  Usage:
--  main [SCRIPT_PATH] [PROJECT_PATH] [OPTIONS]
--
--  * SCRIPT_PATH: path of the LKQL script to evaluate
--  * PROJECT_PATH (optionnal): path of a GPR project file. If present, the
--    LKQL script will be run on every source file of the project.
--  * OPTIONS : -r enable error recovery

----------
-- Main --
----------

procedure Main is
   function Recovery_Enabled return Boolean;

   ----------------------
   -- Recovery_Enabled --
   ----------------------

   function Recovery_Enabled return Boolean is
   begin
      for I in 1 .. Argument_Count loop
         if Argument (I) = "-r" then
            return True;
         end if;
      end loop;

      return False;
   end Recovery_Enabled;

   Recovery : constant Boolean := Recovery_Enabled;
begin
   if Argument_Count = 1 or else (Argument_Count = 2 and then Recovery) then
      Run.Run_Standalone_Query (Argument (1), Recovery);
   elsif Argument_Count = 2 or else (Argument_Count = 3 and then Recovery) then
      Run.Run_Against_Project (Argument (1), Argument (2), Recovery);
   else
      Put_Line ("usage: main [SCRIPT_PATH] [PROJECT_PATH] [OPTIONS]");
   end if;
end Main;

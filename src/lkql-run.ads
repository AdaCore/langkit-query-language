with Libadalang.Analysis; use Libadalang.Analysis;

with GNATCOLL.VFS; use GNATCOLL.VFS;

package LKQL.Run is

   procedure Run_Against_Project (LKQL_Script      : String;
                                  Project_Path     : String;
                                  Recovery_Enabled : Boolean := False);
   --  Run a LKQL script on all source files that belong to the given project.
   --  If Recovery_Enabled is set to true, the user will have the possibility
   --  to resume execution uppon encoutering an error.
   --
   --  LKQL_Script is the name of the file containing the script and
   --  Project_Path is the project file name to load.

   procedure Run_On_Files (LKQL_Script      : String;
                           Ada_Context      : Analysis_Context;
                           Files            : File_Array_Access;
                           Recovery_Enabled : Boolean := False);

private

   function Make_LKQL_Unit (Script_Path : String) return L.Analysis_Unit;

   function Make_Ada_Unit
     (Context : Analysis_Context; Path : String) return Analysis_Unit;

end LKQL.Run;

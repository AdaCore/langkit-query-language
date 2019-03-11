with Liblkqllang.Analysis;
with Libadalang.Analysis;

with GNATCOLL.VFS; use GNATCOLL.VFS;

package Run is
   package LEL renames Liblkqllang.Analysis;
   package LAL renames Libadalang.Analysis;

   procedure Run_Standalone_Query
     (Script_Path : String; Recovery_Enabled : Boolean := False);
   --  Run a standalone LKQL script. If Recovery_Enabled is set to true,
   --  the user will have the possibility to resume execution uppon encoutering
   --  an error.
   --
   --  While in standalone mode, queries are forbidden and will raise an
   --  exception.

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
                           Ada_Context      : LAL.Analysis_Context;
                           Files            : File_Array_Access;
                           Recovery_Enabled : Boolean := False);

private

   function Make_LKQL_Unit (Script_Path : String) return LEL.Analysis_Unit;

   function Make_Ada_Unit
     (Context : LAL.Analysis_Context; Path : String) return LAL.Analysis_Unit;

end Run;

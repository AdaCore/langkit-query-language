with Liblkqllang.Analysis;
with Libadalang.Analysis;

with GNATCOLL.VFS; use GNATCOLL.VFS;

package Run is
   package LEL renames Liblkqllang.Analysis;
   package LAL renames Libadalang.Analysis;

   procedure Run_Standalone_Query (Script_Path : String);
   --  Run a standalone LKQL script.

   procedure Run_Against_Project (LKQL_Script : String; Project_Path : String);
   --  Run a LKQL script against an entire GPR Project.

private

   procedure Run_On_Files (LKQL_Script : String;
                           Ada_Context : LAL.Analysis_Context;
                           Files       : File_Array_Access);

   function Make_LKQL_Unit (Script_Path : String) return LEL.Analysis_Unit;

   function Make_Ada_Unit
     (Context : LAL.Analysis_Context; Path : String) return LAL.Analysis_Unit;

end Run;

with Interpreter.Evaluation; use Interpreter.Evaluation;
with Interpreter.Eval_Contexts; use Interpreter.Eval_Contexts;
with Interpreter.Types.Primitives; use Interpreter.Types.Primitives;

with Libadalang.Project_Provider;

with Ada.Text_IO; use Ada.Text_IO;

with GNAT.OS_Lib;

with GNATCOLL.Projects;
package body Run is
   package GPR renames GNATCOLL.Projects;
   package LAL_GPR renames Libadalang.Project_Provider;

   ----------------------
   -- Run_Single_query --
   ----------------------

   procedure Run_Standalone_Query
     (Script_Path : String; Recovery_Enabled : Boolean := False)
   is
      Unit    : constant LEL.Analysis_Unit :=
        Make_LKQL_Unit (Script_Path);
      Context : Eval_Context;
      Ignore  : Primitive;
   begin
      Context.Error_Recovery_Enabled := Recovery_Enabled;
      Ignore := Eval (Context, Unit.Root);
   end Run_Standalone_Query;

   -------------------------
   -- Run_Against_Project --
   -------------------------

   procedure Run_Against_Project (LKQL_Script      : String;
                                  Project_Path     : String;
                                  Recovery_Enabled : Boolean := False)
   is
      Provider      : LAL.Unit_Provider_Reference;
      Ada_Context   : LAL.Analysis_Context;
      Env           : GPR.Project_Environment_Access;
      Project       : constant GPR.Project_Tree_Access := new GPR.Project_Tree;
      Ignore        : Primitive;
      Source_Files  : File_Array_Access;
      Project_File  : constant GNATCOLL.VFS.Virtual_File :=
        GNATCOLL.VFS.Create (+Project_Path);
   begin
      GPR.Initialize (Env);
      Project.Load (Project_File, Env);
      Provider :=
        LAL_GPR.Create_Project_Unit_Provider_Reference (Project, Env);
      Ada_Context := LAL.Create_Context (Unit_Provider => Provider);
      Source_Files := Project.Root_Project.Source_Files (Recursive => False);
      Run_On_Files (LKQL_Script, Ada_Context, Source_Files, Recovery_Enabled);
      Unchecked_Free (Source_Files);
   end Run_Against_Project;

   ------------------
   -- Run_On_Files --
   ------------------

   procedure Run_On_Files (LKQL_Script      : String;
                           Ada_Context      : LAL.Analysis_Context;
                           Files            : File_Array_Access;
                           Recovery_Enabled : Boolean := False)
   is
      Ada_Unit            : LAL.Analysis_Unit;
      Ignore              : Primitive;
      Interpreter_Context : Eval_Context;
      LKQL_Unit           : constant LEL.Analysis_Unit :=
        Make_LKQL_Unit (LKQL_Script);
   begin
      Interpreter_Context.Error_Recovery_Enabled := Recovery_Enabled;
      for F of Files.all loop
         Put_Line (F.Display_Full_Name);
         Ada_Unit := Make_Ada_Unit (Ada_Context, F.Display_Full_Name);
         Interpreter_Context.AST_Root := Ada_Unit.Root;
         Ignore := Eval (Interpreter_Context, LKQL_Unit.Root);
      end loop;
   end Run_On_Files;

   --------------------
   -- Make_LKQL_Unit --
   --------------------

   function Make_LKQL_Unit (Script_Path : String) return LEL.Analysis_Unit is
      Context : constant LEL.Analysis_Context := LEL.Create_Context;
      Unit    : constant LEL.Analysis_Unit :=
        Context.Get_From_File (Script_Path);
   begin
      if Unit.Has_Diagnostics then
         for D of Unit.Diagnostics loop
            Put_Line (Unit.Format_GNU_Diagnostic (D));
         end loop;
         GNAT.OS_Lib.OS_Exit (1);
      end if;

      return Unit;
   end Make_LKQL_Unit;

   -------------------
   -- Make_Ada_Unit --
   -------------------

   function Make_Ada_Unit
     (Context : LAL.Analysis_Context; Path : String) return LAL.Analysis_Unit
   is
      Unit : constant LAL.Analysis_Unit :=
        Context.Get_From_File (Path);
   begin
      if Unit.Has_Diagnostics then
         for D of Unit.Diagnostics loop
            Put_Line (Unit.Format_GNU_Diagnostic (D));
         end loop;
         return LAL.No_Analysis_Unit;
      end if;

      return Unit;
   end Make_Ada_Unit;

end Run;

with Libadalang.Project_Provider;

with GNATCOLL.VFS; use GNATCOLL.VFS;

package body Project_Utils is

   package LAL_GPR renames Libadalang.Project_Provider;

   ------------------
   -- Load_Project --
   ------------------

   function Load_Project (Project_Path : String;
                          Context      : out LAL.Analysis_Context)
                          return Ada_Unit_Vector
   is
      Env      : GPR.Project_Environment_Access;
      Tree     : constant GPR.Project_Tree_Access :=
        Get_Project_Tree (Project_Path, Env);
      Provider : constant LAL.Unit_Provider_Reference :=
        Get_Project_Provider_Reference (Tree, Env);
   begin
      Context := LAL.Create_Context (Unit_Provider => Provider);
      return Get_Project_Units (Tree, Context);
   end Load_Project;

   ----------------------
   -- Get_Project_Tree --
   ----------------------

   function Get_Project_Tree
     (Project_Path : String;
      Env          : in out GPR.Project_Environment_Access)
      return GPR.Project_Tree_Access
   is
      Project      : constant GPR.Project_Tree_Access := new GPR.Project_Tree;
      Project_File : constant GNATCOLL.VFS.Virtual_File :=
        GNATCOLL.VFS.Create (+Project_Path);
   begin
      GPR.Initialize (Env);
      Project.Load (Project_File, Env);
      return Project;
   end Get_Project_Tree;

   ------------------------------------
   -- Get_project_Provider_Reference --
   ------------------------------------

   function Get_Project_Provider_Reference
     (Tree : GPR.Project_Tree_Access;
      Env  : GPR.Project_Environment_Access)
      return LAL.Unit_Provider_Reference
   is
   begin
      return LAL_GPR.Create_Project_Unit_Provider_Reference (Tree, Env);
   end Get_Project_Provider_Reference;

   -----------------------
   -- Get_Project_Units --
   -----------------------

   function Get_Project_Units (Tree    : GPR.Project_Tree_Access;
                               Context : LAL.Analysis_Context)
                               return Ada_Unit_Vector
   is
      Result       : Ada_Unit_Vectors.Vector;
      Source_Files : File_Array_Access :=
        Tree.Root_Project.Source_Files;
   begin
      for F of Source_Files.all loop
         Result.Append (Context.Get_From_File (F.Display_Full_Name));
      end loop;

      Unchecked_Free (Source_Files);

      return Result;
   end Get_Project_Units;

end Project_Utils;

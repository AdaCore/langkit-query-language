with Libadalang.Analysis;

with GNATCOLL.Projects;

with Ada.Containers.Vectors;

package Project_Utils is

   package LAL renames Libadalang.Analysis;

   package GPR renames GNATCOLL.Projects;

   package Ada_Unit_Vectors is new Ada.Containers.Vectors
     (Positive, LAL.Analysis_Unit, LAL."=");

   subtype Ada_Unit_Vector is Ada_Unit_Vectors.Vector;
   --  Vector of Ada Analysis_Unit values

   function Load_Project (Project_Path : String;
                          Context      : out LAL.Analysis_Context)
                          return Ada_Unit_Vector;
   --  Return a vector containing the analysis units of the files belonging to
   --  the gpr project file at 'Project_Path'.
   --  The analysis context of the project will be stored in 'Context'.

private

   function Get_Project_Tree
     (Project_Path : String;
      Env          : in out GPR.Project_Environment_Access)
      return GPR.Project_Tree_Access;

   function Get_Project_Provider_Reference
     (Tree : GPR.Project_Tree_Access;
      Env  : GPR.Project_Environment_Access)
      return LAL.Unit_Provider_Reference;

   function Get_Project_Units (Tree    : GPR.Project_Tree_Access;
                               Context : LAL.Analysis_Context)
                               return Ada_Unit_Vector;

end Project_Utils;

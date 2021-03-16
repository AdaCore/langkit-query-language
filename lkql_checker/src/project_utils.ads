------------------------------------------------------------------------------
--                                                                          --
--                                   LKQL                                   --
--                                                                          --
--                     Copyright (C) 2019-2021, AdaCore                     --
--                                                                          --
-- LKQL is free software;  you can redistribute it and/or modify  it        --
-- under terms of the GNU General Public License  as published by the Free  --
-- Software Foundation;  either version 3,  or (at your option)  any later  --
-- version.   This  software  is distributed in the hope that it  will  be  --
-- useful but  WITHOUT ANY WARRANTY;  without even the implied warranty of  --
-- MERCHANTABILITY  or  FITNESS  FOR  A PARTICULAR PURPOSE.                 --
--                                                                          --
-- As a special  exception  under  Section 7  of  GPL  version 3,  you are  --
-- granted additional  permissions described in the  GCC  Runtime  Library  --
-- Exception, version 3.1, as published by the Free Software Foundation.    --
--                                                                          --
-- You should have received a copy of the GNU General Public License and a  --
-- copy of the GCC Runtime Library Exception along with this program;  see  --
-- the files COPYING3 and COPYING.RUNTIME respectively.  If not, see        --
-- <http://www.gnu.org/licenses/>.                                          --
------------------------------------------------------------------------------

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

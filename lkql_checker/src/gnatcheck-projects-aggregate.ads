------------------------------------------------------------------------------
--                                                                          --
--                                 GNATCHECK                                --
--                                                                          --
--                     Copyright (C) 2018-2023, AdaCore                     --
--                                                                          --
-- GNATCHECK  is free software;  you can redistribute it and/or modify  it  --
-- under terms of the GNU General Public License  as published by the Free  --
-- Software Foundation;  either version 3,  or (at your option)  any later  --
-- version.   This  software  is distributed in the hope that it  will  be  --
-- useful but  WITHOUT ANY WARRANTY;  without even the implied warranty of  --
-- MERCHANTABILITY  or  FITNESS  FOR  A PARTICULAR PURPOSE.                 --
--                                                                          --
-- You should have received a copy of the GNU General Public License and a  --
-- copy of the GCC Runtime Library Exception along with this program;  see  --
-- the files COPYING3 and COPYING.RUNTIME respectively.  If not, see        --
-- <http://www.gnu.org/licenses/>.                                          --
------------------------------------------------------------------------------

--  This package contains high-level interfaces to project files needed to
--  process aggregate projects.
--
--  There are two different cases here:
--
--  * Simple case  - aggregate project aggregates only one (non-aggregate)
--                   project that has sources. This case from the point of a
--                   tool does not differ from processing of non-aggregate
--                   project, except that when processing a project file, a
--                   tool should unload an aggregate project and load (the
--                   only) project with sources
--
--  * Complex case - aggregate project aggregates more than one project that
--                   has (or, more precisely, may have) sources. Processing of
--                   this case is described in the body of this package.

with GPR2.Path_Name;
with GPR2.Project.View;

package Gnatcheck.Projects.Aggregate is

   procedure Store_Aggregated_Project (S : String);
   function Get_Aggregated_Project return String;
   --  Stores and returns the name of the aggregated project file passed as an
   --  actual for '-A ' option.

   procedure Collect_Aggregated_Projects (P : GPR2.Project.View.Object);
   --  Stores (in internal data structures) the full paths to the
   --  (non-aggregate!) projects that have been aggregated by P

   function Num_Of_Aggregated_Projects return Natural;
   --  Returns the number of (non-aggregate) projects being aggregated. We
   --  need to know this because the case when an aggregate project aggregates
   --  only one project is a useful special case.

   function Get_Aggregated_Prj_Src return GPR2.Path_Name.Object with
      Pre => Num_Of_Aggregated_Projects = 1;
   --  Returns the (single!) aggregate project source file

   -----------------------
   --  Project iterator --
   -----------------------

   --  This iterator should be used when all the (non-aggregate) projects are
   --  stored in internal data structures. This iterator is to be used to
   --  iterate through these projects, each of these projects is loaded and
   --  analyzed, and all the extracted data is stored in internal database.

   procedure Start_Prj_Iterator with
     Pre => Num_Of_Aggregated_Projects > 1;
   --  Initializes the iterator and sets it to the first project stored in the
   --  database

   function Next_Prj_Name return GPR2.Path_Name.Full_Name;
   --  Gets the full name of the project file the iterator points onto

   procedure Prj_Iterator_Next;
   --  Moves iterator forward.

   function Prj_Iterator_Done return Boolean;
   --  Tells that iterator has passed through all the projects stored in the
   --  database

   --  The routines below updates or queries the information related to the
   --  project being accessed during iteration

   procedure Process_Aggregated_Projects
     (My_Project : Arg_Project_Type'Class) with
      Pre => Num_Of_Aggregated_Projects > 1;
   --  Iterates through the projects being aggregated and spawns gnatcheck
   --  for each of them.
   --  This will create one specific report for each project being aggregated
   --  and an umbrella report that lists all the (non-aggregate) project that
   --  have been processed and corresponding report files.

end Gnatcheck.Projects.Aggregate;

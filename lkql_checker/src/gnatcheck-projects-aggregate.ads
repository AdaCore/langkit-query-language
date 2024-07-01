--
--  Copyright (C) 2005-2024, AdaCore
--  SPDX-License-Identifier: GPL-3.0-or-later
--

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
with GPR2.Project.Tree;

package Gnatcheck.Projects.Aggregate is

   procedure Store_Aggregated_Project (S : String);
   function Get_Aggregated_Project return String;
   --  Stores and returns the name of the aggregated project file passed as an
   --  actual for '-A ' option.

   procedure Collect_Aggregated_Projects (Tree : GPR2.Project.Tree.Object);
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

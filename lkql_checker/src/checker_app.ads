------------------------------------------------------------------------------
--                                                                          --
--                                   LKQL                                   --
--                                                                          --
--                     Copyright (C) 2019-2023, AdaCore                     --
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

with Libadalang.Analysis; use Libadalang.Analysis;

with Rules_Factory; use Rules_Factory;

with Langkit_Support.Generic_API.Analysis;
with Langkit_Support.Generic_API.Introspection;

--  TODO: Factor into another package, check what is dead. See #190
package Checker_App is

   package LK renames Langkit_Support.Generic_API.Analysis;
   package LKI renames Langkit_Support.Generic_API.Introspection;

   type Rules_By_Kind_Array
   is array (LKI.Any_Type_Index range <>) of Rule_Vector;

   type Rules_By_Kind is access all Rules_By_Kind_Array;

   type Lkql_Context is record
      Analysis_Ctx : Analysis_Context;
      LKQL_Analysis_Context : L.Analysis_Context;

      Cached_Rules : Rules_By_Kind := null;
      --  Data structure mapping node kinds to the checks that should be ran
      --  when this node type is encountered.

      All_Rules : Rule_Vector;
      --  All known rules

      Traverse_Instantiations : Boolean := False;
      --  Whether we should traverse generic instantiations. This will be set
      --  to true if there is at least one rule in the active set of rules that
      --  requires it. This is used so we don't traverse generic instantiations
      --  if no rule requires it.
   end record;
   --  Context giving access to all the "global" data structures for an LKQL
   --  analysis.

   type Lkql_Context_Access is access all Lkql_Context;
   --  Access to an LKQL context

   type Message_Kinds is
     (Rule_Violation, Internal_Error, Severe_Internal_Error);
   --  Rule_Violation: a rule is flagged
   --  Internal_Error: an internal error occurred
   --  Severe_Internal_Error: a severe internal error occurred which should
   --  not be hidden.

end Checker_App;

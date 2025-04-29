--
--  Copyright (C) 2005-2025, AdaCore
--  SPDX-License-Identifier: GPL-3.0-or-later
--

with Langkit_Support.Generic_API.Analysis;
with Langkit_Support.Generic_API.Introspection;

with Libadalang.Analysis; use Libadalang.Analysis;

with Rules_Factory; use Rules_Factory;

--  TODO: Factor into another package, check what is dead. See #190

package Checker_App is

   package LK renames Langkit_Support.Generic_API.Analysis;
   package LKI renames Langkit_Support.Generic_API.Introspection;

   type Rules_By_Kind_Array is
     array (LKI.Any_Type_Index range <>) of Rule_Vector;

   type Rules_By_Kind is access all Rules_By_Kind_Array;

   type Lkql_Context is record
      Analysis_Ctx          : Analysis_Context;
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

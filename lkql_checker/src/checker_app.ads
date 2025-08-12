--
--  Copyright (C) 2005-2025, AdaCore
--  SPDX-License-Identifier: GPL-3.0-or-later
--

with Libadalang.Analysis; use Libadalang.Analysis;

with Rules_Factory; use Rules_Factory;

package Checker_App is

   type Lkql_Context is record
      Analysis_Ctx          : Analysis_Context;
      LKQL_Analysis_Context : L.Analysis_Context;

      All_Rules : Rule_Vector;
      --  All known rules
   end record;
   --  Context giving access to all the "global" data structures for an LKQL
   --  analysis.

end Checker_App;

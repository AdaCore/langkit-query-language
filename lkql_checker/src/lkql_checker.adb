--
--  Copyright (C) 2005-2025, AdaCore
--  SPDX-License-Identifier: GPL-3.0-or-later
--

--  This is the top of the Lkql_Checker hierarchy.

package body Lkql_Checker is
   --------------------------
   --  Lkql_Checker_Image  --
   --------------------------
   function Lkql_Checker_Mode_Image return String
   is (Lkql_Checker_Mode_Name (Mode));

   ------------------------------
   --  Lkql_Checker_Mode_Name  --
   ------------------------------

   function Lkql_Checker_Mode_Name (Mode : Lkql_Checker_Mode) return String is
   begin
      return
        (case Mode is
           when Gnatcheck_Mode => "gnatcheck",
           when Gnatkp_Mode    => "gnatkp");
   end Lkql_Checker_Mode_Name;
end Lkql_Checker;

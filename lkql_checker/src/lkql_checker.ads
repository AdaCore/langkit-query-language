--
--  Copyright (C) 2005-2025, AdaCore
--  SPDX-License-Identifier: GPL-3.0-or-later
--

--  This is the top of the Lkql_Checker hierarchy.

package Lkql_Checker is
   type Lkql_Checker_Mode is (Gnatcheck_Mode, Gnatkp_Mode);

   Mode : Lkql_Checker_Mode;
   --  The mode of the driver, either GNATcheck or GNATkp.

   function Lkql_Checker_Mode_Image return String;
   --  Get the Lkql_Checker_Mode image.
   --
   --  TODO: Use the Put_Image attribute for Lkql_Checker_Mode instead when
   --  switching to Ada_2022.

   procedure Main (Mode : Lkql_Checker_Mode);
   --  Main entry point to Lkql_Checker.
private
   function Lkql_Checker_Mode_Name (Mode : Lkql_Checker_Mode) return String;
   --  Return the name associated to the given Mode.
end Lkql_Checker;

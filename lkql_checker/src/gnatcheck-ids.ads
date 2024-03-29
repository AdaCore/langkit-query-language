--
--  Copyright (C) 2005-2024, AdaCore
--  SPDX-License-Identifier: GPL-3.0-or-later
--

--  This package defines ID types for rule table and category table. We define
--  ID types in a separate package because both rule type and category type
--  needs both kinds of IDs

package Gnatcheck.Ids is

   -------------
   -- Rule ID --
   -------------

   type Rule_Id is new Natural;
   No_Rule    : constant Rule_Id := Rule_Id'First;

   --  Fake Ids for compiler checks:
   Restrictions_Id : constant Rule_Id := No_Rule + 1;
   Style_Checks_Id : constant Rule_Id := Restrictions_Id + 1;
   Warnings_Id     : constant Rule_Id := Style_Checks_Id + 1;

   subtype Compiler_Checks is Rule_Id range Restrictions_Id ..  Warnings_Id;

   First_Compiler_Check : constant := Restrictions_Id;
   First_Rule           : constant Rule_Id := Warnings_Id + 1;

end Gnatcheck.Ids;

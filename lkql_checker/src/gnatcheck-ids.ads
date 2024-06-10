--
--  Copyright (C) 2005-2024, AdaCore
--  SPDX-License-Identifier: GPL-3.0-or-later
--

--  This package defines ID type for rule map.

with Ada.Containers;
with Ada.Containers.Indefinite_Vectors;

with Langkit_Support.Symbols; use Langkit_Support.Symbols;
with Langkit_Support.Text;    use Langkit_Support.Text;

package Gnatcheck.Ids is

   -----------------------------------
   -- Internal rule id symbol table --
   -----------------------------------

   All_Rule_Ids : constant Langkit_Support.Symbols.Symbol_Table :=
     Create_Symbol_Table;
   --  This table contains all created rule identifiers

   ------------------
   -- Rule id type --
   ------------------

   type Rule_Id is new Thin_Symbol;
   --  Define the rule identifier as a thin symbol. The symbol belongs to
   --  an internal symbol table, which contains all created rule ids. A rule
   --  identifier represents the normalizd name of the rule.

   package Rule_Id_Vec is new Ada.Containers.Indefinite_Vectors
     (Index_Type => Natural, Element_Type => Rule_Id);
   --  A simple vector to store rule identifiers

   ---------------------------
   -- Operation on rule ids --
   ---------------------------

   function Find_Rule_Id (Rule_Name : String) return Rule_Id;
   --  This function maps the `Find` function of `Langkit_Support.Symbols`
   --  to the `Rule_Id` type, allowing user to get or create a rule identifier
   --  from a simple string. Note that the input string is normalized before
   --  the getting or creation of the rule identifier.

   function Get_Id_Text (Id : Rule_Id) return String;
   --  Get the text associated with the given `Rule_Id`, this text represents
   --  the normalized name of the rule identified by `Id`.

   function Is_Compiler_Rule (Id : Rule_Id) return Boolean;
   --  Get whether the given rule identifier represents a compiler-based check

   function Hash (Id : Rule_Id) return Ada.Containers.Hash_Type is
     (Hash (To_Symbol (All_Rule_Ids, Thin_Symbol (Id))));
   --  Shortcut fonction to hash a rule identifier

   ------------------
   -- Constant ids --
   ------------------

   No_Rule_Id : constant Rule_Id := Find (All_Rule_Ids, To_Text (""));
   --  The empty identifier, used to express "no rule"

   Restrictions_Id : constant Rule_Id :=
     Find (All_Rule_Ids, To_Text ("restrictions"));
   Style_Checks_Id : constant Rule_Id :=
     Find (All_Rule_Ids, To_Text ("style_checks"));
   Warnings_Id : constant Rule_Id :=
     Find (All_Rule_Ids, To_Text ("warnings"));
   --  Fake identifiers, used to represent the compiler checks

end Gnatcheck.Ids;

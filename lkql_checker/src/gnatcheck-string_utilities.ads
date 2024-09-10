--
--  Copyright (C) 2005-2024, AdaCore
--  SPDX-License-Identifier: GPL-3.0-or-later
--

with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Containers.Indefinite_Hashed_Sets;
with Ada.Containers.Indefinite_Vectors;
with Ada.Strings.Hash;

with GNAT.OS_Lib; use GNAT.OS_Lib;

with GNATCOLL.Utils; use GNATCOLL.Utils;

package Gnatcheck.String_Utilities is

   --  String-related utilities

   function Image (X : Integer) return String;
   --  Return X'Img without the annoying blank.

   type Modular is mod 2**32;
   function Image (X : Modular) return String;

   function Capitalize (S : String) return String;
   --  Capitalizes the first letter, and all letters following a
   --  non-letter-or-digit. Converts all others to lower case.

   function Slide (X : String) return String;
   --  Return X with X'First = 1

   function Has_Prefix (X, Prefix : String) return Boolean;
   --  True if Prefix is at the beginning of X, case insensitive. For example,
   --  Has_Prefix("An_Identifier", Prefix => "an_") is True.

   function Has_Suffix (X, Suffix : String) return Boolean;
   --  True if Suffix is at the end of X, case insensitive

   function Remove_Spaces (S : String) return String;
   --  Removes all the white spaces from the argument

   function Remove_Quotes (S : String) return String;
   --  Removes surrounding quotes or double-quotes from the provided string
   --  if any, else just return the string.

   function Escape_Quotes (S : String) return String is
     (Replace (S, """", "\"""));
   --  Escape all quotes in the given string by adding a '\` before each of
   --  them.

   function Is_White_Space (Ch : Character) return Boolean is
     (Ch in ' ' | ASCII.HT);
   --  Checks if the argument is either a space or HT character

   function Read_File (FD : File_Descriptor) return String_Access;
   function Read_File (File_Name : String) return String_Access;
   --  Reads the entire contents of the file

   package String_Vectors is
     new Ada.Containers.Indefinite_Vectors (Positive, String);
   subtype String_Vector is String_Vectors.Vector;

   function Split
     (S          : String;
      Sep        : Character;
      Trim_Elems : Boolean := False) return String_Vector;
   --  Split the given ``S`` into multiple pieces, separating those when
   --  encountering ``Sep``.
   --  ``Trim_Elems`` sets whether this method should trim heading and trailing
   --  whitespaces from the result elements.

   function Join (V : String_Vector; Sep : String) return String;
   --  Join all element of the given vector `V` with `Sep`.

   ------------------------------
   -- Simple String dictionary --
   ------------------------------

   generic
      Dictionary_Name : String;
   package Simple_String_Dictionary is
      --  This package defines a simple string dictionary. The dictionary
      --  entries are various strings, each string can be included in the
      --  dictionary only once. The dictionary is not case-sensitive. The
      --  initial state of the dictionary is empty

      procedure Add_To_Dictionary (S : String);
      --  If the dictionary does not contain S, adds S to the dictionary,
      --  Otherwise does nothing.

      procedure Remove_From_Dictionary (S : String);
      --  If the dictionary does not contain S, removes S from the dictionary,
      --  Otherwise does nothing.

      function Is_In_Dictionary (S : String) return Boolean;
      --  Checks if S is in the dictionary

      function Is_Empty return Boolean;
      --  Returns True if the dictionary contains no entries, otherwise returns
      --  False

      procedure Clear;
      --  Removes all the entries from the dictionary

      procedure Reset_Iterator;
      function Next_Entry return String;
      function Done return Boolean;
      --  These three routines implement iterator that allows to get all the
      --  dictionary entries. If a client adds or removes entries to/from a
      --  dictionary while using the iterator, the iterator behavior is
      --  erroneous.

      procedure Print_Dictionary;
      --  Prints into Stderr the content of the dictionary. Each entry is
      --  printed on a new line and is surrounded by ">>" and "<<". (To be
      --  used for debugging purposes).

   end Simple_String_Dictionary;

   ---------------------------------
   -- Simple String to String map --
   ---------------------------------

   package String_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => String,
      Element_Type    => String,
      Hash            => Ada.Strings.Hash,
      Equivalent_Keys => "=");

   -----------------------
   -- Simple String set --
   -----------------------

   package String_Sets is new Ada.Containers.Indefinite_Hashed_Sets
     (Element_Type        => String,
      Hash                => Ada.Strings.Hash,
      Equivalent_Elements => "=",
      "="                 => "=");

end Gnatcheck.String_Utilities;

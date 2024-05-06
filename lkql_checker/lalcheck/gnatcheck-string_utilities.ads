------------------------------------------------------------------------------
--                                                                          --
--                                 GNATCHECK                                --
--                                                                          --
--                     Copyright (C) 2004-2023, AdaCore                     --
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

with Ada.Containers.Indefinite_Vectors;

with GNAT.OS_Lib; use GNAT.OS_Lib;

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

   function Is_White_Space (Ch : Character) return Boolean is
     (Ch in ' ' | ASCII.HT);
   --  Checks if the argument is either a space or HT character

   function Read_File (FD : File_Descriptor) return String_Access;
   function Read_File (File_Name : String) return String_Access;
   --  Reads the entire contents of the file

   package String_Vectors is
     new Ada.Containers.Indefinite_Vectors (Positive, String);
   subtype String_Vector is String_Vectors.Vector;

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

end Gnatcheck.String_Utilities;

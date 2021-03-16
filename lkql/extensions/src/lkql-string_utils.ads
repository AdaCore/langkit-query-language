------------------------------------------------------------------------------
--                                                                          --
--                                   LKQL                                   --
--                                                                          --
--                     Copyright (C) 2019-2021, AdaCore                     --
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

with Langkit_Support.Text; use Langkit_Support.Text;

with Ada.Containers.Hashed_Sets;
with Ada.Containers.Indefinite_Vectors; use Ada.Containers;
with Ada.Strings.Wide_Wide_Unbounded;   use Ada.Strings.Wide_Wide_Unbounded;
with System;

package LKQL.String_Utils is

   package String_Vectors is new
     Indefinite_Vectors (Positive, Unbounded_Text_Type);

   subtype String_Vector is String_Vectors.Vector;
   --  Vector of Unbouted_Text_type values

   package Symbol_Sets is new Ada.Containers.Hashed_Sets
     (Element_Type        => Symbol_Type,
      Hash                => Hash,
      Equivalent_Elements => "=");

   subtype Symbol_Set is Symbol_Sets.Set;
   --  Set of Unbounded_Text_Type values

   function Split_Lines (Str : Text_Type) return String_Vectors.Vector;
   --  Return a list of the lines in the given string

   function Underline_Range (Line  : Unbounded_Text_Type;
                             Start : Positive;
                             Stop  : Positive) return Unbounded_Text_Type;
   --  Return Line plus a new line character and a sequence characters so that
   --  all non-whitespace columns between `Start` and `Stop` (included) are '^'
   --  and the other columns are spaces.
   --
   --  The input must contain a single line of text.

   function Underline_From (Line  : Unbounded_Text_Type;
                            Start : Positive) return Unbounded_Text_Type;
   --  Return Line plus a new line character and a sequence characters so that
   --  all non-whitespace columns from `Start` to the end of the line are
   --  '^' and the other columns are spaces.
   --
   --  The input must contain a single line of text.

   function Underline_To (Line : Unbounded_Text_Type;
                          Stop : Positive) return Unbounded_Text_Type;
   --  Return Line plus a new line character and a sequence characters so that
   --  all non-whitespace columns from the beginning of the line to `Stop` are
   --  '^' and the other columns are spaces.
   --
   --  The input must contain a single line of text.

   function Underline (Line : Unbounded_Text_Type) return Unbounded_Text_Type;
   --  Underline all the text in the input String. The input must contain a
   --  single line of text.

   function Address_Image (Addr : System.Address) return String;
   --  "Better" version of ``System.Address_Image``, that will strip leading
   --  zeros, and put hex letters in lowercase, yielding shorter & more
   --  readable addresses.

end LKQL.String_Utils;

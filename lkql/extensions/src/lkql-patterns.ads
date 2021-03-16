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

with LKQL.Primitives;    use LKQL.Primitives;

private package LKQL.Patterns is

   use Primitive_Options;

   type Match_Result is tagged record
      Matched_Value  : Primitive_Option;
      --  True if the matching attempt succeeded
   end record;
   --  Represents that result of a matching attempt

   function Make_Match_Success
     (Matched_Value : Primitive)
      return Match_Result;
   --  Create a Match_Result value representing a successful matching attempt
   --  with the given binding(s).

   function Is_Success (Self : Match_Result) return Boolean;
   --  Return whether the match was successful

   function Get_Matched_Value (Self : Match_Result) return Primitive
     with Pre => Self.Is_Success;
   --  If the match was successful, return the matched value.
   --  Otherwise, raise an exception.

   Match_Failure : constant Match_Result :=
     Match_Result'(Matched_Value => None);
   --  Special value representing the failure of a matching attempt

   subtype Match_Index is Integer range Positive'First - 1 .. Positive'Last;

   type Match_Array_Result is record
      Matched_Value : Primitive_Option;
      --  If the match was successful, stores the matched value
      Index    :  Match_Index := Match_Index'First;
      --  Index of the first matched pattern
   end record;
   --  Represents the result of a matching attempt against a sequence of
   --  patterns.

end LKQL.Patterns;

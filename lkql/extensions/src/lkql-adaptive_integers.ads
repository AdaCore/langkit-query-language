------------------------------------------------------------------------------
--                                                                          --
--                                   LKQL                                   --
--                                                                          --
--                     Copyright (C) 2019-2022, AdaCore                     --
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

with LKQL.Big_Integers; use LKQL.Big_Integers;

package LKQL.Adaptive_Integers is

   --  This package provides an implementation for adaptive integers, that is,
   --  integers that adapt their internal representation depending on the kind
   --  of number they represent.
   --
   --  Integers between -Standard.Integer'First and Standard.Integer'Last are
   --  represented using a Standard.Integer value meaning that operating on
   --  this range of values is fast and has a low memory footprint.
   --
   --  Integers below or above the previous range are represented using
   --  LKQL.Big_Integers (as of now: GNATCOLL.GMP.Integers.Big_Integer).
   --
   --  Note:
   --
   --  - Operations between small ints that does not overflow always returns
   --    a small int.
   --
   --  - Operations in which one of the operand is a big int will always
   --    return a big int (even if the result could be held in a small int).

   type Adaptive_Integer is private;

   function Create (Value : Integer) return Adaptive_Integer;
   function Create (Value : String) return Adaptive_Integer;

   function Image (Int : Adaptive_Integer) return String;

   function "+" (Int : Adaptive_Integer) return Integer;

   function "=" (L, R : Adaptive_Integer) return Boolean;
   function "<" (L, R : Adaptive_Integer) return Boolean;
   function "<=" (L, R : Adaptive_Integer) return Boolean;
   function ">" (L, R : Adaptive_Integer) return Boolean;
   function ">=" (L, R : Adaptive_Integer) return Boolean;

   function "-" (L : Adaptive_Integer) return Adaptive_Integer;

   function "+" (L, R : Adaptive_Integer) return Adaptive_Integer;
   function "-" (L, R : Adaptive_Integer) return Adaptive_Integer;
   function "*" (L, R : Adaptive_Integer) return Adaptive_Integer;
   function "/" (L, R : Adaptive_Integer) return Adaptive_Integer;

   Zero : constant Adaptive_Integer;

private

   type Internal_Kind is (Small, Big);

   type Adaptive_Integer (Kind : Internal_Kind := Small) is record
      case Kind is
         when Small =>
            Small_Value : Integer;
         when Big =>
            Big_Value : Big_Integer;
      end case;
   end record;

   Zero : constant Adaptive_Integer := (Small, 0);
end LKQL.Adaptive_Integers;

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

with Ada.Finalization;

with GNATCOLL.GMP.Integers;
use GNATCOLL;

package LKQL.Big_Integers is
   --  This package follows the exact same API as the Ada202x
   --  `Ada.Numerics.Big_Numbers.Big_Integers` package. Once that package is
   --  available, this package can be completely removed, and the transition
   --  can be done simply and safely by replacing all occurrences of
   --  `LKQL.Integers` by `Ada.Numerics.Big_Numbers.Big_Integers`.

   type Big_Integer is private;

   function To_Big_Integer (Value : Integer) return Big_Integer;
   function To_Integer (Value : Big_Integer) return Integer;

   function From_String (Value : String) return Big_Integer;
   function To_String   (Value : Big_Integer) return String;

   function "=" (L, R : Big_Integer) return Boolean;
   function "<" (L, R : Big_Integer) return Boolean;
   function "<=" (L, R : Big_Integer) return Boolean;
   function ">" (L, R : Big_Integer) return Boolean;
   function ">=" (L, R : Big_Integer) return Boolean;

   function In_Range (Value, Lo, Hi : Big_Integer) return Boolean;

   function "-" (L : Big_Integer) return Big_Integer;

   function "+" (L, R : Big_Integer) return Big_Integer;
   function "-" (L, R : Big_Integer) return Big_Integer;
   function "*" (L, R : Big_Integer) return Big_Integer;
   function "/" (L, R : Big_Integer) return Big_Integer;

private

   subtype GMP_Big_Integer is GMP.Integers.Big_Integer;

   type Big_Integer_Record is limited record
      Value     : GMP_Big_Integer;
      Ref_Count : Natural;
   end record;

   type Big_Integer_Record_Access is access Big_Integer_Record;

   type Big_Integer is new Ada.Finalization.Controlled with record
      Value     : Big_Integer_Record_Access := null;
   end record;

   overriding procedure Adjust (X : in out Big_Integer);
   overriding procedure Finalize (X : in out Big_Integer);
end LKQL.Big_Integers;

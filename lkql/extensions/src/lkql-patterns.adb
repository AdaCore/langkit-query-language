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

package body LKQL.Patterns is

   ----------------
   -- Is_Success --
   ----------------

   function Is_Success (Self : Match_Result) return Boolean is
     (Is_Some (Self.Matched_Value));

   -----------------------
   -- Get_Matched_Value --
   -----------------------

   function Get_Matched_Value (Self : Match_Result) return Primitive is
      (Extract (Self.Matched_Value));

   ------------------------
   -- Make_Match_Success --
   ------------------------

   function Make_Match_Success
     (Matched_Value : Primitive)
      return Match_Result
   is
      (Match_Result'(Matched_Value => To_Option (Matched_Value)));

end LKQL.Patterns;

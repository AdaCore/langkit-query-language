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

package body Options is

   -------------
   -- Is_Some --
   -------------

   function Is_Some (Self : Option) return Boolean is (Self.Kind = Kind_Some);

   -------------
   -- Is_None --
   -------------

   function Is_None (Self : Option) return Boolean is (Self.Kind = Kind_None);

   -------------
   -- Extract --
   -------------

   function Extract (Self : Option) return Value_Type is (Self.Value);

   ---------------
   -- To_Option --
   ---------------

   function To_Option (Value : Value_Type) return Option is
      (Option'(Kind_Some, Value));

   ----------
   -- None --
   ----------

   function None return Option is (Option'(Kind => Kind_None));

end Options;

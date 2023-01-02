------------------------------------------------------------------------------
--                                                                          --
--                                   LKQL                                   --
--                                                                          --
--                     Copyright (C) 2019-2023, AdaCore                     --
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

--  This package provides a generic 'Option' type, meant to represent optional
--  values.

generic

   type Value_Type is private;
   --  Type of the optional value

package Options is

   type Option_Kind is
     (Kind_None,
      --  The Option doesn't contains a value
      Kind_Some
      --  The Option contains a value
     );

   type Option (Kind : Option_Kind := Kind_None) is private;
   --  Stores an optional value

   function Is_Some (Self : Option) return Boolean;
   --  Return True if the option contains a value

   function Is_None (Self : Option) return Boolean;
   --  Return True if the option doesn't contain a value

   function Extract (Self : Option) return Value_Type
     with Pre => Self.Kind = Kind_Some;
   --  Return the wrapped value, if any.
   --  An Assertion_Error will be raised if there is no value.

   function To_Option (Value : Value_Type) return Option;
   --  Create an Option value that contains the given value

   function None return Option;
   --  Create an Option that doesn't contain a value

private

   type Option (Kind : Option_Kind := Kind_None) is record
      case Kind is
         when Kind_None =>
            null;
         when Kind_Some =>
            Value : Value_Type;
      end case;
   end record;

end Options;

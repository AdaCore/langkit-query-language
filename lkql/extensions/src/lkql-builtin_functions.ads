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

with LKQL.Primitives; use LKQL.Primitives;

--  This package declares every function that is a builtin in LKQL, and a
--  function that returns the array of builtin function descriptors that
--  lists all the functions and their name in LKQL.
--
--  To add a new built-in function, you must:
--
--  1. Add a new function with the correct prototype in that package
--
--  2. Register it in the list of built-ins (The ``Builtin_Functions`` array
--     in the body of this package).

package LKQL.Builtin_Functions is

   type Builtin_Function_Array is
      array (Positive range <>) of Builtin_Function;

   function All_Builtins return Builtin_Function_Array;

end LKQL.Builtin_Functions;

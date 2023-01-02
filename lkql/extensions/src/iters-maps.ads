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

with Funcs;
with Iters.Iterators;

generic

   with package Input_Iterators is new Iters.Iterators (<>);
   --  Wrapped iterators

   with package Output_Iterators is new Iters.Iterators (<>);
   --  Wrapped iterators

package Iters.Maps is

   subtype Result_Type is Output_Iterators.Element_Type;

   package Map_Funcs is new Funcs (Input_Iterators.Element_Type, Result_Type);
   --  Abstraction representing a function that takes values from the input
   --  iterator and returns values of type Return_Type.

   subtype Map_Func is Map_Funcs.Func;

   package Predicates renames Output_Iterators.Predicates;

   type Map_Iter is new Output_Iterators.Iterator_Interface with private;
   --  Iterator that maps a function over the elements of a given iterator

   overriding function Next (Iter   : in out Map_Iter;
                             Result : out Result_Type) return Boolean;

   overriding function Clone (Iter : Map_Iter) return Map_Iter;

   overriding procedure Release (Iter : in out Map_Iter);

   function Map (Input : Input_Iterators.Iterator_Access;
                 Fn    : Map_Funcs.Func_Access) return Map_Iter;

   function Map (Input : Input_Iterators.Iterator_Interface'Class;
                 Fn    : Map_Funcs.Func'Class) return Map_Iter;

private

   type Map_Iter is new Output_Iterators.Iterator_Interface with record
      Inner : Input_Iterators.Iterator_Access;
      Fn    : Map_Funcs.Func_Access;
   end record;

end Iters.Maps;

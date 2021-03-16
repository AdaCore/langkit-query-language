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

with Iters.Iterators;

with Ada.Containers.Vectors;
with Ada.Unchecked_Deallocation;

generic

   with package Vectors is new Ada.Containers.Vectors (<>);
   --  Type of vectors that we want to iterate over

   with package Iterators is new Iters.Iterators (Vectors.Element_Type);
   --  Type of iterators in which the vector will be wrapped

package Iters.Vec_Iterators is

   type Vec_Iterator is new Iterators.Iterator_Interface with private;
   --  Iterator that yields the values contained in a vector

   overriding function Next (Iter   : in out Vec_Iterator;
                             Result : out Vectors.Element_Type) return Boolean;
   --  Get the next iteration element. If all the values from the vector have
   --  already been yielded, return false. Otherwise, return true and set
   --  Result.

   overriding function Clone (Iter : Vec_Iterator) return Vec_Iterator;
   --  Make a deep copy of the iterator

   overriding procedure Release (Iter : in out Vec_Iterator);
   --  Release resources that belong to Iter

   function To_Iterator (Vec : Vectors.Vector) return Vec_Iterator;
   --  Create a Vec_Iterator that wraps 'Vec'

private

   type Vec_Access is access all Vectors.Vector;

   procedure Free_Vec_Access is new Ada.Unchecked_Deallocation
     (Vectors.Vector, Vec_Access);

   type Vec_Iterator is new Iterators.Iterator_Interface with record
      Elements : Vec_Access;
      Next_Element_Index : Vectors.Extended_Index;
   end record;

end Iters.Vec_Iterators;

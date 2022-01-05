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

package body Iters.Vec_Iterators is

   ----------
   -- Next --
   ----------

   overriding function Next (Iter   : in out Vec_Iterator;
                             Result : out Vectors.Element_Type) return Boolean
   is
      Cursor : constant Vectors.Cursor :=
        Iter.Elements.To_Cursor (Iter.Next_Element_Index);
   begin
      if not Vectors.Has_Element (Cursor) then
         return False;
      end if;

      Result := Vectors.Element (Cursor);
      Iter.Next_Element_Index :=
        Vectors.Extended_Index'Succ (Iter.Next_Element_Index);
      return True;
   end Next;

   -----------
   -- Clone --
   -----------

   overriding function Clone (Iter : Vec_Iterator) return Vec_Iterator is
      Elements_Copy : constant Vec_Access :=
        new Vectors.Vector'(Iter.Elements.all);
   begin
      return Vec_Iterator'(Elements_Copy, Iter.Next_Element_Index);
   end Clone;

   -------------
   -- Release --
   -------------

   overriding procedure Release (Iter : in out Vec_Iterator) is
   begin
      Free_Vec_Access (Iter.Elements);
   end Release;

   -----------------
   -- To_Iterator --
   -----------------

   function To_Iterator (Vec : Vectors.Vector) return Vec_Iterator is
      (Vec_Iterator'(new Vectors.Vector'(Vec), Vec.First_Index));

end Iters.Vec_Iterators;

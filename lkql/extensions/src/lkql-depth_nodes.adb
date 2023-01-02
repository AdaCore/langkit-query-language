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

with Ada.Containers; use Ada.Containers;
with Ada.Unchecked_Deallocation;

package body LKQL.Depth_Nodes is

   ----------
   -- Hash --
   ----------

   function Hash (Value : Depth_Node) return Ada.Containers.Hash_Type is
    (LK.Hash (Value.Node));

   ----------
   -- Next --
   ----------

   overriding function Next
     (Iter   : in out Depth_Node_Lk_Node_Iterator;
      Result : out LK.Lk_Node) return Boolean
   is
      D   : Depth_Node;
      Res : Boolean;
   begin
      Res := Iter.Internal.Next (D);
      if Res then
         Result := D.Node;
         return True;
      end if;
      return False;
   end Next;

   -----------
   -- Clone --
   -----------

   overriding function Clone
     (Iter : Depth_Node_Lk_Node_Iterator) return Depth_Node_Lk_Node_Iterator is
   begin
      raise Constraint_Error with "not implemented";
      return Depth_Node_Lk_Node_Iterator'
        (Internal => null);
   end Clone;

   -------------
   -- Release --
   -------------

   overriding procedure Release (Iter : in out Depth_Node_Lk_Node_Iterator) is
      procedure Free is new Ada.Unchecked_Deallocation
        (Depth_Node_Iter'Class, Depth_Node_Iter_Access);
   begin
      Iter.Internal.Release;
      Free (Iter.Internal);
   end Release;

   -------------------------
   -- To_Lk_Node_Iterator --
   -------------------------

   function To_Lk_Node_Iterator
     (Self : Depth_Node_Iter'Class) return Lk_Node_Iterator'Class is
   begin
      return Depth_Node_Lk_Node_Iterator'
        (Internal => new Depth_Node_Iter'Class'(Self));
   end To_Lk_Node_Iterator;

end LKQL.Depth_Nodes;

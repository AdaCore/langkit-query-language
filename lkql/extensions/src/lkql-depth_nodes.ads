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

with Ada.Containers;
with Ada.Containers.Vectors;

with Langkit_Support.Generic_API.Analysis;

with Options;
with Iters.Iterators;
with LKQL.Lk_Nodes_Iterators; use LKQL.Lk_Nodes_Iterators;

package LKQL.Depth_Nodes is

   package LK renames Langkit_Support.Generic_API.Analysis;

   type Depth_Node is record
      Depth : Natural;
      Node  : LK.Lk_Node;
   end record;
   --  Depth-mapped AST node used in (and returned by) selectors

   package Depth_Node_Vectors is new Ada.Containers.Vectors
     (Positive, Depth_Node);
   --  Vectors of Depth_Node values

   subtype Depth_Node_Vector is Depth_Node_Vectors.Vector;
   --  Vector of Depth_Node values

   package Depth_Node_Iters is new Iters.Iterators (Depth_Node);
   --  Iterators over Depth_Node values

   subtype Depth_Node_Iter is
     Depth_Node_Iters.Iterator_Interface;
   --  Interface implemented by iterators over Depth_Node values

   subtype Depth_Node_Iter_Access is Depth_Node_Iters.Iterator_Access;
   --  Pointer to an iterator over Depth_Node values

   package Depth_Node_Options is new Options (Depth_Node);
   --  Optional Depth_Node values

   subtype Depth_Node_Option is Depth_Node_Options.Option;
   --  Optional Depth_Node value

   type Depth_Node_Array is array (Positive range <>) of Depth_Node;

   function Hash (Value : Depth_Node) return Ada.Containers.Hash_Type;
   --  Return the has of a Depth_Node value.
   --  The hash is computed by xoring the hash of the ast node and its
   --  depth value.

   function To_Lk_Node_Iterator
     (Self : Depth_Node_Iter'Class) return Lk_Node_Iterator'Class;
   --  Transform a depth node iterator into a basic ``Lk_Node_Iterator``. Note
   --  that this takes ownership of the original iterator, which shouldn't be
   --  reused afterwards! If you want to retain the original iterator, pass a
   --  clone to this function.

private

   type Depth_Node_Lk_Node_Iterator is new Lk_Node_Iterator with record
      Internal : Depth_Node_Iter_Access;
   end record;

   overriding function Next
     (Iter   : in out Depth_Node_Lk_Node_Iterator;
      Result : out LK.Lk_Node) return Boolean;

   overriding function Clone
     (Iter : Depth_Node_Lk_Node_Iterator) return Depth_Node_Lk_Node_Iterator;

   overriding procedure Release (Iter : in out Depth_Node_Lk_Node_Iterator);

end LKQL.Depth_Nodes;

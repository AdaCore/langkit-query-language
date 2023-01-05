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

with LKQL.Depth_Nodes; use LKQL.Depth_Nodes;
with GNATCOLL.Refcount; use GNATCOLL.Refcount;

package LKQL.Selector_Lists is

   type Selector_List is new Depth_Node_Iter with private;

   function Nodes (Self : Selector_List) return Lk_Node_Vector;
   --  Return an array containing the Ada_Node values returned by the selector
   ---
   --  NOTE that this will consume the entire selector.

   function Depth_Nodes (Self : Selector_List) return Depth_Node_Vector;
   --  Return an array containing the Depth_Node values returned by the
   --  selector.
   ---
   --  NOTE that this will consume the entire selector.

   function Max_Depth (Self : Selector_List) return Natural;
   --  Return the depth of the 'deepest' node return by the selector
   ---
   --  NOTE that this will consume the entire selector.

   function Length (Self : Selector_List) return Natural;
   --  Return the number of nodes returned by the selector.
   ---
   --  NOTE that this will consume the entire selector.

   overriding function Next
     (Self : in out Selector_List; Result : out Depth_Node) return Boolean;

   overriding function Clone (Self : Selector_List) return Selector_List;

   type Depth_Node_Filter_Access is
      not null access all Depth_Node_Iters.Filter_Iter;

   function Make_Selector_List
     (Iter : Depth_Node_Iter_Access) return Selector_List;
   --  Return a Selector_List wrapping the given iterator

private

   --------------------------
   -- Selector_Shared_Data --
   --------------------------

   subtype Optionnal_Natural is Integer range -1 .. Integer'Last;
   --  Subtype used to represent optional Natural values.
   --  A value of -1 is equivalent to 'None'.

   type Selector_Shared_Data is new Refcounted with record
      Iter         : Depth_Node_Iters.Resetable_Iter;
      --  Iterator yielding the selector values
      Max_Depth    : Optionnal_Natural := -1;
      --  Maximum depth of the selector values
      Total_Length : Optionnal_Natural := -1;
      --  Total number of elements that belong to the result set of the
      --  selector.
   end record;
   --  Data shared by multiple references to the same selector list

   function Values
     (Data : in out Selector_Shared_Data) return Depth_Node_Vector;
   --  Return the nodes yielded by the selector

   function Nth_Node (Data : in out Selector_Shared_Data;
                     N    : Positive) return Depth_Node_Iters.Element_Option;
   --  Return the nth node yielded by the selector, if any

   function Draw_N_From_Iter (Data : in out Selector_Shared_Data;
                              N    : Positive)
                              return Depth_Node_Iters.Element_Option;
   --  Draw N nodes from the wrapped iterator and return the nth drawn node,
   --  if any.
   --  All drawn nodes will be stored in the cache.

   function Get_Total_Length (Data : in out Selector_Shared_Data)
                              return Natural;
   --  Return the total number of nodes returned by the selector

   function Get_Max_Depth (Data : in out Selector_Shared_Data) return Natural;
   --  Return the depth of the deepest node returned by the selector.

   function Filtered_Count (Data : in out Selector_Shared_Data) return Natural
     with Pre => Data.Iter.Get_Inner.all in Depth_Node_Iters.Filter_Iter;
   --  Return the number of nodes that didn't match the selector predicate

   procedure Release (Data : in out Selector_Shared_Data);
   --  Release the Selector_Shared_Data's value memory

   package Shared_Data_Ptrs is new Shared_Pointers
     (Element_Type => Selector_Shared_Data,
      Release      => Release);

   subtype Shared_Data_Ref is Shared_Data_Ptrs.Ref;

   function Make_Shared_Data
     (Iter : Depth_Node_Iter_Access) return Shared_Data_Ref;

   -------------------
   -- Selector_List --
   -------------------

   type Selector_List is new Depth_Node_Iter with record
      Data     : Shared_Data_Ref;
      --  Pointer to the actual data.
      Next_Pos : Positive;
      --  Position of the next element to be
   end record;

end LKQL.Selector_Lists;

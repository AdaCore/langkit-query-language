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

package body LKQL.Selector_Lists is

   -----------
   -- Nodes --
   -----------

   function Nodes (Self : Selector_List) return Lk_Node_Vector is
      Depth_Node_Values : constant Depth_Node_Vector := Self.Depth_Nodes;
   begin
      return Result : Lk_Node_Vector do
         for El of Depth_Node_Values loop
            Result.Append (El.Node);
         end loop;
      end return;
   end Nodes;

   ------------
   -- Values --
   ------------

   function Depth_Nodes (Self : Selector_List) return Depth_Node_Vector is
      (Self.Data.Get.Values);

   ---------------
   -- Max_Depth --
   ---------------

   function Max_Depth (Self : Selector_List) return Natural is
     (Self.Data.Get.Get_Max_Depth);

   ------------
   -- Length --
   ------------

   function Length (Self : Selector_List) return Natural is
     (Self.Data.Get.Get_Total_Length);

   ----------
   -- Next --
   ----------

   function Next
     (Self : in out Selector_List; Result : out Depth_Node) return Boolean
   is
      use Depth_Node_Iters.Element_Options;
      Element : constant Option := Self.Data.Get.Nth_Node (Self.Next_Pos);
   begin
      if Is_Some (Element) then
         Self.Next_Pos := Self.Next_Pos + 1;
         Result := Extract (Element);
      end if;

      return Is_Some (Element);
   end Next;

   -----------
   -- Clone --
   -----------

   function Clone (Self : Selector_List) return Selector_List is
      Data_Ref : constant Shared_Data_Ref := Self.Data;
   begin
      return Selector_List'(Data_Ref, Positive'First);
   end Clone;

   ------------------------
   -- Make_Selector_List --
   ------------------------

   function Make_Selector_List
     (Iter : Depth_Node_Iter_Access) return Selector_List
   is
     (Data     => Make_Shared_Data (Iter),
      Next_Pos => Positive'First);

   ------------
   -- Values --
   ------------

   function Values
     (Data : in out Selector_Shared_Data) return Depth_Node_Vector
   is
      use Depth_Node_Iters.Element_Options;
      Current_Pos     : Positive := 1;
      Current_Element : Option := Data.Nth_Node (Current_Pos);
   begin
      return Elements_Vec : Depth_Node_Vector do
         while Is_Some (Current_Element) loop
            Elements_Vec.Append (Extract (Current_Element));
            Current_Pos := Current_Pos + 1;
            Current_Element := Data.Nth_Node (Current_Pos);
         end loop;
      end return;
   end Values;

   --------------
   -- Nth_Node --
   --------------

   function Nth_Node (Data : in out Selector_Shared_Data;
                      N    : Positive) return Depth_Node_Iters.Element_Option
   is
      use Depth_Node_Iters.Element_Options;
      Element : constant Option := Data.Iter.Get_Cached (N);
   begin
      return (if Is_Some (Element) then Element
              else Draw_N_From_Iter (Data, N - Data.Iter.Cache_Length));
   end Nth_Node;

   ----------------------
   -- Draw_N_From_Iter --
   ----------------------

   function Draw_N_From_Iter (Data : in out Selector_Shared_Data;
                              N    : Positive)
                              return Depth_Node_Iters.Element_Option
   is
      use Depth_Node_Iters.Element_Options;
      Element   : Depth_Node;
      Remaining : Natural := N;
   begin
      while Remaining > 0 and then Data.Iter.Next (Element) loop
         Remaining := Remaining - 1;
      end loop;

      return (if Remaining = 0 then To_Option (Element)
              else None);
   end Draw_N_From_Iter;

   ----------------------
   -- Get_Total_Length --
   ----------------------

   function Get_Total_Length (Data : in out Selector_Shared_Data)
                              return Natural
   is
   begin
      if Data.Total_Length = -1 then
         Data.Total_Length := Integer (Data.Values.Length);
      end if;

      return Data.Total_Length;
   end Get_Total_Length;

   -------------------
   -- Get_Max_Depth --
   -------------------

   function Get_Max_Depth (Data : in out Selector_Shared_Data) return Natural
   is
      Max : Natural := 0;
   begin
      if Data.Max_Depth = -1 then
         for N of Data.Values loop
            if N.Depth > Max then
               Max := N.Depth;
            end if;
         end loop;

         Data.Max_Depth := Max;
      end if;

      return Data.Max_Depth;
   end Get_Max_Depth;

   --------------------
   -- Filtered_Count --
   --------------------

   function Filtered_Count (Data : in out Selector_Shared_Data) return Natural
   is
      Inner : constant Depth_Node_Iters.Filter_Iter :=
        Depth_Node_Iters.Filter_Iter (Data.Iter.Get_Inner.all);
   begin
      return Inner.Filtered_Count;
   end Filtered_Count;

   -------------
   -- Release --
   -------------

   procedure Release (Data : in out Selector_Shared_Data) is
   begin
      Data.Iter.Release;
   end Release;

   ----------------------
   -- Make_Shared_Data --
   ----------------------

   function Make_Shared_Data
     (Iter : Depth_Node_Iter_Access) return Shared_Data_Ref
   is
      Resetable : constant Depth_Node_Iters.Resetable_Iter :=
        Depth_Node_Iters.Resetable (Depth_Node_Iters.Iterator_Access (Iter));
      Data : constant Selector_Shared_Data :=
        (Refcounted with Iter => Resetable, others => <>);
   begin
      return Result : Shared_Data_Ref do
         Result.Set (Data);
      end return;
   end Make_Shared_Data;

end LKQL.Selector_Lists;

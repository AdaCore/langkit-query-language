------------------------------------------------------------------------------
--                                                                          --
--                                   LKQL                                   --
--                                                                          --
--                        Copyright (C) 2021-2022, AdaCore                  --
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

with Ada.Unchecked_Conversion;

package body Unbounded_Holders is

   package body Holders_Impl is

      use System.Storage_Elements;

      ---------
      -- Get --
      ---------

      function Unchecked_Get (Self : Holders.Holder) return T_Access is
         use Holders;
         subtype Sized_Buffer is Unsized_Buffer (0 .. Self.Real_Size);
         Buffer : Sized_Buffer;
         for Buffer'Address use Self.Buffer'Address;

         function To_T_Access is new Ada.Unchecked_Conversion
           (System.Address, T_Access);
      begin
         return To_T_Access (Buffer'Address);
      end Unchecked_Get;

      ------------
      -- Create --
      ------------

      function Create (Value : T) return Holders.Holder is
         Size_In_Storage_Elements : constant Storage_Count
           := Value'Size / System.Storage_Unit;
      begin
         if Size_In_Storage_Elements > Holders.Max_Size then
            raise Constraint_Error with
              "Value too big to be held in Holder (need "
              & Size_In_Storage_Elements'Image & ", got "
              & Holders.Max_Size'Image & ")";
         end if;

         declare
            pragma Warnings (Off, "program execution may be");
            Ret    : Holders.Holder;
            Buffer : Holders.Internal_Buffer;
            for Buffer'Address use Value'Address;
            pragma Warnings (On, "program execution may be");
         begin
            for I in 0 .. Size_In_Storage_Elements loop
               Ret.Buffer (I) := Buffer (I);
            end loop;
            Ret.Real_Size := Size_In_Storage_Elements;
            return Ret;
         end;
      end Create;

   end Holders_Impl;

   package body Holders is
      -------------------
      -- Unchecked_Get --
      -------------------

      function Unchecked_Get (Self : Holder) return T_Access is
      begin
         return T_Access
           (Holders_Implem.Unchecked_Get
              (Holders_Implem.Holders.Holder (Self)));
      end Unchecked_Get;

      ------------
      -- Create --
      ------------

      function Create (Value : T) return Holder is
      begin
         return (Holders_Implem.Create (Value) with null record);
      end Create;
   end Holders;

end Unbounded_Holders;

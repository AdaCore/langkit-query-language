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

package body LKQL.Adaptive_Integers is
   Int_First_Big : constant Big_Integer := To_Big_Integer (Integer'First);
   Int_Last_Big  : constant Big_Integer := To_Big_Integer (Integer'Last);

   function Create (Value : Big_Integer) return Adaptive_Integer;
   --  Create an Adaptive_Integer from a Big_Integer

   function To_Big_Integer (Int : Adaptive_Integer) return Big_Integer;
   --  Return the given Adaptive_Integer as a Big_Integer, performing
   --  the conversion if necessary.

   ------------
   -- Create --
   ------------

   function Create (Value : Integer) return Adaptive_Integer is
   begin
      return (Kind => Small, Small_Value => Value);
   end Create;

   ------------
   -- Create --
   ------------

   function Create (Value : Big_Integer) return Adaptive_Integer is
   begin
      return (Kind => Big, Big_Value => Value);
   end Create;

   ------------
   -- Create --
   ------------

   function Create (Value : String) return Adaptive_Integer is
   begin
      return (Kind => Small, Small_Value => Integer'Value (Value));
   exception
      when Constraint_Error =>
         return (Kind => Big, Big_Value => From_String (Value));
   end Create;

   -----------
   -- Image --
   -----------

   function Image (Int : Adaptive_Integer) return String is
   begin
      case Int.Kind is
         when Small =>
            return Int.Small_Value'Image;
         when Big =>
            return To_String (Int.Big_Value);
      end case;
   end Image;

   --------------------
   -- To_Big_Integer --
   --------------------

   function To_Big_Integer (Int : Adaptive_Integer) return Big_Integer is
   begin
      if Int.Kind = Small then
         return To_Big_Integer (Int.Small_Value);
      else
         return Int.Big_Value;
      end if;
   end To_Big_Integer;

   ---------
   -- "+" --
   ---------

   function "+" (Int : Adaptive_Integer) return Integer is
   begin
      if Int.Kind = Small then
         return Int.Small_Value;
      else
         return To_Integer (Int.Big_Value);
      end if;
   end "+";

   generic
      with function Small_Op (L, R : Integer) return Boolean;
      with function Big_Op   (L, R : Big_Integer) return Boolean;
   function Dispatch_Rel_Op
     (L, R : Adaptive_Integer) return Boolean;
   --  Generic implementation of relational operations on adaptive integers

   ---------------------
   -- Dispatch_Rel_Op --
   ---------------------

   function Dispatch_Rel_Op
     (L, R : Adaptive_Integer) return Boolean
   is
   begin
      case L.Kind is
         when Small =>
            case R.Kind is
               when Small =>
                  return Small_Op (L.Small_Value, R.Small_Value);
               when Big =>
                  return Big_Op
                    (To_Big_Integer (L.Small_Value), R.Big_Value);
            end case;
         when Big =>
            case R.Kind is
               when Small =>
                  return Big_Op
                    (L.Big_Value, To_Big_Integer (R.Small_Value));
               when Big =>
                  return Big_Op (L.Big_Value, R.Big_Value);
            end case;
      end case;
   end Dispatch_Rel_Op;

   package Implementation is
      function "=" is new Dispatch_Rel_Op ("=", "=");
      function "<" is new Dispatch_Rel_Op ("<", "<");
      function "<=" is new Dispatch_Rel_Op ("<=", "<=");
      function ">" is new Dispatch_Rel_Op (">", ">");
      function ">=" is new Dispatch_Rel_Op (">=", ">=");
   end Implementation;

   ---------
   -- "=" --
   ---------

   function "=" (L, R : Adaptive_Integer) return Boolean
      renames Implementation."=";

   ---------
   -- "<" --
   ---------

   function "<" (L, R : Adaptive_Integer) return Boolean
      renames Implementation."<";

   ----------
   -- "<=" --
   ----------

   function "<=" (L, R : Adaptive_Integer) return Boolean
      renames Implementation."<=";

   ---------
   -- ">" --
   ---------

   function ">" (L, R : Adaptive_Integer) return Boolean
      renames Implementation.">";

   ----------
   -- ">=" --
   ----------

   function ">=" (L, R : Adaptive_Integer) return Boolean
      renames Implementation.">=";

   ---------
   -- "-" --
   ---------

   function "-" (L : Adaptive_Integer) return Adaptive_Integer is
   begin
      case L.Kind is
         when Small =>
            --  Check for overflow
            declare
               X : constant Integer := L.Small_Value;
            begin
               if X = Integer'First then
                  --  overflow
                  return Create (-To_Big_Integer (X));
               else
                  return (Kind => Small, Small_Value => -X);
               end if;
            end;
         when Big =>
            return (Kind => Big, Big_Value => -L.Big_Value);
      end case;
   end "-";

   ---------
   -- "+" --
   ---------

   function "+" (L, R : Adaptive_Integer) return Adaptive_Integer is
   begin
      if L.Kind = Small and R.Kind = Small then
         --  Check for overflow
         declare
            X : constant Integer := L.Small_Value;
            Y : constant Integer := R.Small_Value;
         begin
            if (X > 0 and then Y > Integer'Last  - X) or else
               (X < 0 and then Y < Integer'First - X)
            then
               --  overflow
               return Create (To_Big_Integer (X) + To_Big_Integer (Y));
            else
               return Create (X + Y);
            end if;
         end;
      end if;

      --  If any of the operands is a big integer, perform the computation
      --  on two big integers.
      return Create (To_Big_Integer (L) + To_Big_Integer (R));
   end "+";

   ---------
   -- "-" --
   ---------

   function "-" (L, R : Adaptive_Integer) return Adaptive_Integer is
      (L + (-R));

   ---------
   -- "*" --
   ---------

   function "*" (L, R : Adaptive_Integer) return Adaptive_Integer is
      --  Always do the multiplication in big integer space, as checking
      --  for multiplication overflow is quite complicated.
      P : constant Big_Integer := To_Big_Integer (L) * To_Big_Integer (R);
   begin
      if L.Kind = Small and R.Kind = Small then
         if In_Range (P, Int_First_Big, Int_Last_Big) then
            return Create (To_Integer (P));
         end if;
      end if;
      return Create (P);
   end "*";

   ---------
   -- "/" --
   ---------

   function "/" (L, R : Adaptive_Integer) return Adaptive_Integer is
   begin
      if L.Kind = Small and R.Kind = Small then
         --  Check for the only possible overflow
         if L.Small_Value /= Integer'First or else R.Small_Value /= -1 then
            return Create (L.Small_Value / R.Small_Value);
         end if;
      end if;
      return Create (To_Big_Integer (L) / To_Big_Integer (R));
   end "/";

end LKQL.Adaptive_Integers;

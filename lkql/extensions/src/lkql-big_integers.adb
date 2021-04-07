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

with Ada.Unchecked_Deallocation;

package body LKQL.Big_Integers is
   function Create (Value : GMP_Big_Integer) return Big_Integer;

   ------------
   -- Create --
   ------------

   function Create (Value : GMP_Big_Integer) return Big_Integer is
   begin
      return Res : constant Big_Integer :=
        (Ada.Finalization.Controlled with Value => new Big_Integer_Record'
           (Value     => <>,
            Ref_Count => 1))
      do
         Res.Value.Value.Set (Value);
      end return;
   end Create;

   --------------------
   -- To_Big_Integer --
   --------------------

   function To_Big_Integer (Value : Integer) return Big_Integer is
      Res : GMP_Big_Integer;
   begin
      Res.Set (GNATCOLL.GMP.Long (Value));
      return Create (Res);
   end To_Big_Integer;

   ----------------
   -- To_Integer --
   ----------------

   function To_Integer (Value : Big_Integer) return Integer is
     (Integer'Value (To_String (Value)));

   -----------------
   -- From_String --
   -----------------

   function From_String (Value : String) return Big_Integer is
     (Create (GNATCOLL.GMP.Integers.Make (Value)));

   ---------------
   -- To_String --
   ---------------

   function To_String   (Value : Big_Integer) return String is
     (GNATCOLL.GMP.Integers.Image (Value.Value.Value));

   generic
      with function Op (L, R : GMP_Big_Integer) return Boolean;
   function Rel_Op (L, R : Big_Integer) return Boolean;
   --  Generic implementation for binary operations over big integers
   --  returning a boolean.

   generic
      with function Op (L : GMP_Big_Integer) return GMP_Big_Integer;
   function Un_Op (L : Big_Integer) return Big_Integer;
   --  Generic implementation if unary operations over a big integer
   --  returning a big integer.

   generic
      with function Op (L, R : GMP_Big_Integer) return GMP_Big_Integer;
   function Bin_Op (L, R : Big_Integer) return Big_Integer;
   --  Generic implementation of binary operations over big integers
   --  returning a big integer.

   ------------
   -- Rel_Op --
   ------------

   function Rel_Op (L, R : Big_Integer) return Boolean is
     (Op (L.Value.Value, R.Value.Value));

   -----------
   -- Un_Op --
   -----------

   function Un_Op (L : Big_Integer) return Big_Integer is
     (Create (Op (L.Value.Value)));

   ------------
   -- Bin_Op --
   ------------

   function Bin_Op (L, R : Big_Integer) return Big_Integer is
     (Create (Op (L.Value.Value, R.Value.Value)));

   package Implementation is
      use GNATCOLL.GMP.Integers;

      function "="  is new Rel_Op ("=");
      function "<"  is new Rel_Op ("<");
      function "<=" is new Rel_Op ("<=");
      function ">"  is new Rel_Op (">");
      function ">=" is new Rel_Op (">=");

      function "-" is new Un_Op ("-");

      function "+" is new Bin_Op ("+");
      function "-" is new Bin_Op ("-");
      function "*" is new Bin_Op ("*");
      function "/" is new Bin_Op ("/");
   end Implementation;

   ---------
   -- "=" --
   ---------

   function "=" (L, R : Big_Integer) return Boolean
      renames Implementation."=";

   ---------
   -- "<" --
   ---------

   function "<" (L, R : Big_Integer) return Boolean
      renames Implementation."<";

   ----------
   -- "<=" --
   ----------

   function "<=" (L, R : Big_Integer) return Boolean
      renames Implementation."<=";

   ---------
   -- ">" --
   ---------

   function ">" (L, R : Big_Integer) return Boolean
      renames Implementation.">";

   ----------
   -- ">=" --
   ----------

   function ">=" (L, R : Big_Integer) return Boolean
      renames Implementation.">=";

   --------------
   -- In_Range --
   --------------

   function In_Range (Value, Lo, Hi : Big_Integer) return Boolean is
     (Lo <= Value and then Value <= Hi);

   ---------
   -- "-" --
   ---------

   function "-" (L : Big_Integer) return Big_Integer
      renames Implementation."-";

   ---------
   -- "+" --
   ---------

   function "+" (L, R : Big_Integer) return Big_Integer
      renames Implementation."+";

   ---------
   -- "-" --
   ---------

   function "-" (L, R : Big_Integer) return Big_Integer
      renames Implementation."-";

   ---------
   -- "*" --
   ---------

   function "*" (L, R : Big_Integer) return Big_Integer
      renames Implementation."*";

   ---------
   -- "/" --
   ---------

   function "/" (L, R : Big_Integer) return Big_Integer
      renames Implementation."/";

   ----------
   -- Free --
   ----------

   procedure Free is new Ada.Unchecked_Deallocation
     (Big_Integer_Record, Big_Integer_Record_Access);
   --  Free a big integer record

   overriding procedure Adjust (X : in out Big_Integer) is
   begin
      if X.Value /= null then
         X.Value.Ref_Count := X.Value.Ref_Count + 1;
      end if;
   end Adjust;

   overriding procedure Finalize (X : in out Big_Integer) is
   begin
      if X.Value /= null then
         X.Value.Ref_Count := X.Value.Ref_Count - 1;
         if X.Value.Ref_Count = 0 then
            Free (X.Value);
         end if;
      end if;
   end Finalize;
end LKQL.Big_Integers;

with Ada.Unchecked_Deallocation;

package Deeply_Inlined_2012 is

   generic
   procedure P0 with Inline;

   generic
   procedure P1 with Inline;

   generic
   procedure P2 with Inline;

   generic
   procedure P3 with Inline;

   generic
   procedure P4 with Inline;   --  FLAG

   generic
   procedure P5 with Inline;   --  FLAG

   type T is private;

   procedure Free (Elem : in out T);  --  NOFLAG

private
   type U is null record;
   type U_Access is access U;

   procedure Free is new Ada.Unchecked_Deallocation  --  NOFLAG
     (U, U_Access);

   type T is record
      C : U_Access;
   end record;

   pragma Inline (Free);
end Deeply_Inlined_2012;

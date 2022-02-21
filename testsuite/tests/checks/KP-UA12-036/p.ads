with Ada.Unchecked_Deallocation;

package P is

   type Root_Object is tagged null record;
   type Root_Object_Ptr is access all Root_Object'Class;

   type Type1 is array (1..4) of Long_Float;
   for Type1'Alignment use 32;

   type Nasty is new Root_Object with record
      X : Type1;
   end record;
   type Nasty_Ptr is access all Nasty;

   procedure Free is  --  FLAG: Nasty is part of Root_Object'Class
     new Ada.Unchecked_Deallocation (Root_Object'Class, Root_Object_Ptr);

   procedure Free is  --  FLAG
     new Ada.Unchecked_Deallocation (Nasty, Nasty_Ptr);

end P;

with Ada.Finalization;

package Controlled_Type_Declarations is

   type Resource is new Ada.Finalization.Controlled with private;  --  FLAG
   type Resource_2 is tagged private;  --  FLAG
   type Resource_3 is record
      R : Resource_2;
   end record;
   
private
   type Resource is new Ada.Finalization.Controlled with null record; -- FLAG
   type Resource_2 is new Ada.Finalization.Controlled with null record; -- FLAG
   type Resource_4 is new Resource_2 with null record; -- FLAG
end Controlled_Type_Declarations;

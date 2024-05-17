with Ada.Finalization;
package Foo is
   type Resource is new Ada.Finalization.Controlled with private;  --  FLAG

   subtype Sub_Resource is Resource;  -- NOFLAG

   type New_Resource is new Sub_Resource;  --  FLAG
private
   type Resource is new Ada.Finalization.Controlled with null record; --  FLAG
   type Ressource_Array is new Array (Positive range <>)
        of Ada.Finalization.Controlled; --  NOFLAG
   type Ressource_Record is record
        F : Ada.Finalization.Controlled; --  NOFLAG
    end record;
end Foo;

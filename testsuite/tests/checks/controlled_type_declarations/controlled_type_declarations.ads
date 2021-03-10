with Ada.Finalization;
package Foo is
   type Resource is new Ada.Finalization.Controlled with private;  --  FLAG
private
   type Resource is new Ada.Finalization.Controlled with null record; -- FLAG
   type Ressource_Array is new Array (Positive range <>)
        of Ada.Finalization.Controlled; -- NO FLAG
   type Ressource_Record is record
        F : Ada.Finalization.Controlled; -- NO FLAG
    end record;
end Foo;

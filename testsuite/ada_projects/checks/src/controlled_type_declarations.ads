with Ada.Finalization;
package Foo is
   type Resource is new Ada.Finalization.Controlled with private;  --  FLAG
private
   type Resource is new Ada.Finalization.Controlled with null record;
end Foo;

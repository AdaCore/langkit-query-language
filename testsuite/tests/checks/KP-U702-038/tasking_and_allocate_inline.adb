procedure Tasking_And_Allocate_Inline is

   type C is tagged null record;

   task type T is
      entry E;
   end;
   task body T is
   begin
      accept E do
         null;
      end E;
   end;

   Child : T;
   X : access C'Class := new C'(null record);   --  FLAG
begin
   Child.E;
end;

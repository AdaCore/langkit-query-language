procedure Main is

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

   procedure P is
      X : access C'Class := new C'(null record);   --  FLAG
   begin
      Child.E;
   end;
begin
   P;
end;

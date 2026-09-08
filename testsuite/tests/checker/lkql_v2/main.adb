procedure Main is
   X : Integer;

   generic
   package Pkg is
      function Foo return Boolean is (True);
   end Pkg;

   package My_Pkg is new Pkg;
begin
   null;
end Main;

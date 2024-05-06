procedure Main is
   type Int   is range 0 .. 100; --  FLAG (2)
   type Int_T is range 0 .. 100;

   type Int_A   is access Int;  --  FLAG (2)
   type Int_PTR is access Int;  --  NOFLAG
   type Int_PTR_A is access Int_PTR;  --  FLAG (2)
   type Int_PTR_PTR is access Int_PTR;  --  NOFLAG

   Const   : constant Int := 1; --  FLAG (2)
   Const_C : constant Int := 1;
begin
   null;
end Main;

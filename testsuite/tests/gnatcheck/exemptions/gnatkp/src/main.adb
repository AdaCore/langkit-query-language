procedure Main is
   type Int_Arr is array (1 .. 3) of Integer;

   X : Int_Arr;

   A_1 : Int_Arr := (1, 2, 3)    --  FLAG
     with Address => X'Address;

   pragma Annotate (GnatKP, Exempt_On, "kp_19198", "Because");
   A_2 : Int_Arr := (1, 2, 3)    --  FLAG
     with Address => X'Address;
   pragma Annotate (GnatKP, Exempt_Off, "kp_19198");

   --## kp off kp_19198 ## Because
   A_3 : Int_Arr := (1, 2, 3)    --  FLAG
     with Address => X'Address;
   --## kp on kp_19198
begin
   goto lbl;   --  FLAG

   pragma Annotate (Gnatcheck, Exempt_On, "goto_statements", "Because");
   goto lbl;   --  FLAG
   pragma Annotate (Gnatcheck, Exempt_Off, "goto_statements");

   --## rule off goto_statements ## Because
   goto lbl;   --  FLAG
   --## rule on goto_statements

   <<lbl>>
end Main;

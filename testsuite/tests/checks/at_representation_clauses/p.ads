package P is
   Var : Integer;

   Id : Integer;
   for Id use at Var'Address;   --  FLAG

   type Rec is record
      Field : Integer;
   end record;

   for Rec use record           --  FLAG
     at mod 2;
   end record;

end P;

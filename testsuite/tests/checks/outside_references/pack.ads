package Pack is

   I : Integer;

   procedure Proc (J : in out Integer);

   type Rec is record
      C : Integer;
   end record;

   Global_Rec : Rec;

   type Arr is array (1 .. 10) of Integer;

end Pack;

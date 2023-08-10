package Deriv is

   type Coordinates is record
      X, Y, Z : Float;
   end record;

   type Hidden_Coordinates is new Coordinates;   --  FLAG

   type T_Int is new Integer;                    --  FLAG

   type T_Rec is tagged record
      A : Integer;
   end record;
   type T_Rec_D is new T_Rec with record         --  NOFLAG
      B : Boolean;
   end record;

   type T_Rec_2 is record
      F : Integer;
   end record;
   type TD_Rec_2 is new T_Rec_2;                 --  FLAG

   type T_Priv_1 is private;
   type T_Priv_2 is abstract tagged private;

   type T_Rec_DP is new T_Rec with private;      --  NOFLAG

   procedure Proc (Param : T_Priv_1);

private

   type T_Priv_1 is new Positive range 1 .. 10;  --  FLAG
   type T_Priv_2 is new T_Rec with record        --  NOFLAG
      I : T_Priv_1;
   end record;

   type T_Rec_DP is new T_Rec with null record;  --  NOFLAG

end Deriv;

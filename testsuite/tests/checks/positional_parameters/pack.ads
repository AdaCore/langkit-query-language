package Pack is

   type Tagged_T is tagged null record;

   procedure T_Proc1 (X : Tagged_T);

   procedure T_Proc2 (X : Tagged_T; Y : Integer);

   procedure T_Proc3
     (X : Tagged_T;
      Y : Integer;
      Z : Integer := 1);

   task T is
      entry E1 (I : Integer; J : Integer);
      entry E2 (Boolean) (I : Integer; J : Integer);
      entry E3 (Boolean);
   end T;

end Pack;

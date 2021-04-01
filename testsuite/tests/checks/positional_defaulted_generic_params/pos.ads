package Pos is
   function Fun_1 (I : Integer) return Integer;
   function Fun_2 (I : Integer) return Integer;

   generic
      I_Par1 : Integer;
      I_Par2 : Integer := 1;
      with function Fun_1 (I : Integer) return Integer is <>;
      with function Fun_3 (I : Integer) return Integer is Fun_2;
   package Pack_G is
      Var_1 : Integer := I_Par1;
      Var_2 : Integer := I_Par2;
      Var_3 : Integer := Fun_1 (Var_1);
      Var_4 : Integer := Fun_3 (Var_2);
   end Pack_G;

   package Pack_I_1 is new Pack_G (1);

   package Pact_I_2 is new Pack_G
     (2, I_Par2 => 3, Fun_1 => Fun_2, Fun_3 => Fun_1);

   package Pack_I_3 is new Pack_G (1,
                                   2,            --  FLAG
                                   Fun_2,        --  FLAG
                                   Fun_1);       --  FLAG

end Pos;

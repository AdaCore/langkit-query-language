package Other is
   type Arr is array (1 .. 10) of Integer;
   subtype Most is Integer range 1 .. 8;

   type Rec is record
      C1 : Integer;
      C2 : Integer;
      C3 : Integer;
      C4 : Integer;
   end record;

   type Tagged_Rec is tagged record
      C1 : Integer;
   end record;

   type New_Tagged_Rec is new Tagged_Rec with record
      C2 : Integer;
      C3 : Integer;
      C4 : Integer;
   end record;

   Arr_Var1 : Arr := (others => 1);                 --  NO FLAG
   Arr_Var2 : Arr := (1 => 1, 2 => 2, others => 0); --  FLAG
   Arr_Var3 : Arr := (1 | 2 => 1, others => 0);     --  FLAG
   Arr_Var4 : Arr := (1 .. 2 => 1, others => 0);    --  FLAG
   Arr_Var5 : Arr := (Most => 1, others => 0);      --  FLAG

   Rec_Var1 : Rec := (C1 => 1, others => 0);        --  NO FLAG
   Rec_Var2 : Rec := (1, 2, others => 3);           --  FLAG

   Tagged_Rec_Var : Tagged_Rec := (C1 => 1);

   New_Tagged_Rec_Var : New_Tagged_Rec := (Tagged_Rec_Var with others => 0); -- FLAG
end Other;

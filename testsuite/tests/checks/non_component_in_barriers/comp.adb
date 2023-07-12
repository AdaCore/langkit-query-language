procedure Comp is

   type Arr is array (1 .. 10) of Boolean;

   type Rec is record
      C : Boolean;
   end record;

   type Arr_Of_Rec is array (1 .. 10) of Rec;

   protected Obj is
      entry E1;
      entry E2;
      entry E3;
      entry E4;
   private
      Is_Set : Boolean := False;

      A   : Arr;
      R   : Rec;
      R1  : Rec;
      A_R : Arr_Of_Rec;
   end Obj;

   Global_Bool : Boolean := False;

   protected body Obj is

      entry E1 when A (1) is          -- NOFLAG
      begin
         Is_Set := False;
      end E1;

      entry E2 when R.C is            -- NOFLAG
      begin
         Is_Set := True;
      end E2;

      entry E3 when A_R (1).C is      -- NOFLAG
      begin
         Is_Set := True;
      end E3;

      entry E4 when R = R1 is      -- NOFLAG
      begin
         Is_Set := True;
      end E4;

   end Obj;

begin
   null;
end;

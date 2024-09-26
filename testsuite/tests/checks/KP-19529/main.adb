with Pkg; use Pkg;

procedure Main is
   type Int_Arr is array (1 .. 3) of Integer;
   type Rec is record
      I : Integer;
      B : Boolean;
   end record;
   subtype S_Rec is Rec;
   type D_Rec is new Rec;
   type Rec_Arr is record
      I : Integer;
      A : Int_Arr;
   end record;

   I_1 : Integer := 1;
   I_2 : Integer;
   S_1 : String := "Hello";
   S_2 : String (1 .. 5);
   I_A_1 : Int_Arr := Int_Arr'(1, 2, 3);
   I_A_2 : Int_Arr;
   R_1 : Rec := Rec'(1, True);
   R_2 : Rec;
   P_R_1 : P_Rec := Create (1, True);
   P_R_2 : P_Rec;
   R_A_1 : Rec_Arr := Rec_Arr'(1, Int_Arr'(1, 2, 3));

   procedure Inner is
      S_3 : String (1 .. 5);

      procedure Inner_Inner is
         S_4 : String renames S_1;
      begin
         S_3 := S_1;    --  FLAG
         S_3 := S_4;    --  FLAG
         S_2 := S_3;    --  FLAG
      end Inner_Inner;
   begin
      I_2 := 2;         --  NOFLAG
      I_2 := I_1;       --  NOFLAG
      S_2 := "Hello";   --  NOFLAG
      S_2 := S_1;       --  FLAG
      S_3 := S_1;       --  NOFLAG
      S_2 := S_3;       --  NOFLAG
      I_A_2 := I_A_1;   --  FLAG
      R_2 := R_1;       --  FLAG
      P_R_2 := P_R_1;   --  FLAG
      R_A_1.A := I_A_1; --  FLAG

      declare
         S_4 : String (1 .. 5);
      begin
         S_2 := S_1;    --  FLAG
         S_3 := S_1;    --  NOFLAG
         S_4 := S_1;    --  NOFLAG
      end;
   end Inner;

   package Inner_Pkg is
      D_X : Rec;
      procedure Inner_Inner;
   end Inner_Pkg;

   package body Inner_Pkg is
      B_X : Rec;

      procedure Inner_Inner is
      begin
         D_X := D_X;    --  FLAG
         B_X := B_X;    --  FLAG
      end Inner_Inner;
   end Inner_Pkg;
begin
   null;
end Main;

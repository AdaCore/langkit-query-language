procedure Main is
   type R_1 (B : Boolean := False) is limited record
      case B is
         when True =>
            I : Integer;
         when others =>
            F : Float;
      end case;
   end record;
   subtype S_R_1 is R_1;
   type D_R_1 is new R_1;
   type L_D_R_1 is limited new R_1;

   type Int_Arr is array (Integer range <>) of Integer;
   type R_2 (I : Integer := 2) is limited record
      A : Int_Arr (1 .. I);
   end record;
   type R_3 (I : Integer) is limited record
      A : Int_Arr (1 .. I);
   end record;
   type R_4 (I : Integer := 2) is record
      A : Int_Arr (1 .. I);
   end record;

   type R_5 (I : Integer := 2) is limited record
      A : Int_Arr (1 .. 4);
   end record;

   type R_6 is tagged limited record
      B : Boolean;
   end record;
   type D_R_6 (I : Integer := 2) is new R_6 with record
      A : Int_Arr (1 .. I);
   end record;
   subtype S_D_R_6 is D_R_6;

   function Get_R_1 return R_1 is
     (B => True, I => 0);
   function Get_S_R_1 return S_R_1 is
     (B => True, I => 0);
   function Get_D_R_1 return D_R_1 is
     (B => True, I => 0);
   function Get_L_D_R_1 return L_D_R_1 is
     (B => True, I => 0);
   function Get_R_2 return R_2 is
     (I => 4, A => (others => 0));
   function Get_R_3 return R_3 is
     (I => 4, A => (others => 0));
   function Get_R_4 return R_4 is
     (I => 4, A => (others => 0));
   function Get_R_5 return R_5 is
     (I => 4, A => (others => 0));
   function Get_D_R_6 return D_R_6 is
     (B => False, I => 4, A => (others => 0));
   function Get_S_D_R_6 return S_D_R_6 is
     (B => False, I => 4, A => (others => 0));

   procedure Consume_R_1 (R : R_1) is null;
   procedure Consume_S_R_1 (R : S_R_1) is null;
   procedure Consume_D_R_1 (R : D_R_1) is null;
   procedure Consume_L_D_R_1 (R : L_D_R_1) is null;
   procedure Consume_R_2 (R : R_2) is null;
   procedure Consume_R_3 (R : R_3) is null;
   procedure Consume_R_4 (R : R_4) is null;
   procedure Consume_R_5 (R : R_5) is null;
   procedure Consume_D_R_6 (R : D_R_6) is null;
   procedure Consume_S_D_R_6 (R : S_D_R_6) is null;
begin
   Consume_R_1 (Get_R_1);           --  FLAG
   Consume_S_R_1 (Get_S_R_1);       --  FLAG
   Consume_D_R_1 (Get_D_R_1);       --  FLAG
   Consume_L_D_R_1 (Get_L_D_R_1);   --  FLAG
   Consume_R_2 (Get_R_2);           --  FLAG
   Consume_R_3 (Get_R_3);           --  NOFLAG
   Consume_R_4 (Get_R_4);           --  NOFLAG
   Consume_R_5 (Get_R_5);           --  NOFLAG
   Consume_D_R_6 (Get_D_R_6);       --  FLAG
   Consume_S_D_R_6 (Get_S_D_R_6);   --  FLAG
end Main;

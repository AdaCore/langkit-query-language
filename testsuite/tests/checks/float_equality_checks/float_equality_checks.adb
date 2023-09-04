procedure Float_Equality_Checks is
   A_Float, B_Float : Float := 0.0;

   type Coefficient is digits 10 range -1.0 .. 1.0;
   A_Coeff, B_Coeff : Coefficient := 0.0;
   function "=" (A, B : Coefficient) return Boolean is
   begin
      return A - B < 0.01;
   end "=";

   subtype Probability is Long_Float range 0.0 .. 1.0;
   A_Prob, B_Prob : Probability := 0.0;

   type My_Float is new Float;
   A_MF, B_MF : My_Float := 0.0;

   A_SF, B_SF : Short_Float := 0.0;
   function "=" (A, B : Short_Float) return Boolean is
   begin
      return A - B < 0.01;
   end "=";

   type Volt is delta 0.125 range 0.0 .. 255.0;
   A_Volt, B_Volt : Volt := 0.0;

   B1_A : constant Boolean := A_Float = B_Float;              --  FLAG
   B1_B : constant Boolean := A_Float /= B_Float;             --  FLAG
   B2_A : constant Boolean := A_Coeff = B_Coeff;              --  NOFLAG
   B2_B : constant Boolean := A_Coeff /= B_Coeff;             --  NOFLAG
   B3_A : constant Boolean := "=" (A_Prob, B_Prob);           --  FLAG
   B3_B : constant Boolean := "/=" (A_Prob, B_Prob);          --  FLAG
   B4_A : constant Boolean := Standard."=" (A_Prob, B_Prob);  --  FLAG
   B4_B : constant Boolean := Standard."/=" (A_Prob, B_Prob); --  FLAG
   B5_A : constant Boolean := A_SF = B_SF;                    --  NOFLAG
   B5_B : constant Boolean := A_SF /= B_SF;                   --  NOFLAG
   B5_C : constant Boolean := Standard."="(A_SF, B_SF);       --  FLAG
   B5_D : constant Boolean := Standard."/="(A_SF, B_SF);      --  FLAG
   B6_A : constant Boolean := A_MF = B_MF;                    --  FLAG
   B6_B : constant Boolean := A_MF /= B_MF;                   --  FLAG
   B7_A : constant Boolean := "=" (A_MF, B_MF);               --  FLAG
   B8_A : constant Boolean := A_Volt = B_Volt;                --  NOFLAG
   B8_B : constant Boolean := A_Volt /= B_Volt;               --  NOFLAG
   Result : Boolean;

   function Renamed_Eq (Left : Float; Right : Float) return Boolean renames "=";
   function Renamed_Not_Eq (Left : Short_Float; Right : Short_Float)
     return Boolean renames "/=";

begin
   Result := Renamed_Eq (A_Float, B_Float);  --  NOFLAG
   Result := Renamed_Not_Eq (A_SF, B_SF);    --  NOFLAG
end Float_Equality_Checks;

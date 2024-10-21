package body Function_Out is
   function F_1 (I : Integer) return Integer is          --  NOFLAG
   begin
      return I;
   end F_1;

   function F_2 (I : in Integer) return Integer is       --  NOFLAG
   begin
      return I;
   end F_2;

   function F_3 (I : out Integer) return Integer is      --  NOFLAG
   begin
      return I;
   end F_3;

   function F_4 (I : in out Integer) return Integer is   --  NOFLAG
   begin
      return I;
   end F_4;

   function F_5 (I : out Integer) return Integer is      --  FLAG
   begin
      return I;
   end F_5;

   function Gen_F (I : out T) return Integer is          --  NOFLAG
   begin
      return 0;
   end Gen_F;

   function Sep_F (I : out Integer) return Integer       --  FLAG
     is separate;

   procedure P (I : out Integer) is                      --  NOFLAG
   begin
      null;
   end P;
end Function_Out;

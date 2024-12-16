procedure Main is

   type Custom_Int is new Integer;
   function "/" (X, Y : Custom_Int) return Custom_Int is
     (Integer (X) / Integer (Y));

   function Same_Op (X : Natural) return Integer is
      Y : Integer;
   begin
      Y := (X + 1) / (X - 1);        --  NOFLAG
      return (X + 1) / (x +  1);     --  FLAG
   end Same_Op;

   function Is_Nan (F : Float) return Boolean is (F = F);  --  NOFLAG

   F      : Float;
   Is_Inf : Boolean := Is_NaN (F - F);     --  NOFLAG

   G  : constant := 2 ** (31 - 15);        --  NOFLAG
   N : Natural;
   I : Integer;
   C : Custom_Int;
begin
   if (I + 1) = (I + 1)               --  FLAG
     and then (N mod N) /= (N mod N)  --  FLAG (3)
   then
      null;
   end if;

   if (C / C) = 0 then                --  FLAG
      null;
   end if;
end Main;

function Same (A, B : Boolean) return Boolean is

   function Same_Logic (A, B : Boolean) return Boolean is
   begin
      return A or else B or else A;  --  FLAG
      return (A or B) or else (A or B);  --  FLAG
   end Same_Logic;

begin
   return A or else B;            -- NOFLAG
end Same;

procedure Ranges is

   L : Positive := 1;
   C : constant := 1;

   S1 : String (L .. 10);     -- NOFLAG
   S2 : String (1 .. 10);     --  FLAG
   S3 : String (1 .. C);      --  FLAG

   subtype S10 is String (1 .. 10);   --  FLAG

begin
   for J in 1 .. 10 loop   --  FLAG
      null;
   end loop;
end Ranges;

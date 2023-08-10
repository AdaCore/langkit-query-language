procedure Loops (S1, S2 : String) is
   Detected : Boolean := False;
begin
   Outer : for J in S1'Range loop
      for K in S2'Range loop
         if S1 (J) = S2 (K) then
            Detected := True;
            exit Outer;                     --  FLAG
         end if;
      end loop;
   end loop Outer;

   for J in S1'Range loop
      Outer2: for K in S2'Range loop
         if S1 (J) = S2 (K) then
            Detected := True;
            exit Outer2;                    --  NOFLAG
         end if;
      end loop Outer2;
   end loop;
end Loops;

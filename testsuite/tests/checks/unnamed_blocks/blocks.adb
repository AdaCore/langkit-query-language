procedure Blocks (S : in out String) is
   I : Integer := 1;
begin
   if S'Length > 10 then
      declare                                  --  FLAG
         S1   : String (S'Range);
         Last : Positive := S1'Last;
         Idx  : Positive := 0;
      begin
         for J in S'Range loop                 --  FLAG
            S1 (Last - Idx) := S (J);
            Idx             := Idx + 1;

            for K in S'Range loop              --  FLAG
               S (K) := Character'Succ (S (K));
            end loop;
         end loop;
         S := S1;
      end;
   end if;

   begin          --  FLAG
      null;
   end;

   Named: begin   -- NOFLAG
      null;
   end Named;

   Named: loop    -- NOFLAG
      loop        --  FLAG
         null;
      end loop;
   end loop Named;
end Blocks;

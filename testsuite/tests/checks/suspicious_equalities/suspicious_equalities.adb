procedure Suspicious_Equalities is
   type Enum is (E1, E2, E3);

   B : Boolean := True;
   E : Enum := E1;
   X : Integer := 0;
   Y : Integer := 0;
begin
   if B = True and B = False then        --  FLAG
      null;
   end if;
   if B = True and then B = False then   --  FLAG
      null;
   end if;
   if B = True or B = False then         --  NOFLAG
      null;
   end if;
   if B /= True or B /= False then       --  FLAG
      null;
   end if;
   if B /= True or else B /= False then  --  FLAG
      null;
   end if;
   if B /= True and B /= False then      --  NOFLAG
      null;
   end if;
   if E = E1 and then E = E2 then        --  FLAG
      null;
   end if;
   if E /= E1 or else E /= E2 then       --  FLAG
      null;
   end if;

   if X = 1 and x = 2 then               --  FLAG
      null;
   end if;
   if X = 1 and then x = 2 then          --  FLAG
      null;
   end if;
   if X /= 1 or x /= 2 then              --  FLAG
      null;
   end if;
   if X /= 1 or else x /= 2 then         --  FLAG
      null;
   end if;

   if X = 1 and Y = 5 and X = 2 then     --  FLAG
      null;
   end if;
   if X = 1 and (Y = 5 and X = 2) then   --  FLAG
      null;
   end if;
   if (X = 1 and Y = 5) and (X = 2 and Y = 3)  --  FLAG
   then
      null;
   end if;
   if (X = 1 and Y = 5) or X /= 2 then         --  NOFLAG
      null;
   end if;
   if (X = 1 and Y = 5) or X /= 2 or X /= 2    --  FLAG
   then
      null;
   end if;
   if X = (Y + 1) and then Y < 0 then          --  NOFLAG
      null;
   end if;
end;

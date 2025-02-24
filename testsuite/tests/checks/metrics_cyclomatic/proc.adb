procedure Proc (I : in out Integer; S : String) is   --  FLAG Complexity 7
   task type T;
   protected type Prot is
      procedure P1;
      entry E1;
   end Prot;

   task body T is   --  FLAG Complexity 6
   begin
      case I is
         when 1 => null;
         when 2 => null;
         when 3 => null;
         when 4 => null;
         when 5 => null;
         when others => null;
      end case;
   end T;

   protected body Prot is
      procedure P1 is  --  FLAG Complexity 6
      begin
         case I is
            when 1 => null;
            when 2 => null;
            when 3 => null;
            when 4 => null;
            when others => if I > 0 then null; end if;
         end case;
      end P1;

      entry E1 when True is   --  FLAG Complexity 7
      begin
         case I is
            when 1 => null;
            when 2 => null;
            when 3 => null;
            when 4 => null;
            when others => I := (if I > 0 then 1 elsif False then 2 else 3);
         end case;
      end E1;
   end Prot;

   task U is
      entry E1;
      entry E2;
      entry E3;
      entry E4;
   end U;

   task body U is  --  FLAG Complexity 6
   begin
      select
         accept E1;
      or
         accept E2;
      or
         accept E3;
      or
         accept E4;
      or
         terminate;
      end select;
   end U;

begin
   if I in 1 .. 10 then
      for J in S'Range loop
         if S (J) = ' ' then
            if I < 10 then
               I := 10;
            end if;
         end if;

         I := I + Character'Pos (S (J));
      end loop;
   elsif S = "abs" then
      if I > 0 then
         I := I + 1;
      end if;
   end if;
end Proc;

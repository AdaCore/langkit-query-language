procedure Proc (I : in out Integer; S : String) is   --  FLAG (7)
   task type T;
   protected type Prot is
      procedure P1;
      entry E1;
   end Prot;

   task body T is   --  FLAG (6)
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
      procedure P1 is  --  FLAG (6)
      begin
         case I is
            when 1 => null;
            when 2 => null;
            when 3 => null;
            when 4 => null;
            when others => if I > 0 then null; end if;
         end case;
      end P1;

      entry E1 when True is   --  FLAG (7)
      begin
         case I is
            when 1 => null;
            when 2 => null;
            when 3 => null;
            when 4 => null;
            when others => I := if I > 0 then 1 elsif False then 2 else 3;
         end case;
      end E1;
   end Prot;

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

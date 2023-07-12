procedure Ranges is
   subtype My_Int is Integer range -20 .. -10;
   type Str is new String (1 .. 10);
   Obj : Str;
   X   : Integer := 0;

begin
   for J in Integer'Range loop    --  FLAG
      if J in Natural'Range then  --  FLAG
         null;
      elsif J in Natural'First .. Natural'Last then  --  FLAG
         null;
      elsif J in Obj'First .. Obj'Last then  --  FLAG
         null;
      end if;
   end loop;

   for J in Str'Range loop    -- NOFLAG
      Obj (J) := ' ';
   end loop;

   case X is
      when Obj'First .. Obj'Last => null;   --  FLAG
      when My_Int'Range          => null;   --  FLAG
      when others => null;
   end case;
end Ranges;

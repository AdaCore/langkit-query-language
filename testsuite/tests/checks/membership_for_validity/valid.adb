procedure Valid is
   subtype My_Int is Integer range 1 .. 10;

   X : My_Int;
   Y : Integer;

begin
   if X in My_Int then                           --  FLAG
      null;
   elsif X in My_Int'First .. My_Int'Last then   --  FLAG
      null;
   elsif X in Integer'First .. Integer'Last then -- NOFLAG
      null;
   elsif Y in Integer then                       --  FLAG
      null;
   elsif X in Integer then                       -- NOFLAG
      null;
   end if;
end Valid;

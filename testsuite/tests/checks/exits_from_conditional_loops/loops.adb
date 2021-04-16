function Loops (S : String) return Natural is
   Result : Natural := 0;
begin
   for J in S'Range loop
      exit when S (J) = '@';  --  FLAG
      Result := Result + J;
   end loop;

   return 0;
end Loops;

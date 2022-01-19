function Dup (X : Integer) return Integer is
begin
   if X > 0 then
      declare       --  FLAG with line 13
         A : Integer := X;
         B : Integer := A + 1;
         C : Integer := B + 1;
         D : Integer := C + 1;
      begin
         return D;
      end;
   else
      declare       --  duplicate of line 4
         A : Integer := X;
         B : Integer := A + 1;
         C : Integer := B + 1;
         D : Integer := C + 1;
      begin
         return D;
      end;
   end if;
end Dup;

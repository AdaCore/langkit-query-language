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

   declare
      K : Boolean :=
        (if 1 = 2 then (
           1 /= 2
           and then 1 /= 2
           and then 1 = 2
           and then 1 = 2
        ));
      --  should not crash the rule because of the omitted else-expression.
      --  The dummy expression in the then branch is necessary to trigger the
      --  access check fail in the original bug.
   begin
      if Test then
         null;
      end if;
   end;
end Dup;

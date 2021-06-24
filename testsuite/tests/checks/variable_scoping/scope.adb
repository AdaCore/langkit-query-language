procedure Scope is
   X : Integer;  --  FLAG
begin
   declare
      Y : Integer := 42;
   begin
      X := Y;
   end;
end;

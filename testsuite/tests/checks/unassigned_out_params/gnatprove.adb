procedure Gnatprove (X : out Integer) is    --  NOFLAG

   procedure Delegate (X : out Integer);    --  NOFLAG

   procedure Delegate (X : out Integer) is  --  NOFLAG
   begin
      X := 42;
   end;
begin
   Delegate (X);
end;

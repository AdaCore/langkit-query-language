procedure Gnatprove (X : out Integer) is    --  NO FLAG

   procedure Delegate (X : out Integer);    --  NO FLAG

   procedure Delegate (X : out Integer) is  --  NO FLAG
   begin
      X := 42;
   end;
begin
   Delegate (X);
end;

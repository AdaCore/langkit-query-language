procedurE Test is  -- FLAG
   B : Integer;
   A : access Integer := B'Access;  -- NOFLAG
begin  -- NOFLAG
   DECLARE  -- FLAG
   begin
      NULL;  -- FLAG
   end;
end Test; -- NOFLAG

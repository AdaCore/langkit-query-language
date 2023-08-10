with System.Assertions;

procedure P is
   F : Boolean := False;
begin
   begin
      raise System.Assertions.Assert_Failure;   --  FLAG
      pragma Assert (False);                    --  FLAG
      pragma Assert (F > 0);                    --  NOFLAG
   end;
exception
  when others =>
    null;
end;

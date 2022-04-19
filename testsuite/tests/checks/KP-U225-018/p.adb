procedure P is
   type T is delta 0.01 range -128.0 .. 128.0;

   procedure Call (X : in out Long_Float) is
   begin
      null;
   end;

   Q : T := 12.5;
begin
   Call (Long_Float (Q));  --  FLAG
end;

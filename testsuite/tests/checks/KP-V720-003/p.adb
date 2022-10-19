procedure P is
   procedure Call (X : in out Integer; Y : Integer) with Import;

   X : Integer;
begin
   Call (X, 1);  --  FLAG
end P;

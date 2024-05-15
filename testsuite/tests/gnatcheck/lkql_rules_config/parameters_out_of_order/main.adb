procedure Main is
   procedure P1 (X : Integer; Y : out Boolean);  --  NOFLAG
   procedure P2 (X : out Integer; Y : Boolean);  --  FLAG
begin
   null;
end Main;

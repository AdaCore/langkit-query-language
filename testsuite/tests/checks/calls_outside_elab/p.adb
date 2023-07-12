package body P is

   function Create return Integer is (1);
   function Create2 return Integer is (1);

   function Create3 return Integer renames Create;

   function F (I : Integer) return Integer is (Create + I);  --  FLAG

   procedure Subp1 is
      X : Integer := Create;    --  FLAG
   begin
      X := Create;              --  FLAG
      X := Create2;             -- NOFLAG
      X := Create3;             -- NOFLAG
   end;

   Global : Integer := Create;  -- NOFLAG
begin
   Global := Create;            -- NOFLAG

   begin
      Global := Create;         -- NOFLAG
   end;
end;

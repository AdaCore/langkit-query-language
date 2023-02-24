package body P is

   function Create return Integer is (1);
   function Create2 return Integer is (1);

   function Create3 return Integer renames Create;

   function F (I : Integer) return Integer is (Create + I);  --  FLAG

   procedure Subp1 is
      X : Integer := Create;    --  FLAG
   begin
      X := Create;              --  FLAG
      X := Create2;             --  NO FLAG
      X := Create3;             --  NO FLAG
   end;

   Global : Integer := Create;  --  NO FLAG
begin
   Global := Create;            --  NO FLAG
end;

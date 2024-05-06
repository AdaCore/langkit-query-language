procedure Main is
   package Test is
      V1 : Integer := 42;
      V2 : Integer := 50;
      V3 : Integer := 60;
   end Test;

   procedure F1 (X : Integer) is null;
   procedure F2 (X : Integer) is null;
begin
   F1 (Test.V1);  --  FLAG
   F1 (Test.V2);  --  NOFLAG
   F1 (Test.V3);  --  NOFLAG

   F2 (Test.V1);  --  FLAG
   F2 (Test.V2);  --  FLAG
   F2 (Test.V3);  --  FLAG
end Main;

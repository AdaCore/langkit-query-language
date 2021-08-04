procedure Calls is

   procedure Unknown with Import;

   function X return Integer is (1);

   Val : Integer := X;      --  NO FLAG
begin
   Unknown;                 --  FLAG
end Calls;

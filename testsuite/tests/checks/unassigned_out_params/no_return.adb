procedure No_Return (X : out Integer) is

   procedure Raise_Exc with Import, No_Return;

begin
   X := 1;
exception
   when others =>
      Raise_Exc;
end No_Return;

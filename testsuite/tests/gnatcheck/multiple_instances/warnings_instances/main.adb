procedure Main is
   Value : Integer;

   function Test (I : Integer) return Boolean is  --  FLAG
      Value : Integer;  --  FLAG (4)
   begin
      Value := 5;
      return False;
   end;
begin
   null;
end Main;

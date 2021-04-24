with Ada.Text_IO; use Ada.Text_IO;
procedure Inst is
   package My_Int_IO is new Integer_IO (Integer);   --  FLAG
begin
   null;
end Inst;

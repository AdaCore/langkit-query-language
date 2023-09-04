with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Conversion;
procedure Pos (I : in out Integer) is
   type My_Int is range -12345 .. 12345;

   function To_My_Int is new Ada.Unchecked_Conversion
     (Source => Integer, Target => My_Int);

   function To_Integer is new Ada.Unchecked_Conversion
     (My_Int, Integer);                                --  FLAG (2)

   package My_Int_IO is new Ada.Text_IO.Integer_IO (My_Int);  --  NOFLAG
begin
   null;
end Pos;

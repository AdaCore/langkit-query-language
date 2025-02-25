with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Conversion;
procedure Pos (I : in out Integer) is
   type My_Int is range -12345 .. 12345;

   function To_My_Int is new Ada.Unchecked_Conversion
     (Source => Integer, Target => My_Int);

   function To_Integer is new Ada.Unchecked_Conversion
     (My_Int, Integer);                                --  FLAG (2)

   package My_Int_IO is new Ada.Text_IO.Integer_IO (My_Int);  --  NOFLAG

   generic
      A, B, C : Integer;
   package G is
   end G;

   package I_G_1 is new G (1, 2, 3);  -- FLAG (3)
   package I_G_2 is new G (1, 2, C => 3);  -- FLAG (2)
   package I_G_3 is new G (1, C =>  2, B => 3);  -- FLAG (1)

   generic package R_G renames G;

   package I_RG is new R_G (1, 2, 3); -- FLAG (3)

   generic
      A : Integer;
   package H is
   end H;

   package I_H is new H (1);  -- NOFLAG

   generic package R_H renames H;

   package I_RH is new R_H (1); -- NOFLAG

   generic
      A, B, C : Integer := 0;
   package D is
   end D;

   package I_D is new D (1);  -- FLAG

begin
   null;
end Pos;

package P is
   type Enum is (one, two, three);

   type Bits is array (Natural range <>) of Boolean;
   pragma Pack (Bits);

   type R1 (D : Enum := one) is record
      X1 : integer;
      case D is
         when one => X3 : Bits (1 .. 1024);
         when two => X4 : Bits (1 .. 512);
         when three => X5 : integer;
      end case;
   end record;

   type R2 is new R1;  --  FLAG
   for R2 use record
      D  at 4 range 0 .. 7;
      X5 at 8 range 0 .. 31;
      X3 at 8 range 0 .. 1023;
      X4 at 8 range 0 .. 511;
   end record;

end P;

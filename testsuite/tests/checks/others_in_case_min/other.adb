procedure Other is
   type Enum is (A, B, C, D, E);
   subtype Enum1 is Enum range A .. B;
   X : Enum;

   type Int is new Integer range 1 .. 10;
   K : Int;

   type Mod_Int is mod 2 ** 2;
   L : Mod_Int;

   Y : Integer;

   function Get return String with Import;
   S : String := Get;

begin
   case X is
      when Enum1  => null;
      when C      => null;
      when others => null;  --  FLAG if N=3
   end case;

   case K is
      when 1 .. 6 | 8 => null;
      when 7          => null;
      when others     => null;   --  FLAG
   end case;

   case L is
      when 0 | 1  => null;
      when others => null;  --  FLAG
   end case;

   --  Test on a large integer
   case Y is
      when 0 => null;
      when others => null;  --  NOFLAG
   end case;

   --  Test on an attribute
   case S'Length is
      when 0 => null;
      when others => null;  --  NOFLAG
   end case;
end Other;

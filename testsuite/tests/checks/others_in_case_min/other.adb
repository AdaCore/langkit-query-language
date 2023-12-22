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

   C : Character;

   W_C : Wide_Character;

   W_W_C : Wide_Wide_Character;

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

   --  Test on a char
   case C is
      when 'A' => null;
      when others => null;   --  NOFLAG
   end case;

   case C is
      when Character'First .. Character'Last => null;
      when others => null;   -- NOFLAG because all case handled
   end case;

   case C is
      when Character'First .. Character'Val (255 - 1) => null;
      when others => null;   -- FLAG if N=3
   end case;

   case C is
      when Character'First .. Character'Val (255 - 3) => null;
      when others => null    -- NOFLAG if N=3
   end case;

   --  Test on wide char
   case W_C is
      when 'A' => null;
      when others => null;   --  NOFLAG
   end case;

   case W_C is
      when Wide_Character'First .. Wide_Character'Last => null;
      when others => null;   -- NOFLAG because all case handled
   end case;

   case W_C is
      when Wide_Character'First .. Wide_Character'Val (65535 - 1) => null;
      when others => null;   -- FLAG if N=3
   end case;

   case W_C is
      when Wide_Character'First .. Wide_Character'Val (65535 - 3) => null;
      when others => null    -- NOFLAG if N=3
   end case;

   --  Test on wide wide char
   case W_W_C is
      when 'A' => null;
      when others => null;   --  NOFLAG
   end case;

   case W_W_C is
      when Wide_Wide_Character'First .. Wide_Wide_Character'Last => null;
      when others => null;   -- NOFLAG because all case handled
   end case;

   case W_W_C is
      when Wide_Wide_Character'First .. Wide_Wide_Character'Val (2147483647 - 1) => null;
      when others => null;   -- FLAG if N=3
   end case;

   case W_W_C is
      when Wide_Wide_Character'First .. Wide_Wide_Character'Val (2147483647 - 3) => null;
      when others => null    -- NOFLAG if N=3
   end case;
end Other;

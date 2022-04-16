package Main is
   type Bit_T is range 0 .. 1;
   type Bits_T is array (Positive range <>) of Bit_T;
   type User is range 0 .. 511;

   type Data is record
      Length  : Natural;
      Message : Bits_T (1 .. 200);
   end record;
   function "=" (L, R : in Data) return Boolean;

   type Variant (Disc : User := User'First) is record  --  FLAG
      D : Data;
      case Disc is
         when 99     => Stm_Id : Positive;
         when others => null;
      end case;
   end record;

end Main;

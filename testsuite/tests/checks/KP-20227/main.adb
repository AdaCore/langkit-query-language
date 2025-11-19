procedure Main is
   type R_1 (B : Boolean) is record  --  FLAG
      case B is
         when True =>
            I : Integer;

         when False =>
            C : Character;
      end case;
   end record;

   pragma Unchecked_Union (R_1);

   type R_2 (B : Boolean) is record  --  FLAG
      case B is
         when True =>
            I : Integer;

         when False =>
            C : Character;
      end case;
   end record
   with Unchecked_Union;

   type R_3 (B : Boolean) is record  --  NOFLAG
      case B is
         when True =>
            I : Integer;

         when False =>
            C : Character;
      end case;
   end record;

   subtype S_R_1 is R_1;   --  FLAG
   type D_R_1 is new R_1;  --  FLAG
begin
   null;
end Main;

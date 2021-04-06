package body Enumeration_Ranges_In_Case_Statements is
   type Enum is (A, B, C, D, E, F, G);
   function F (J : Integer) return Enum is separate;

   procedure Bar (I : in out Integer) is
      type Arr is array (A .. C) of Integer;
      subtype S is Integer range -10 .. 10;
   begin
      case F (I) is
         when Arr'Range  =>  --  FLAG
            I := I + 1;
         when D .. E =>      --  FLAG
            null;
         when F | G =>       --  NO FLAG
            null;
         when others =>      --  NO FLAG
            null;
      end case;

      case I is
         when S =>   -- NO FLAG
            return;
         when 11 .. 20 =>   -- NO FLAG
            return;
         when others =>      --  NO FLAG
            null;
      end case;
   end Bar;
end Enumeration_Ranges_In_Case_Statements;

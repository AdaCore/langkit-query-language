package body Enumeration_Ranges_In_Case_Statements is
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
         when F | G =>       -- NOFLAG
            null;
         when others =>      -- NOFLAG
            null;
      end case;

      case I is
         when S =>   -- NOFLAG
            return;
         when 11 .. 20 =>   -- NOFLAG
            return;
         when others =>      -- NOFLAG
            null;
      end case;
   end Bar;
end Enumeration_Ranges_In_Case_Statements;

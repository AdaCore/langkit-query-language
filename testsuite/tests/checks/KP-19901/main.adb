procedure Main is
   package Test is
      type P0 is tagged null record;                          -- NOFLAG
      type P1 is tagged null record with Predicate => True;   -- NOFLAG
      type P2 is tagged private;                              -- NOFLAG
      type P3 is new P0 with private with Predicate => True;  -- NOFLAG
      type P4 is new P1 with private with Predicate => True;  -- NOFLAG
      type P6 is tagged private;                              -- NOFLAG
   private
      type P2 is tagged null record with Predicate => True;       -- NOFLAG
      type P3 is new P0 with null record;                         -- NOFLAG
      type P4 is new P1 with null record;                         -- NOFLAG
      type P5 is new P2 with null record with Predicate => True;  -- FLAG
      type P6 is tagged null record;                              -- NOFLAG
      pragma Predicate (P6, True);
   end Test;

   package body Test is
   end Test;

   subtype S0 is Test.P0 with Predicate => True;  -- NOFLAG
   subtype S1 is Test.P1 with Predicate => True;  -- NOFLAG
   subtype S2 is Test.P2 with Predicate => True;  -- FLAG
   subtype S3 is Test.P3 with Predicate => True;  -- FLAG
   subtype S4 is Test.P4 with Predicate => True;  -- FLAG
   subtype S5 is S4;                              -- NOFLAG
   subtype S6 is Test.P2;                         -- NOFLAG
   subtype S7 is S6 with Predicate => True;       -- FLAG

   type T0 is new Test.P0 with null record with Predicate => True;  -- NOFLAG
   type T1 is new Test.P1 with null record with Predicate => True;  -- NOFLAG
   type T2 is new Test.P2 with null record with Predicate => True;  -- FLAG
   type T3 is new Test.P3 with null record with Predicate => True;  -- FLAG
   type T4 is new Test.P4 with null record with Predicate => True;  -- FLAG
   type T5 is new T4 with null record;                              -- NOFLAG
   type T6 is new Test.P2 with null record;                         -- NOFLAG
   type T7 is new T6 with null record with Predicate => True;       -- FLAG

   subtype S8 is S6;                                           -- NOFLAG
   type T8 is new S8 with null record with Predicate => True;  -- FLAG

   type T9 is new Test.P6 with null record with Predicate => True;  -- FLAG
   type T10 is new Test.P2 with null record;                        -- FLAG
   pragma Predicate (T10, True);
begin
   null;
end;

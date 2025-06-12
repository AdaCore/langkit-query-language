procedure Main_Static is
   package Test is
      type P0 is new String (1 .. 1) with Static_Predicate => True;  -- NOFLAG
      type P1 is private;                                            -- NOFLAG
   private
      type P1 is new String (1 .. 2) with Static_Predicate => True;  -- NOFLAG
   end Test;

   package body Test is
   end Test;

   subtype S0 is Test.P0 with Dynamic_Predicate => True;  -- NOFLAG
   type T0 is new Test.P0 with Dynamic_Predicate => True; -- NOFLAG
   type T1 is new Test.P1 with Dynamic_Predicate => True; -- FLAG
begin
   null;
end;

------------------------------------------------------------------------------
--                                                                          --
--              GNATCHECK QUALIFICATION TEST SUITE COMPONENTS               --
--                                                                          --
--                                                                          --
--                       Copyright (C) 2021, AdaCore                        --
--                                                                          --
------------------------------------------------------------------------------

package body Overloaded_Operators is

   type Small is range -100 .. 100;
   type Big   is range -1_000 .. 1_000;

   function F1_G (L : T_F) return T_F is (-L);

   function F2_G (L, R : T_F) return T_F is (L + R);

   function Fun1 (I : Integer) return Integer is (I + 1);

   --  Function bodies and stubs that have separate specs

   function "+" (L, R : Int) return Int is (1);                --  NO FLAG

   function "*" (L, R : Int) return Boolean is                 --  NO FLAG
   begin
      return L + R > 0;
   end "*";

   function "and" (L : Int; R : Boolean) return Boolean is     --  NO FLAG
   begin
      return R or (L > 0);
   end "and";

   --  Renaming as body:
   function "not" (L : Char) return Char renames Fun_Char;     --  NO FLAG

   --  Function instantiations:

   function "-" is new F1_G (Small);                           --  FLAG
   function "not" is new F1_G (Big);                           --  FLAG

   function "*" is new F2_G (Small);                           --  FLAG
   function "and" is new F2_G (Big);                           --  FLAG

   --  Overloading deep inside nested scopes

   procedure Proc1 is
      procedure Proc2 is

         procedure Proc3 is
            type Large is range -100_000 .. 100_000;
         begin

            declare
               I : Large;
            begin

               declare
                  function "+" (L, R : Large) return Large;    --  FLAG

                  function "+" (L, R : Large) return Large is  --  NO FLAG
                  begin
                     return L * R - L - R;
                  end "+";

               begin
                  I := I + 1;
               end;

            end;

         end Proc3;

      begin
         null;
      end Proc2;
   begin
      null;
   end Proc1;

end Overloaded_Operators;

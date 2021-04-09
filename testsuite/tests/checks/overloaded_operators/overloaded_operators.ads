------------------------------------------------------------------------------
--                                                                          --
--              GNATCHECK QUALIFICATION TEST SUITE COMPONENTS               --
--                                                                          --
--                                                                          --
--                       Copyright (C) 2021, AdaCore                        --
--                                                                          --
------------------------------------------------------------------------------

package Overloaded_Operators is

   generic
      type T_F is range <>;
   function F1_G (L : T_F) return T_F;

   generic
      type T_F is range <>;
   function F2_G (L, R : T_F) return T_F;

   type Int is range 1 .. 100;

   type Char is new Character range 'a' .. 'z';

   function Fun_Char (Ch : Char) return Char is ('a');

   function Fun1 (I : Integer) return Integer;

   function Fun2 (I : Integer; J : Integer) return Integer is (I + J);

   --  Function declarations

   function "+" (L, R : Int) return Int;                         --  FLAG
   function "*" (L, R : Int) return Boolean;                     --  FLAG
   function "-" (L : Boolean) return Boolean is (not L);         --  FLAG

   function "and" (L : Int; R : Boolean) return Boolean;         --  FLAG
   function "not" (L : Char) return Char;                        --  FLAG

   --  Function renamings

   function "abs" (Left : Integer) return Integer renames Fun1;  --  FLAG
   function "-" (I, J : Integer) return Integer renames Fun2;    --  FLAG

   function "not" (Left : Integer) return Integer renames Fun1;  --  FLAG
   function "and" (I, J : Integer) return Integer renames Fun2;  --  FLAG

   --  formal functions
   generic
      type T_F is range <>;
      with function "+" (L, R : T_F) return T_F;                 --  FLAG
      with function "abs" (L : T_F) return T_F;                  --  FLAG
      A, B : T_F;
   package Pack_G is
      Var_1 : T_F := A + B;
      Var_2 : T_F := abs A;
   end Pack_G;

end Overloaded_Operators;

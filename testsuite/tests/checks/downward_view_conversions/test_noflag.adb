procedure Test is
   package P is
      type T is tagged private;
      subtype S is T;
      type T_Access is access all T;
      type T_Constant_Access is access constant T;

      type U is new T with private;
   private
      type T is tagged null record;
      type U is new T with null record;
   end P;

   package body P is
      function F (X : T_Access) return T_Constant_Access is (T_Constant_Access (X)); -- NOFLAG

      function G (X : U) return T is (T (X)); -- NOFLAG

      function I (X : T) return T is (T (X)); -- NOFLAG

      function J (X : T) return S is (S (X)); -- NOFLAG
   end P;
begin
   null;
end Test;

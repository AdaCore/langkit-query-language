package Param is
   type T is tagged private;

   procedure P1 (This : in out T);                 --  NO FLAG
   procedure P2 (That : in out T);                 --  FLAG
   procedure P3 (I : Integer; This : in out T);    --  FLAG
   procedure P4 (This : Integer; That : in out T); --  FLAG
   procedure P5 (Not_This : T) is separate;        --  FLAG
   procedure P6 (This : access T);                 --  NO FLAG

   function F1 (B : Boolean) return T;             -- NO FLAG

private
   type T is tagged null record;
end Param;

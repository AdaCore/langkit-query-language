package Param is
   type T is tagged private;

   procedure P1 (This : in out T);                 --  NOFLAG
   procedure P2 (That : in out T);                 --  FLAG
   procedure P3 (I : Integer; This : in out T);    --  FLAG
   procedure P4 (This : Integer; That : in out T); --  FLAG
   procedure P5 (Not_This : T) is separate;        --  FLAG
   procedure P6 (This : access T);                 --  NOFLAG

   function F1 (B : Boolean) return T;             --  NOFLAG

private
   type T is tagged null record;
end Param;

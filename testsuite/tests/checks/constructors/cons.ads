package Cons is

   type T is tagged record
      I : Integer;
   end record;

   subtype ST_CW is T'Class;
   type A_ST_CW is access ST_CW;

   type A_T is access T;
   subtype S_A_T is A_T;

   function Fun (I : Integer) return T;                    -- FLAG
   function Bar (JJJ : Integer) return T renames Fun;      -- FLAG
   function Foo (KKK : Integer) return T is ((I => KKK));  -- FLAG

   function CW (I : T'Class) return T;                     -- FLAG
   function CW1 (I : ST_CW) return T;                      -- FLAG
   function CW2 (I : A_ST_CW) return T;                    -- FLAG

   function Fun1 (X : access T) return T;                  -- NOFLAG
   function Fun2 (I : Integer) return access T;            -- FLAG
   function Fun3 (I : S_A_T) return access T;              -- FLAG

   type T1 is tagged private;

   type T2 is tagged private;
   function Fun_T2 return T2;                              -- FLAG

   type T3 is tagged private;
   function Fun_T3 (I : Integer) return T3;                -- FLAG

   type T4 is tagged private;
   function Fun_T4 (I : Integer) return T4;                -- FLAG

private
   type T1 is tagged null record;
   type T2 is tagged null record;
   type T3 is tagged null record;
   type T4 is tagged null record;
end Cons;

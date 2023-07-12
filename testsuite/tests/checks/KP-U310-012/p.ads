package P is

   type T is record
      D : Duration;
   end record;

   type T_Array is array (Positive range <>) of T;
   type T_Array2 (<>) is private;
   type Q (N : Natural) is private;

   function Create return Q;                          --  FLAG
   function Create return T_Array2;                   --  FLAG

   function Create return T_Array with Post => True;  --  FLAG
   function No_Flag return T_Array;                   -- NOFLAG

private

   type T_Array2 is array (Positive range <>) of T
     with Type_Invariant => True;

   type Q (N : Natural) is record
      P : T_Array (1 .. N);
   end record with Type_Invariant => True;

end P;

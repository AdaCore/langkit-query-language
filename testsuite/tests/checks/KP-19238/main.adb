procedure Main is
   type T1 is null record with Convention => C_Pass_By_Copy;
   type T2 is null record with Convention => C;
   type T3 is null record;

   type D1_T1 is new T1 with Convention => C_Pass_By_Copy;  -- NOFLAG
   type D1_T2 is new T1 with Convention => C;  -- FLAG
   type D1_T3 is new T1;  -- NOFLAG

   type D2_T1 is new T2 with Convention => C_Pass_By_Copy;  -- NOFLAG
   type D2_T2 is new T2 with Convention => C;  -- NOFLAG
   type D2_T3 is new T2;  -- NOFLAG

   type D3_T1 is new T3 with Convention => C_Pass_By_Copy;  -- NOFLAG
   type D3_T2 is new T3 with Convention => C;  -- NOFLAG
   type D3_T3 is new T3;  -- NOFLAG

   type D4 is new T1;  -- NOFLAG
   type D5 is new D4 with Convention => C;  -- FLAG

   subtype S1 is T1;
   type D6 is new S1 with Convention => C;  -- FLAG

   type D4 is new T3 with Convention => C_Pass_By_Copy;  -- NOFLAG
   type D5 is new D4 with Convention => C;  -- FLAG

   -- No need to flag D6 since D5 is already flagged
   type D6 is new D5;  -- NOFLAG
begin
   null;
end Main;

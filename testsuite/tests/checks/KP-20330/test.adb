with System; use System;

procedure Test is
   type R1 is tagged record
      C : Integer;
   end record
     with Scalar_Storage_Order => System.High_Order_First,
          Alignment => 1;

   type R2 is record
      C : Integer;
   end record;

   type R3 is tagged null record
     with Scalar_Storage_Order => System.High_Order_First,
          Alignment => 1;

   type R4 is new R3 with record
      D : Integer;
   end record;

   type R5 is new R1 with null record;

   type R6 is new R1 with record
      D : Integer;
   end record;

   subtype R7 is R1;

   subtype R8 is R2;

   subtype R9 is R5;

   type R10 is new R2;

   type Arr is array (1 .. 10) of Integer;

   type R11 is tagged record
      C : Arr;
   end record
     with Scalar_Storage_Order => System.High_Order_First,
          Alignment => 1;

   subtype R12 is R8;

   subtype R13 is R9;

   O1  : constant R1 := (C => 1);  -- FLAG
   O2  : R1 := (C => 1);  -- NOFLAG
   O3  : constant R2 := (C => 1);  -- NOFLAG
   O4  : constant R5 := (C => 1);  -- FLAG
   O5  : constant R6 := (others => 1);  -- NOFLAG
   O6  : constant R7 := (others => 1);  -- FLAG
   O7  : constant R8 := (others => 1);  -- NOFLAG
   O8  : constant R9 := (others => 1);  -- FLAG
   O9  : constant R10 := (others => 1);  -- NOFLAG
   O10 : constant R3 := (null record);  -- NOFLAG
   O11 : constant R4 := (D => 1);  -- FLAG
   O12 : constant R11 := (C => (1, 2, 3));  -- NOFLAG
   O13 : constant R12 := (others => 1);  -- NOFLAG
   O14 : constant R13 := (others => 1);  -- FLAG
begin
   null;
end;

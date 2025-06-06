with System;

procedure P2 is
  type Angle_Type is new Float range 0.0 .. 359.99;
  -- Derived floating point type

  -- Array type with floating point elements.
  type My_Array_Type is array (1 .. 2) of Angle_Type;

  -- Big endian byte order
  for My_Array_Type'Scalar_Storage_Order use System.High_Order_First;

  type My_Array_Type2 is array (1 .. 2) of Angle_Type;

  My_Array  : My_Array_Type := (1.1, 2.2);
  My_Array2 : My_Array_Type2 := (1.1, 2.2);

  Is_Valid  : Boolean := My_Array (1)'Valid;   --  FLAG

begin
  Is_Valid := My_Array2 (1)'Valid;             --  NOFLAG
end P2;

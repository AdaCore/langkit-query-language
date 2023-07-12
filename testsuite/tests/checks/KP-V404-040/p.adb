with System;

procedure P is
  type Angle_Type is new Float range 0.0 .. 359.99;
  -- Derived floating point type

  -- Record type with floating point elements.
  type My_Record_Type is record
    Az : Angle_Type;
    El : Angle_Type;
  end record;

  -- Big endian byte order
  for My_Record_Type'Bit_Order use System.High_Order_First;
  for My_Record_Type'Scalar_Storage_Order use System.High_Order_First;

  type My_Record_Type2 is record
    Az : Angle_Type;
    El : Angle_Type;
  end record;

  My_Record  : My_Record_Type := (1.1, 2.2);
  My_Record2 : My_Record_Type2 := (1.1, 2.2);

  Is_Valid  : Boolean := My_Record.Az'Valid;   --  FLAG

begin
  Is_Valid := My_Record2.Az'Valid;             -- NOFLAG
end P;

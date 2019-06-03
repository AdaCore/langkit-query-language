package Type_Declarations is
   type Idx is range 0 .. 100;

   -------------------------------------------------------------------------
   --  Calls to subprograms with parameters of these types may be flagged --
   -------------------------------------------------------------------------

   type U_Arr_1 is array (Idx range <>) of Character with Pack => True;
   subtype S_U_Arr_1 is U_Arr_1 (1 .. 2);

   type U_Arr_2 is array (Idx range <>) of Character;
   pragma Pack (U_Arr_2);
   subtype S_U_Arr_2 is U_Arr_2 (1 .. 2);

   ---------------------------------------------------------------------
   --  Calls to subprograms with parameters of these types should not --
   --  be flagged                                                     --
   ---------------------------------------------------------------------

   type U_Arr_3 is array (Idx range <>) of Character;
   --  non-packed type

   type C_Arr_1 is array (1 .. 10) of Character with Pack => True;
   --  constrained array
   type C_Arr_2 is array (1 .. 10) of Character;
   --  constrained array

   ----------------------------------------------------------------
   --  Components of this record types as actuals may be flagged --
   ----------------------------------------------------------------

   type Rec_1 is record
      C : U_Arr_1 (1 .. 2);
   end record;

   for Rec_1 use record
      C at 8 range 0 .. 31;
   end record;

   type Rec_2 is record
      C : U_Arr_2 (1 .. 2);
   end record;

   for Rec_2 use record
      C at 8 range 0 .. 31;
   end record;

   -----------------------------------------------------------------------
   --  Components of this record types as actuals should not be flagged --
   -----------------------------------------------------------------------

   type Rec_3 is record
      C : U_Arr_1 (1 .. 2);
   end record;
   --  no representation clause

   type Rec_4 is record
      C : U_Arr_3 (1 .. 2);
   end record;
   --  non-packed component
   for Rec_4 use record
      C at 8 range 0 .. 31;
   end record;

   type Rec_5 is record
      C : C_Arr_1;
   end record;
   --  component of constrained array type
   for Rec_5 use record
      C at 8 range 0 .. 1024;
   end record;

   type Rec_6 is record
      C : C_Arr_2;
   end record;
   --  component of constrained array type and non-packed
   for Rec_6 use record
      C at 8 range 0 .. 1024;
   end record;

end Type_Declarations;

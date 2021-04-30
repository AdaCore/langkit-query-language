with Ada.Finalization; use Ada.Finalization;
with Ada.Streams;      use Ada.Streams;

with Pack_G;

package Test_Deriving_From_Predefined_Type is

   type My_Controlled_1 is new Controlled with record              --  NO FLAG
      I : Integer;
   end record;

   type My_Controlled_2 is new Controlled with private;            --  NO FLAG

   type My_Stream_1 is abstract new Root_Stream_Type with record   --  NO FLAG
      I : Integer;
   end record;

   type My_Stream_2 is abstract new Root_Stream_Type with private; --  NO FLAG

   type My_Float is new Float;                                     --  FLAG

   type My_My_Float is new My_Float;                               --  FLAG

   subtype S_My_My_Float is My_My_Float range -100.0 .. 100.0;

   type My_Private_F is private;

   subtype S_My_PrivateF is My_Private_F;

   type My_String is new String;                                   --  FLAG

   subtype My_String_10 is My_String (1 .. 10);

   type My_Private_S is private;

   --  Test instantiation

   type My_Int1 is range -1000 .. 1000;
   type My_Float_1 is digits 8;
   type My_Array is array (1 .. 10) of My_Int1;

   package Pack_I_1 is new Pack_G (My_Int1, My_Float_1, My_Array);
   --  Nothing should be flagged for this instantiation

   package Pack_I_2 is new Pack_G (Integer, Float, My_String_10);
   --  Three diagoses are expexted here;

   type NT1 is new Pack_I_2.T1;                                        --  FLAG
   type NT2 is new Pack_I_2.T2;                                        --  FLAG
   type NT3 is new Pack_I_2.T3;                                        --  FLAG

private

   type My_Controlled_2 is new Controlled with record              --  NO FLAG
      I : Integer;
   end record;

   type My_Stream_2 is abstract new Root_Stream_Type with record   --  NO FLAG
      I : Integer;
   end record;

   type My_Private_F is new S_My_My_Float;                         --  FLAG

   type My_Private_S is new My_String_10;                          --  FLAG

end Test_Deriving_From_Predefined_Type;

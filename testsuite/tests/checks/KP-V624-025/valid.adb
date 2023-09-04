procedure Valid is

   package H1 is
      type H1_T is private;
      function Value return H1_T;
   private
      type H1_T is new Integer range -Integer'Last .. Integer'Last;
      function Value return H1_T is (0);
   end H1;

   package H2 is
      type H2_T is private;
      function Value return H2_T;
   private
      subtype S1 is H1.H1_T;
      subtype S2 is S1;
      type H2_T is new S2;
      function Value return H2_T is (H2_T (H1.Value));
   end H2;

   package H3 is
      type H3_T is private;
      function Value return H3_T;
   private
      subtype S1 is H2.H2_T;
      subtype S2 is S1;
      type H3_T is new S2;
      function Value return H3_T is (H3_T (H2.Value));
   end H3;

   package Hidden_Pkg is
      type Hidden is private;
      function Value return Hidden;
   private
      subtype S1 is H3.H3_T;
      subtype S2 is S1;
      type Hidden is new S2;
      function Value return Hidden is (Hidden (H3.Value));
   end Hidden_Pkg;

   type My_Record is record
     C : Hidden_Pkg.Hidden := Hidden_Pkg.Value;
   end record;

   Content : My_Record;
   X : Boolean;

begin
   X := Content'Valid_Scalars;    --  FLAG
   X := Content.C'Valid_Scalars;  --  NOFLAG
end Valid;

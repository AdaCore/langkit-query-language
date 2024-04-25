with Ada.Containers.Vectors;

procedure Main is
   --  Define an integer vector type

   package Integer_Vector
     is new Ada.Containers.Vectors
       (Index_Type => Natural,Element_Type => Integer);

   subtype Vector is Integer_Vector.Vector;

   --  Define a custom container with only "Add_Unnamed" assoc

   type Test_Container_1 is null record
      with Aggregate => (Empty => New_Empty,
                         Add_Unnamed => Append);

   function New_Empty return Test_Container_1 is (null record);
   procedure Append (Self : in out Test_Container_1;
                     Value : Integer) is null;

   --  Define a custom container with both "Add_Unnamed" and "New_Indexed"

   type Test_Container_2 is null record
      with Aggregate => (Empty => New_Empty,
                         New_Indexed => New_Indexed_Cont,
                         Add_Unnamed => Append,
                         Assign_Indexed => Insert);

   function New_Empty return Test_Container_2 is (null record);
   function New_Indexed_Cont(X, Y : Integer) return Test_Container_2 is (null record);
   procedure Append (Self : in out Test_Container_2;
                     Value : Integer) is null;
   procedure Insert (Self : in out Test_Container_2;
                     Index : Integer;
                     Value : Integer) is null;

   --  Create a derived container type

   subtype Derived_Container is Test_Container_2;

   --  Test container aggregate expressions

   V :         Vector := [ 1, 2, 3, 4 ];                        --  NOFLAG
   Copy :      Vector := [ for E of V => E ];                   --  FLAG
   Only_Even : Vector := [ for E of V when E mod 2 = 0 => E ];  --  FLAG

   C_1_1 : Test_Container_1 := [ 1, 2, 3, 4 ];         --  NOFLAG
   C_1_2 : Test_Container_1 := [ for E of V => E ];    --  NOFLAG
   C_2_1 : Test_Container_2 := [ 1, 2, 3, 4 ];         --  NOFLAG
   C_2_2 : Test_Container_2 := [ for E of V => E ];    --  FLAG
   D_C_1 : Derived_Container := [ 1, 2, 3, 4 ];        --  NOFLAG
   D_C_2 : Derived_Container := [ for E of V => E ];   --  FLAG
begin
   null;
end Main;

procedure Test_Ghost_Code is
   A : String := "hello";

   procedure Foo
      with Pre => A'Image = "hello",  --  NOFLAG
           Post => A'Image = "hello";  --  NOFLAG

   B : String := A'Image with Ghost;  --  NOFLAG

   function Bar return String is (A'Image) with Ghost;  --  NOFLAG

   procedure Foo is null;

   package P with Ghost is
      B : String := A'Image; --  NOFLAG
   end P;

   generic
   package Gen_Pkg is
      B : String := A'Image;  --  FLAG (via instantiation line 23)
   end Gen_Pkg;

   package Inst is new Gen_Pkg;
begin
   null;
end Test_Ghost_Code;

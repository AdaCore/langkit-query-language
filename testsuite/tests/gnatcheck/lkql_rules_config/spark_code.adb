procedure Spark_Code
   with SPARK_Mode => On
is
   A : String := "hello";

   X : array (1 .. 10) of Integer; --  FLAG

   procedure Foo
      with Pre => A'Image = "hello",  --  NOFLAG because in ghost code
           Post => A'Image = "hello";  --  NOFLAG because in ghost code

    Decl : Integer :=
     (declare A : constant Integer := 12;  --  FLAG (2) First for Ada 2022 and second for constant suffix
              B : constant Integer := 15; begin A + B);  --  FLAG for constant suffix
begin
   Decl := @ + 12;  --  FLAG
   goto Next;  --  NOFLAG because in SPARK mode
   <<Next>>
end Name;

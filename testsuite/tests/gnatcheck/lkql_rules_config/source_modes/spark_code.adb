procedure Spark_Code
   with SPARK_Mode => On
is
   type My_Int is new Integer;    --  FLAG
   type My_Int_T is new Integer;  --  NOFLAG

   A : String := "hello";

   procedure Foo
      with Pre => A'Image = "hello",  --  NOFLAG because in ghost code
           Post => A'Image = "hello";  --  NOFLAG because in ghost code

    Decl : Integer :=
     (declare A : constant Integer := 12;  --  FLAG
              B : constant Integer := 15; begin A + B);
begin
   Decl := @ + 12;  --  FLAG
   goto Next;  --  NOFLAG
   <<Next>>
end Name;

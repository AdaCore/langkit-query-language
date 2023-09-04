with Ada.Text_IO; use Ada.Text_IO;

procedure Test is

   type Point is record X, Y : Integer; end record;

   type Int_Array is array (Positive range <>) of Integer;
   Vals : Int_Array := (1, 2, 3, 4, 5, 6);
   Sum : Integer := Vals'Reduce ("+", 0);  --  FLAG

   Decl : Integer :=
     (declare A : constant Integer := 12;  --  FLAG
              B : constant Integer := 15; begin A + B);

   P : Point := (12, 15);
   P2 : Point := (P with delta Y => 18);  --  FLAG

   Vals_2 : Int_Array := (for I of Vals => I * 2); --  FLAG

   protected A is
   end A;

   protected body A is
      procedure Foo is null; --  FLAG
      function Bar return Integer is (12); --  FLAG
   end A;

   type Str is null record with String_Literal => To_Str; --  FLAG

   function To_Str (X : Wide_Wide_String) return Str is
     (null record);

   type Big_Int is null record with Integer_Literal => To_Big_Int; --  FLAG

   function To_Big_Int (X : String) return Big_Int is (null record);


   type Big_Real is null record with Real_Literal => To_Big_Real; --  FLAG

   function To_Big_Real (X : String) return Big_Real is (null record);

   function Foo (Bar: Integer with Test_Aspect) return Integer is (21);  --  FLAG

   generic
      type T is private;
      with function Foo (Self : T) return Integer
        with Post => Foo'Result > 15;
   package Bar is
   end Bar;
begin
   Put_Line (Vals'Image);  --  FLAG
   Put_Line (Sum'Image);

   Decl := @ + 12;  --  FLAG
end Test;

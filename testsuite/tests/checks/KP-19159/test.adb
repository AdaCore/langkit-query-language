procedure Test is

  type Arr1 is array (1 .. 32) of Integer;

  type Arr2 is array (1 .. 32) of Integer;

  type Int is range 1 .. 100;

  procedure Foo (A : Arr2) is
  begin
    if A (16) /= 42 then
      raise Program_Error;
    end if;
  end Foo;

  procedure Bar (X : Integer) is
  begin
     null;
  end Bar;

  A : Arr1;
  B : Int := 10;
  C : Arr2;

begin
  A (16) := 42;
  Foo (Arr2 (A));    --  FLAG
  Bar (Integer (B)); --  NOFLAG
  C := Arr2 (A);     --  NOFLAG
end Test;

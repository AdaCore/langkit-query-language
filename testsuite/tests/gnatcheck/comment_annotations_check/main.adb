procedure Main is
   package Pkg is
      procedure Foo (A : Integer) is null;  -- FLAG

      --  1: Testing aliases (implicit_in)

      --## rule off implicit_in ## Exemption justification
      procedure Bar (A : Integer) is null;
      --## rule on implicit_in

      procedure Baz (A : Integer) is null;  -- FLAG

      --  2: Check that no justification doesn't trigger a warning in "rule
      --  off"
      --## rule off expression_functions
      function Boo return Boolean is (True);

      --  3: We check that justification is ignored in "rule on"
      --## rule on expression_functions ## useless comment
      function Moo return Boolean is (True);  -- FLAG
   end Pkg;

begin
   null;
end Main;

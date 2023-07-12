procedure Main is
   package Pkg is
      procedure Foo (A : Integer) is null;  -- FLAG

      --  1: Testing aliases (implicit_in)

      --## rule off implicit_in ## Exemption justification
      procedure Bar (A : Integer) is null;  -- FLAG in valid exemptions
      --## rule on implicit_in

      procedure Baz (A : Integer) is null;  -- FLAG

      --  2: Check that no justification doesn't trigger a warning in "rule
      --  off"
      --## rule off expression_functions
      function Boo return Boolean is (True);  -- FLAG in valid exemptions

      --  3: We check that justification is ignored in "rule on"
      --## rule on expression_functions ## useless comment
      function Moo return Boolean is (True);  -- FLAG

      --  4: Check that any extra wording is ignored and that the missing
      --  rule on is flagged
      --## rule off expression_functions this is not -- a justification -- FLAG because no matching 'exempt_OFF'
      function Loo return Boolean is (True);  -- FLAG in valid exemptions
   end Pkg;

   --  Check that a redundant rule off is detected and the alias used in the message
   --## rule off expr_func -- FLAG rule is already exempted at line 24
   procedure Poo (A : Integer) is null; --## rule line off implicit_in ## justify line off -- FLAG in valid exemptions

   --  Check that there is a warning for "rule line on"
   procedure Poo (A : Integer) is null; --## rule line on implicit_in -- FLAG (2)
begin
   null;
end Main;

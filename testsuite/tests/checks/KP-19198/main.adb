with System;
procedure Bad_Agg_Init_With_Address_Clause is
   pragma Assertion_Policy (Check);

   Buff : aliased String (1 .. 100);
   function Ident (A : System.Address) return System.Address is (A);
   Addr : constant System.Address := Ident (Buff'Address);

   function Four return Integer is (4);
   function Falz return Boolean is (False);

   type Ints is array (1 .. 5) of Integer;
   type Drec_Type (D : Boolean) is record Int : Integer; end record;

   procedure Array_With_Statically_Known_Constraint_Violation is
      X : constant Ints := (1 .. 4 => 123) with Address => Addr; --  FLAG
   begin
      pragma Assert (False);
   end Array_With_Statically_Known_Constraint_Violation;

   procedure Array_With_Statically_Unknown_Constraint_Violation is
      X : constant Ints := (1 .. Four => 123) with Address => Addr; --  FLAG
   begin
      pragma Assert (False);
   end Array_With_Statically_Unknown_Constraint_Violation;

   procedure Record_With_Statically_Known_Constraint_Violation is
      subtype False_Drec is Drec_Type (False);
      X : constant False_Drec := (True, 123) with Address => Addr; --  FLAG
   begin
      pragma Assert (False);
   end Record_With_Statically_Known_Constraint_Violation;

   procedure Record_With_Statically_Unknown_Constraint_Violation is
      subtype False_Drec is Drec_Type (Falz);
      X : constant False_Drec := (True, 123) with Address => Addr; --  FLAG
   begin
      pragma Assert (False);
   end Record_With_Statically_Unknown_Constraint_Violation;

begin
   begin
      Array_With_Statically_Known_Constraint_Violation;
   exception
      when Constraint_Error => null;
   end;
   begin
      Array_With_Statically_Unknown_Constraint_Violation;
   exception
      when Constraint_Error => null;
   end;
   begin
      Record_With_Statically_Known_Constraint_Violation;
   exception
      when Constraint_Error => null;
   end;
   begin
      Record_With_Statically_Unknown_Constraint_Violation;
   exception
      when Constraint_Error => null;
   end;
end Bad_Agg_Init_With_Address_Clause;

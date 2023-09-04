with Ada.Tags; use Ada.Tags;
with Ada.Tags.Generic_Dispatching_Constructor;
procedure Eq_Test is
   pragma Assertion_Policy (Check);

   package Pkg is
      type Root is Interface;
      -- Test passes if we instead use this definition of type Root:
      --   type Root is abstract tagged null record;

      function Convert (Ref : not null access Root'Class) return Root
        is abstract;

      type E1 is new Root with record
         F1 : Integer;
      end record;
      overriding function Convert
        (Ref : not null access Root'Class) return E1 is
        (E1 (Ref.all));  --  FLAG

      type E2 is new E1 with record
         F2 : Integer;
      end record;
      overriding function Convert
        (Ref : not null access Root'Class) return E2 is
        (E2 (Ref.all));  --  FLAG

   end Pkg;
   use Pkg;

   function Truncate is new Ada.Tags.Generic_Dispatching_Constructor
     (Root, Root'Class, Convert);

   function Prefix_Eq (Obj, Prefix : Root'Class) return Boolean is
      Obj_Copy : aliased Root'Class := Obj;
      Truncated : constant Root'Class :=
        Truncate (Prefix'Tag, Obj_Copy'Access);
   begin
      pragma Assert (Truncated'Tag = Prefix'Tag);
      return Prefix = Truncated;
   end Prefix_Eq;

begin
   for X1 in 1 .. 3 loop
      for X2 in 1 .. 3 loop
         for X3 in 1 .. 3 loop
            declare
               Prefix : constant E1      := (F1 => X1);
               Obj    : constant E2      := (F1 => X2, F2 => X3);
               Eq     : constant Boolean :=
                 Prefix_Eq (Obj => Obj, Prefix => Prefix);
            begin
               pragma Assert (Eq = (X1 = X2));
            end;
         end loop;
      end loop;
   end loop;
end Eq_Test;

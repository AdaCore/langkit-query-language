procedure DIC is
   package P1 is
      type T is private with Default_Initial_Condition => Is_Zero (T);
      function Is_Zero (X : T) return Boolean;
      function Is_Valid (X : T) return Boolean;
      function Ten return T;
      function Decr (X : T) return T with Pre => Is_Valid (X);
   private
      type T is record
         F : Integer := 0;
      end record;
      function Is_Zero (X : T) return Boolean is (X.F = 0);
      function Ten return T is ((F => 10));
      function Is_Valid (X : T) return Boolean is (X.F > 0);
      function Decr (X : T) return T is ((F => X.F - 1));
   end P1;
   use P1;
   package P2 is
      type T_Iterable is private with
        Iterable =>
          (Has_Element => Has_Element,
           Next        => Next,
           First       => First);
      function Iterate (X : T) return T_Iterable;
      function First (X : T_Iterable) return T;
      function Next (X : T_Iterable; C : T) return T;
      function Has_Element (X : T_Iterable; C : T) return Boolean;
   private
      type T_Iterable1 is tagged record
         C : T;
      end record;
      type T_Iterable is new T_Iterable1 with record
         I : Integer;
      end record;
      function Iterate (X : T) return T_Iterable is (C => X, I => 0);
      function First (X : T_Iterable) return T is (X.C);
      function Next (X : T_Iterable; C : T) return T is (Decr (C));
      function Has_Element (X : T_Iterable; C : T) return Boolean is (Is_Valid (C));
   end P2;
   use P2;
   V : T := Ten;
begin
   for U in Iterate (V) loop  --  FLAG
      if Is_Zero (V) then
         null;
      end if;
   end loop;
end DIC;

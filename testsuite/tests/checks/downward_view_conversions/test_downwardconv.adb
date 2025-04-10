procedure Main is
   package Foo is
      type T1 is tagged private;
      type T1_Access is access all T1'Class;
      procedure Proc1 (X : in out T1'Class);

      type T2 is new T1 with private;
      type T2_Access is access all T2'Class;
      procedure Proc2 (X : in out T2'Class);

   private
      type T1 is tagged record
         C : Integer := 0;
      end record;

      type T2 is new T1 with null record;
   end Foo;

   package body Foo is

      procedure Proc1 (X : in out T1'Class) is
         Var : T2 := T2 (X);                          --  FLAG
         X_Acc : T1_Access := X'Unrestricted_Access;
         Var_2 : T2_Access := T2_Access (X_Acc);      -- FLAG
      begin
         Proc2 (T2'Class (X));                        --  FLAG

         --  NOFLAG (W324-006, neither converting from or to classwide should
         --  trigger this check)
         Var := T2 (T2'Class (Var));
      end Proc1;

      procedure Proc2 (X : in out T2'Class) is
      begin
         X.C := X.C + 1;
      end Proc2;

   end Foo;

   procedure No_Flag is
      package P is
         type T is tagged private;
         subtype S is T;
         type T_Access is access all T;
         type T_Constant_Access is access constant T;

         type U is new T with private;
      private
         type T is tagged null record;
         type U is new T with null record;
      end P;

      package body P is
         function F (X : T_Access) return T_Constant_Access
         is (T_Constant_Access (X)); -- NOFLAG

         function G (X : U) return T is (T (X)); -- NOFLAG

         function I (X : T) return T is (T (X)); -- NOFLAG

         function J (X : T) return S is (S (X)); -- NOFLAG
      end P;
   begin
      null;
   end No_Flag;
begin
   null;
end Main;

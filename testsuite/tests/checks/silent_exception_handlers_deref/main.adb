procedure Main is
   --  Implicit deref case

   type Proc_Access is access procedure (I : Integer);

   procedure Test (A, B : Proc_Access) is
   begin
      --  Implicit deref cases
      begin
         null;
      exception
         when Program_Error => A (1); --  FLAG
         when others        => B (1); --  NO FLAG
      end;

      --  Explicit deref cases
      begin
         null;
      exception
         when Program_Error => A.all (1); --  FLAG
         when others        => B.all (1); --  NO FLAG
      end;
   end Test;

   package P is
      type T is tagged null record;

      procedure P (A : T);

      type T_Access is access function return T;
   end P;

   package body P is
      procedure P (A : T) is null;

      procedure Test (X, Y : T_Access) is
      begin
         --  Explicit deref in dotted call cases
         begin
            null;
         exception
            when Program_Error    => X.all.P; --  FLAG
            when others           => Y.all.P; --  NO FLAG
         end;
      end Test;
   end P;

begin
   null;
end Main;

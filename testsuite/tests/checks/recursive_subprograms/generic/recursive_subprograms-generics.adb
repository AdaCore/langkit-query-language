package body Recursive_Subprograms.Generics is

   package body Simple_Recursion_Cases is

      function Factorial (N : Natural) return Natural is
      begin
         if N = 0 then
            return 1;
         else
            return Factorial (N - 1);
         end if;
      end Factorial;

      procedure Naive_Sort (A : in out My_Arr) is
         Tmp : Integer;
      begin
         for J in A'First .. A'Last - 1 loop
            if A (J) > A (J + 1) then
               Tmp       := A (J + 1);
               A (J + 1) := A (J);
               A (J)     := Tmp;
               Naive_Sort (A);
            end if;
         end loop;

      end Naive_Sort;

      --------------------------------
      --  Simple indirect recusrion --
      --------------------------------

      function F1 (I : Integer) return Integer is
         Result : Integer := I;
      begin
         if Result < 100 then
            Result := Result + 10;
            P1 (Result);
         end if;

         return Result;
      end F1;

      procedure P1 (I : in out Integer) is
      begin
         if I < 100 then
            I := F1 (I);
         end if;
      end P1;

   end Simple_Recursion_Cases;

   package body Recursion_And_Default_Component_Initialization is

      function F1 (I : Integer) return Integer is
         Result : Integer := I;
      begin
         if Result < 100 then
            Result := F2 (Result + 1);
         end if;

         return Result;
      end F1;

      function F2 (I : Integer) return Integer is
         Result : Integer := I;
         My_Rec_Var : My_Record;   --  Implicit call to F1
      begin
         if Result < 100 then
            Result := Result + 1;
         else
            Result := Result + My_Rec_Var.I;
         end if;

         return Result;
      end F2;

      procedure P1 (I : in out Integer) is
         My_Discr_Rec_Var : My_Discr_Record;   --  Implicit call to F3
      begin
         if I < 100 then
            I := I + 1;
         else
            I := I + My_Discr_Rec_Var.D;
         end if;

      end P1;

      function F3 (I : Integer) return Integer is
         Result : Integer := I;
      begin
         P1 (Result);
         return Result;
      end F3;

   end Recursion_And_Default_Component_Initialization;

   package body Recursion_And_Default_Parameter_Initialization is

      function F1 (I : Integer) return Integer is
         Result : Integer := I;
      begin
         if Result < 100 then
            Result := F3 (Result + 1);
         end if;

         return Result;
      end F1;

      function F2 (I : Integer := F1 (1)) return Integer is
      begin
         return I;
      end;

      function F3 (I : Integer) return Integer is
         Result : Integer := I;
      begin
         if Result < 100 then
            Result := Result + 1;
         else
            Result := Result + F2;  --  Implicit call to F1
         end if;

         return Result;
      end F3;

   end Recursion_And_Default_Parameter_Initialization;

   -----------------
   -- Tricky case --
   -----------------

    generic
       with procedure P;
    procedure P2;

    procedure P2 is
    begin
       P;
    end P2;

    procedure P3 is new P2 (P1);                   --  FLAG

    procedure P1 is
    begin
       P3;
    end P1;
end Recursive_Subprograms.Generics;

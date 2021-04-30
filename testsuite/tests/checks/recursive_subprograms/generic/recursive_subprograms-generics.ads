package Recursive_Subprograms.Generics is

   -------------------------------
   -- Simple cases of recursion --
   -------------------------------

   generic
   package Simple_Recursion_Cases is
      function Factorial (N : Natural) return Natural;   --  FLAG
      --  Direct recursion in function;

      type My_Arr is array (Integer range <>) of Integer;
      procedure Naive_Sort (A : in out My_Arr);         --  FLAG
      -- Direct recursion in subprogram

      function F1 (I : Integer) return Integer;         --  FLAG
      procedure P1 (I : in out Integer);                --  FLAG
      --  Makes up indirect recursive calls
   end Simple_Recursion_Cases;

   ----------------------------------------------------------------
   -- Recursion involving default component initialization calls --
   ----------------------------------------------------------------

   generic
   package Recursion_And_Default_Component_Initialization is
      -- Defaulted record component --
      function F1 (I : Integer) return Integer;         --  FLAG
      function F2 (I : Integer) return Integer;         --  FLAG (default init)

      X : Integer := 1;

      type My_Record is record
         I : Integer := F1 (X);
      end record;

      -- Defaulted discriminant --
      procedure P1 (I : in out Integer);                --  FLAG
      function F3 (I : Integer) return Integer;         --  FLAG (default init)

      type My_Discr_Record (D : Integer := F3 (X)) is record
         I : Integer;
      end record;

   end Recursion_And_Default_Component_Initialization;

   ----------------------------------------------------------------
   -- Recursion involving default parameter initialization calls --
   ----------------------------------------------------------------

   generic
   package Recursion_And_Default_Parameter_Initialization is
      function F1 (I : Integer) return Integer;           --  FLAG
      function F2 (I : Integer := F1 (1)) return Integer; --  FLAG
      function F3 (I : Integer) return Integer;           --  FLAG
   end Recursion_And_Default_Parameter_Initialization;

   -----------------
   -- Tricky case --
   -----------------

   procedure P1;                                          --  FLAG

end Recursive_Subprograms.Generics;

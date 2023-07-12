procedure Params3 is
   package Wrapper is
      type By_Copy is private;
   private
      type By_Copy is new Character;
   end Wrapper;
   use Wrapper;
   type BC_String is array (Positive range <>) of By_Copy;

   type By_Ref is tagged null record;

   procedure Proc_CC  (X : out By_Copy;   Y : in out By_Copy) is null;
   procedure Proc_CC2 (X : in  By_Copy;   Y : out    By_Copy) is null;
   procedure Proc_CCC (X : out By_Copy;   Y : in     By_Copy; Z : in By_Copy) is null;
   procedure Proc_RRR (X : out By_Ref;    Y : in     By_Ref;  Z : in By_Ref)  is null;
   procedure Proc_CC3 (                   Y : in     By_Copy; Z : in By_Copy; X : out By_Copy) is null;
   procedure Proc_RR3 (                   Y : in     By_Ref;  Z : in By_Ref;  X : out By_Ref)  is null;
   procedure Proc_SC  (X : out BC_String; Y : out    By_Copy) is null;

   function  Func_CC  (X : out By_Copy;   Y : in out By_Copy) return Integer is (1);
   Result : Integer;

   function "+" (C : By_Copy; I : Integer) return By_Copy is (C);

begin
Simple_Cases:
   declare
      package Pack1 is
         X : By_Copy;
      end Pack1;
      package Pack2 renames Pack1;

      I,J : By_Copy;
      Alias1 : By_Copy renames I;

      R1, R2 : By_Ref;
   begin
      Proc_CC (I, I);                         -- FLAG
      Proc_CCC (I, I, I);                     -- NOFLAG (by copy type)
      Proc_RRR (R1, R1, R1);                  -- FLAG
      Proc_CC (I, J);                         -- NOFLAG
      Proc_CCC(I,  J,  J);                    -- NOFLAG
      Proc_RRR(R1, R2, R2);                   -- NOFLAG
      Proc_CCC(I,  I,  J);                    -- NOFLAG (by copy type)
      Proc_RRR(R1, R1, R2);                   -- FLAG
      Proc_CC3(    I,  J,  I);                -- NOFLAG (by copy type)
      Proc_RR3(   R1, R2, R1);                -- FLAG
      Proc_CC2(J+1, J);                       -- NOFLAG
      Proc_CC (X => I, Y => J);               -- NOFLAG
      Proc_CC (X => Simple_Cases.I, Y => I);  -- FLAG

      Proc_CC (I, By_Copy(I));                -- FLAG

      Proc_CC (I, Alias1);                    -- FLAG
      Proc_CC (Pack1.X, Pack2.X);             -- FLAG

      Result := Func_CC (I, I);                         -- FLAG
      Result := Func_CC (X => Simple_Cases.I, Y => I);  -- FLAG
      Result := Func_CC (I, J);                         -- NOFLAG
   end Simple_Cases;

Selectors:
   declare
      type Rec1 is
         record
            I, J : By_Copy;
         end record;

      type Rec2 is
         record
            I, J : By_Copy;
            K, L : Rec1;
         end record;

      procedure Proc_R1C (X : out Rec1; Y : in out By_Copy) is begin null;  end;

      procedure Proc_R2C (X : out Rec2; Y : in out By_Copy) is begin null; end;

      R1, R2 : Rec2;
      Alias2 : Rec2 renames R1;
      Alias3 : Rec1 renames Alias2.K;
   begin
      Proc_CC (R1.I, R1.I);                   -- FLAG
      Proc_CC (R1.I, R1.J);                   -- NOFLAG
      Proc_CC (R1.I, R2.I);                   -- NOFLAG

      Proc_R2C (R1,     R1.I);                -- FLAG
      Proc_R2C (R1,     R1.K.I);              -- FLAG
      Proc_R2C (R1,     R1.K.J);              -- FLAG
      Proc_R2C (Alias2, R1.K.J);              -- FLAG
      Proc_R2C (R1,     Alias2.K.J);          -- FLAG
      Proc_R2C (R1,     Alias3.J);            -- FLAG
      Proc_R2C (Alias2, Alias2.K.J);          -- FLAG

      Proc_R1C (Alias3, Alias3.J);            -- FLAG
      Proc_R1C (R1.K, R1.K.J);                -- FLAG
      Proc_R1C (R1.L, R1.K.J);                -- NOFLAG
   end Selectors;

Indexing:
   declare
      type Rec is
         record
            S : BC_String (1..10);
         end record;
      X   : Rec;
      I,J : Integer;
      Tab1 : array (1..10) of Rec;
      type Enum is (A, B);
      Tab2 : array (Enum) of By_Copy;
      E : Enum;
   begin
      Proc_SC (X.S,               X.S(I));            -- FLAG
      Proc_CC (X.S(3),            X.S(4));            -- NOFLAG (static indexing)
      Proc_CC (X.S(3),            X.S(10#3#));        -- FLAG
      Proc_CC (Tab2 (A),          Tab2 (B));          -- NOFLAG (static indexing)
      Proc_CC (Tab2 (Indexing.A), Tab2 (Indexing.B)); -- NOFLAG (static indexing)
      Proc_CC (Tab2 (A),          Tab2 (a));          -- FLAG
      Proc_SC (Tab1(I).S,         Tab1(J).S(3));      -- NOFLAG (potential aliasing)
      Proc_SC (Tab1(I).S(3..5),   Tab1(J).S(J));      -- NOFLAG (potential aliasing)
   end Indexing;

Dereferences:
   declare
      type Acc is access all BC_String (1..10);

      function F return Acc is begin return null; end;
      function G return Acc renames F;

      procedure Proc_AS (X : out Acc; Y : out BC_String) is begin null; end;

      type Rec is
         record
            S : aliased BC_String (1..10);
            A : Acc;
            B : Acc;
         end record;

      --   X : Character;
      R : Rec;
      A : Acc;
      B : Acc renames R.A;
      C : Acc := R.S'Access;
   begin
      Proc_SC (A.all,   A(3));                      -- FLAG
      Proc_SC (B.all,   R.A(3));                    -- FLAG
      Proc_SC (R.A.all, R.A(3));                    -- FLAG
   end Dereferences;

Dispatching:
   declare
      package Pack is
         type T is tagged null record;
         procedure Proc_T (A : out T; B : out T);
      end Pack;
      package body Pack is
         procedure Proc_T (A : out T; B : out T) is begin null; end;
      end Pack;
      use Pack;
      X : T;
      Y : T'Class := X;
      Z : T'Class := Y;
   begin
      Proc_T (Y, Z);
      Proc_T (Y, Y);                          -- FLAG
      Proc_T (A => Y, B => Y);                -- FLAG
   end Dispatching;

Dispatching2:
   declare
      type T is tagged record
         I : By_Copy;
      end record;
      type T_Acc is access T;
      type TC_Acc is access T'Class;
      type D is new T with null record;

      VT : T;
      VTA : T_Acc;
      VTAC : TC_Acc;

      procedure P1 (X, Y : in out By_Copy) is null;

   begin
      P1 (VT.I, VT.I);                        -- FLAG
      P1 (VTA.I, VTA.I);                      -- FLAG
      P1 (VTAC.I, VTAC.I);                    -- FLAG
   end Dispatching2;
end Params3;

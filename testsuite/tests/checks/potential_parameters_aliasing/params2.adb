procedure Params2 is
   package Wrapper is
      type By_Copy is private;
   private
      type By_Copy is new Character;
   end Wrapper;
   use Wrapper;
   type BC_String is array (Positive range <>) of By_Copy;

   procedure Proc_CC  (X : out By_Copy;   Y : in out By_Copy) is null;
   procedure Proc_SC  (X : out BC_String; Y : out    By_Copy) is null;

begin
Indexing:
   declare
      type Rec is record
         S : BC_String (1..10);
      end record;
      X   : Rec;
      I,J : Integer;
      Tab1 : array (1..10) of Rec;
   begin
      Proc_SC (Tab1(I).S,         Tab1(J).S(3));      -- FLAG (potential aliasing)
      Proc_SC (Tab1(I).S(3..5),   Tab1(J).S(J));      -- FLAG (potential aliasing)
      Proc_SC (X.S,               X.S(I));            -- NOFLAG (sure aliasing)
      Proc_CC (X.S(3),            X.S(4));            -- NOFLAG (static indexing)
   end Indexing;
end Params2;

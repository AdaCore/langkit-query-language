with Ada.Unchecked_Conversion;
procedure UC (I : in out Integer) is
   type T1 is array (1 .. 10) of Integer;
   type T2 is array (1 .. 10) of Integer;

   function UC is new Ada.Unchecked_Conversion (T1, T2);
   function Func (A : T1) return T2 with Import;

   Var1 : T1 := (others => 1);
   Var2 : T2 := (others => 2);

   procedure Init (X : out T2; Y : T2 := UC (Var1)) is   --  FLAG
   begin
      X := Y;
   end Init;

   procedure Ident (X : T2; Y : out T2) is
   begin
      Y := X;
   end Ident;

begin
   Ident (UC (Var1), Var2);                --  FLAG
   Ident (Func (Var1), Var2);              --  NO FLAG
end UC;

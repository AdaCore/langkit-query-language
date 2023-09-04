procedure Params2 is

   type T is record
      b : Boolean := False;
      i : Integer := 0;
      S : String (1..100) := (others => ' ');
   end record;

   Var : T;

   procedure Set (In_Record : in T; Out_Record : out T) is
   begin
      Out_Record.B := False;
   end Set;

begin
   Set (Var, Var); --  FLAG
end Param2;

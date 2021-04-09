package body Prot is
   protected body P is
      entry E (I : in out Integer) when Flag is
      begin
         I   := Var + I;
         Var := I;           --  FLAG
      end E;

      procedure P (I : Integer) is
      begin
         Flag := I > 0;
      end P;
   end P;
end Prot;

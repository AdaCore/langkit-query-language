package Default_Parameters is
   procedure P (I : in out Integer; J : Integer := 0);
   procedure Q (I : in out Integer; J : Integer);
   procedure R (I, J : Integer := 0; K : Integer := 0); --  FLAG
end;

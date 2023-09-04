package Implicit is
   procedure Proc1 (I :    Integer);          --  FLAG
   procedure Proc2 (I : in Integer);          --  NOFLAG
   procedure Proc3 (I :    access Integer);   --  NOFLAG
end Implicit;

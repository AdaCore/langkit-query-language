package Implicit is
   procedure Proc1 (I :    Integer);          --  FLAG
   procedure Proc2 (I : in Integer);          --  NO FLAG
   procedure Proc3 (I :    access Integer);   --  NO FLAG
end Implicit;

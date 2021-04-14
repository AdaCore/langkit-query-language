package Raise_Statements is
   Global_Ex : exception;

   procedure Proc_1;
   procedure Proc_2;

   generic function Fun (I : Integer) return Integer;

   task T is
      entry E;
   end T;

end Raise_Statements;

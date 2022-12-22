package Names is       --  FLAG
   procedure Proc;

   package Nested is   --  FLAG
      --
      --
   end;

   package Nested2 is   --  NO FLAG
      --
      --
   end Nested2 ;

   task type Task1;     --  NO FLAG

   task Task2 is        --  FLAG
      --
      --
   end;

   protected Prot1 is       --  FLAG
      --
      procedure P;
   end;

   protected type Prot2 is  --  NO FLAG
   end Prot2;

end;

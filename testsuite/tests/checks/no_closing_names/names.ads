package Names is       --  FLAG
   procedure Proc;

   package Nested is   --  FLAG
      --
      --
   end;

   package Nested2 is   -- NOFLAG
      --
      --
   end Nested2 ;

   task type Task1;     -- NOFLAG

   task Task2 is        --  FLAG
      --
      --
   end;

   protected Prot1 is       --  FLAG
      --
      procedure P;
   end;

   protected type Prot2 is  -- NOFLAG
   end Prot2;

end;

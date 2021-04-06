package Decls is

   protected P0 is
      entry Get (I: Integer);       --  NO FLAG
      entry Put (I: out Integer);   --  FLAG
      entry Put2;                   --  FLAG
      procedure Reset;
      function Check return Boolean;
   private
      Val : Integer := 0;
   end P0;

   protected P1 is
      entry E;  --  NO FLAG
   end P1;

end Decls;  

package Test is
   Exception_0 : exception;
   package Pack is
      Exception_1 : exception;
      procedure Proc_1;
   end Pack;
   generic
   package Pack_G is
      Exception_G1 : exception;
      procedure Proc_G1;
   end Pack_G;
end Test;

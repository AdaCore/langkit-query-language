package body Test is
   package body Pack is
      Exception_2 : exception;
      Exception_3 : exception;
      procedure Proc_0 is
      begin
         raise Exception_0;
      end Proc_0;
      procedure Proc_1 is
      begin
         raise Exception_1;
      end Proc_1;
      procedure Proc_2 is
      begin
         raise Exception_2; -- FLAG
      end Proc_2;
      procedure Proc_3 is
      begin
         raise Exception_3;
      exception
         when Exception_3 =>
            null;
      end Proc_3;
      procedure Proc_4 is
      begin
         raise Exception_3;
      exception
         when others =>
            null;
      end Proc_4;
   end Pack;

   package body Pack_G is
      Exception_G2 : exception;
      Exception_G3 : exception;
      Exception_G4 : exception;
      procedure Proc_G0 is
      begin
         raise Exception_0;
      end Proc_G0;
      procedure Proc_G1 is
      begin
         raise Exception_G1;
      end Proc_G1;
      procedure Proc_G2 is
      begin
         raise Exception_G2; -- FLAG
      end Proc_G2;
      procedure Proc_G3 is
      begin
         raise Exception_G4;
      exception
         when Exception_G3 | Exception_G4 =>
            null;
      end Proc_G3;
   end Pack_G;
end Test;

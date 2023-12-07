package Main is
   type Int_Access is access Integer;    --  FLAG (2)
   type Char_Access is access Character; --  FLAG (2)
   type Valid_T is access String;

   My_Int  : constant Integer   := 0;    --  FLAG (2)
   My_Char : constant Character := '0';  --  FLAG (2)
   Valid_C : constant String    := "Valid!";
end Main;

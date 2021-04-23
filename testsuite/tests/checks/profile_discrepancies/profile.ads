package Profile is

   procedure Proc1
     (I : Integer;
      J : Integer);

   procedure Proc2
     (I, J : Integer);

   function F1 return Integer;

   generic
   function F1_G return access procedure (I, J: Integer);

   procedure Proc3 (I : Integer);

   procedure Proc4 (I : Integer);

   function F2 return Integer;

end Profile;

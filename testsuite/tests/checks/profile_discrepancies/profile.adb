package body Profile is

   subtype My_Int is Integer;

   procedure Proc1                                     --  FLAG
     (I : Integer;
      J : My_Int) is begin null; end;

   procedure Proc2                                     --  FLAG
     (I : Integer;
      J : Integer) renames Proc1;

   function F1 return My_Int is (1);                   --  FLAG

   function F1_G return                                --  FLAG
     access procedure (I : Integer; J: Integer) is
   begin
      return Proc1'Access;
   end F1_G;

   procedure Proc3 (I : Standard.Integer) is null;     --  FLAG

   procedure Proc4 (I : in Integer) is null;           --  FLAG

   function F2 return Standard.Integer is              --  FLAG
   begin
      return 1;
   end F2;

   protected T is
      entry E (X : Integer);
   end T;

   protected body T is
      entry E (X : in Integer) when True is            --  FLAG
      begin
         null;
      end E;
   end T;

end Profile;

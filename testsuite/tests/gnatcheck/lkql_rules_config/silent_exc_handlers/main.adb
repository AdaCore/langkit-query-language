procedure Main is
   procedure Raiser is null;

   package Inner is
      procedure Exempted is null;
      procedure Other is null;
   end Inner;

   procedure Test_1 is
   begin
      null;
   exception
      when others =>  --  NOFLAG
         Raiser;
   end Test;

   procedure Test_2 is
   begin
      null;
   exception
      when others =>  --  NOFLAG
         Inner.Exempted;
   end Test_2;

   procedure Test_3 is
   begin
      null;
   exception
      when others =>  --  FLAG
         Inner.Other;
   end Test_3;

begin
   null;
exception
   when other =>  --  FLAG
      null;
end Main;

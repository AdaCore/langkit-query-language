package body Exception_Declarations is

   procedure Proc_1 is
      Ex_1 : exception;                         --  FLAG
      Ex_2 : exception;                         --  NOFLAG
      Ex_3 : exception;                         --  NOFLAG

      Ex_3_R : exception renames Ex_3;

   begin

      declare
         Ex_4 : exception;                      --  NOFLAG
      begin
         null;
      exception
         when others =>
            null;
      end;

      declare
         Ex_5 : exception;                      --  FLAG
      begin
         null;
      end;

   exception
      when Ex_2 =>
         null;
      when Ex_3_R =>
         null;
   end Proc_1;

   procedure Proc_2 is
      Ex : exception;                           --  FLAG
   begin
      null;
   end Proc_2;

   procedure Proc_3 is
      Ex : exception;                           --  NOFLAG
   begin
      null;
   exception
      when others =>
         null;
   end Proc_3;

   function Fun (I : Integer) return Integer is
      Ex_1 : exception;                         --  FLAG
      Ex_2 : exception;                         --  NOFLAG
   begin
      return 1;
   exception
      when Ex_2 =>
         return 2;
   end Fun;

   task body T is
      Ex : exception;                           --  FLAG
   begin
      accept E;
   end T;

end Exception_Declarations;

package body Raise_Statements is

   procedure Proc_1 is
      Ex_1 : exception;                         --  NO FLAG
      Ex_2 : exception;                         --  NO FLAG
      Ex_3 : exception;                         --  NO FLAG

      Ex_3_R : exception renames Ex_3;

   begin

      declare
         Ex_4 : exception;                      --  NO FLAG
      begin
         null;
      exception
         when Ex_4 =>
            raise;                              --  FLAG
         when Ex_1 =>
            raise Ex_2;                         --  NO FLAG
         when others =>
            raise Global_Ex;
      end;

      declare
         Ex_5 : exception;                      --  NO FLAG
      begin
         null;
      exception
         when others =>
            raise;                              --  NO FLAG
      end;

   exception
      when Ex_1 =>
         raise Global_Ex;                       --  NO FLAG
      when Ex_2 =>
         raise;                                 --  FLAG
      when Ex_3_R =>
         raise Ex_1;                            --  FLAG
   end Proc_1;

   procedure Proc_2 is
      Ex : exception;                           --  NO FLAG
   begin
      null;
   exception
      when Global_Ex =>
         raise Ex;                              --  FLAG
      when Ex =>
         raise Global_Ex;                       --  NO FLAG
   end Proc_2;

   function Fun (I : Integer) return Integer is
      Ex_1 : exception;                         --  NO FLAG
      Ex_2 : exception;                         --  NO FLAG
   begin
      return 1;
   exception
      when Ex_1 =>
         raise;                                 --  FLAG
      when Ex_2 =>
         return 2;
   end Fun;

   task body T is
      Ex : exception;                           --  NO FLAG
   begin
      accept E do
         null;
      exception
         when others =>
            raise Ex;                           --  NO FLAG
      end E;
   exception
      when others =>
         raise Global_Ex;                       --  NO FLAG
   end T;

end Raise_Statements;

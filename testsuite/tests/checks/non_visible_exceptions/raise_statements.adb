package body Raise_Statements is

   procedure Proc_1 is
      Ex_1 : exception;                         -- NOFLAG
      Ex_2 : exception;                         -- NOFLAG
      Ex_3 : exception;                         -- NOFLAG

      Ex_3_R : exception renames Ex_3;

   begin

      declare
         Ex_4 : exception;                      -- NOFLAG
      begin
         null;
      exception
         when Ex_4 =>
            raise;                              --  FLAG
         when Ex_1 =>
            raise Ex_2;                         -- NOFLAG
         when others =>
            raise Global_Ex;
      end;

      declare
         Ex_5 : exception;                      -- NOFLAG
      begin
         null;
      exception
         when others =>
            raise;                              -- NOFLAG
      end;

   exception
      when Ex_1 =>
         raise Global_Ex;                       -- NOFLAG
      when Ex_2 =>
         raise;                                 --  FLAG
      when Ex_3_R =>
         raise Ex_1;                            --  FLAG
   end Proc_1;

   procedure Proc_2 is
      Ex : exception;                           -- NOFLAG
   begin
      null;
   exception
      when Global_Ex =>
         raise Ex;                              --  FLAG
      when Ex =>
         raise Global_Ex;                       -- NOFLAG
   end Proc_2;

   function Fun (I : Integer) return Integer is
      Ex_1 : exception;                         -- NOFLAG
      Ex_2 : exception;                         -- NOFLAG
   begin
      return 1;
   exception
      when Ex_1 =>
         raise;                                 --  FLAG
      when Ex_2 =>
         return 2;
   end Fun;

   task body T is
      Ex : exception;                           -- NOFLAG
   begin
      accept E do
         null;
      exception
         when others =>
            raise Ex;                           -- NOFLAG
      end E;
   exception
      when others =>
         raise Global_Ex;                       -- NOFLAG
   end T;

end Raise_Statements;

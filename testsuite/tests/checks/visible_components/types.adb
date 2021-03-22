package body Types is

   procedure P is
      package P1 is
         type T is tagged record   --  NO FLAG
            X : Integer;
         end record;
      end P1;
   begin
      null;
   end P;

end Types;

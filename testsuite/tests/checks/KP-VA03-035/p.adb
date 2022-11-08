package body P is

   Global : Integer;

   protected body Imp is
      procedure Get (Index : out Integer) is  --  FLAG
      begin
         Index := P.Global;
      end Get;
   end Imp;

end P;

procedure Names is  --  FLAG
   task T3;         --  NOFLAG

   task body T3 is  --  NOFLAG
   begin
      null;
   end T3;
end;

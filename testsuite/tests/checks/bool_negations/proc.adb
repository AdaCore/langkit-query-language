procedure Proc (Buffer_Length : Integer) is
   Is_Data_Available : Boolean := Buffer_Length > 0;   -- NOFLAG
begin
   Is_Data_Available := not (Buffer_Length = 0);   --  FLAG

   if not (not Is_Data_Available) then             --  FLAG
      null;
   end if;
end Proc;

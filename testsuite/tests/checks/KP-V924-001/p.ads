package P is

   protected Imp with Lock_Free is   --  FLAG
      procedure Get (Index : out Integer);
   private
      Next_Index : Integer := 1;
   end Imp;

end P;

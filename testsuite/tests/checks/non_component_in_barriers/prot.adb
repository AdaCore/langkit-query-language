procedure Prot is

   protected Obj is
      entry Get (V : out Integer);
      entry Get2;
      entry Get_KO;
   private
      Local  : Integer;
      Is_Set : Boolean := False;
   end Obj;

   Global_Bool : Boolean := False;

   protected body Obj is
      entry Get (V : out Integer)
        when Is_Set is             -- NOFLAG
      begin
         V := Local;
         Is_Set := False;
      end Get;

      entry Get2
        when True is               -- NOFLAG
      begin
         Is_Set := False;
      end Get2;

      entry Get_KO
        when Global_Bool is        --  FLAG
      begin
         Global_Bool := False;
      end Get_KO;
   end Obj;

begin
   null;
end Prot;

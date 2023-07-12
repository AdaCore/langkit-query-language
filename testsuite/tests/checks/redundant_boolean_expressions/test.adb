function Test (Cond : Boolean; J : Integer) return Boolean is
   X : Boolean;
begin
   if Cond then                --  FLAG
      X := False;
   else
      X := True;
   end if;

   if Cond then                --  FLAG
      return False;
   else
      return True;
   end if;

   X := X = Standard.true;     --  FLAG

   if not (J > 1) then         --  FLAG
      null;
   end if;

   declare
      type Bool_N is new Boolean;
      Y : Bool_N;
   begin
      if Y = True then    --  FLAG
         J := 1;
      end if;
   end;

   return X = Standard.False;  --  FLAG
   return False;               -- NOFLAG
end Test;

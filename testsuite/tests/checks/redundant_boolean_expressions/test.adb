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

   return X = Standard.False;  --  FLAG
   return False;               --  NO FLAG
end Test;

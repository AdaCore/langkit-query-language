package body P is

   function P1 (X : Rec) return Boolean is
   begin
      return X = None;   --  FLAG
      return None /= X;  --  FLAG
      return X = X;      --  NO FLAG
   end P1;

end P;

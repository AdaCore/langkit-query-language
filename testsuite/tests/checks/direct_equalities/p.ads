package P is
   type Rec is record
      F1 : Integer;
   end record;

   None : constant Rec := (F1 => 0);

   function P1 (X : Rec) return Boolean;
end P;

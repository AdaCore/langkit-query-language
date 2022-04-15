function P (Param : access Integer) return Boolean is
   type Typ is access all Integer;

   function A_Inner (Param : access Integer) return Typ is
   begin
      return Typ (Param);
   end;

begin
   return A_Inner (Param) = Typ (Param);  --  FLAG
end;

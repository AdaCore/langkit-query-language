with Pack1;
with Pack2;
procedure Proc is
   use Pack1;               --  FLAG

   procedure Inner is
      use type Pack2.T;     --  FLAG (if Except_USE_TYPE_Clauses is not set)
   begin
      null;
   end Inner;
begin
   null;
end Proc;

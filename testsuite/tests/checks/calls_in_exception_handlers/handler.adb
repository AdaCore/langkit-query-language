procedure Handler is
   procedure Safe_P with Import;
   procedure Unsafe_P with Import;
begin
   null;
exception
   when Constraint_Error =>   --  NO FLAG
      Safe_P;
   when Program_Error =>      --  FLAG
      Safe_P;
      Unsafe_P;
   when others =>             --  FLAG
      Unsafe_P;
end Handler;

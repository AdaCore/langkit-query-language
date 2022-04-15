package body Main_Pkg is
   
   Singleton : aliased Object;
   
   function Instance
     return Object_Access
   is
   begin
      return Singleton'Access;
   end Instance;

   procedure Start
     (This : not null access Object)
   is
   begin
      This.M_Impl_B.Initialize
        (A   => This.M_Impl_A'Access,   --  FLAG
         B   => This.M_Impl_A'Access);  --  FLAG
      This.M_Impl_B.Run;
   end Start;

end Main_Pkg;

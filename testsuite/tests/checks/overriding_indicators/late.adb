package body Late is
   function Prim (Arg: T_Ancestor) return Float is
   begin
      return 0.1;
   end Prim;

   function Prim (Arg: T_Descendant) return Float is  -- FLAG
   begin
      return 0.2;
   end Prim;


   function Late_Prim (Arg: T_Ancestor) return Float is
   begin
      return 0.1;
   end Late_Prim;

   function Late_Prim (Arg: T_Descendant) return Float is  -- NOFLAG
   begin
      return 0.2;
   end Late_Prim;
end Late;

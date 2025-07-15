package Late is
   type T_Ancestor is null record;

   function Prim (Arg: T_Ancestor) return Float;

   type T_Descendant is new T_Ancestor;

   -- "Late" primitive of T_Ancestor. Can't be inherited by types declared
   -- before in the source Code.
   function Late_Prim (Arg: T_Ancestor) return Float;

   -- The following operation cannot override the primive of T_Ancestor because
   -- the latter is not inherited by T_Descendant since the primitive of
   -- T_Ancestor has been declared *after* the declaration of T_Descendant.
   function Late_Prim (Arg: T_Descendant) return Float; -- NOFLAG

   -- The following operation override the primitive of T_Ancestor
   function Prim (Arg: T_Descendant) return Float; -- FLAG
end Late;

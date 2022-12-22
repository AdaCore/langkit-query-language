package Renamings is
   I : Integer;
   J : Integer renames I;     --  NO FLAG

   function Foo (I, J : Integer) return Integer;

   function "+" (I, J : Integer)           --  NO FLAG
     return Integer renames Standard."+";
   function "+" (I, J : Integer)           --  NO FLAG
     return Integer renames Foo;

   function "+" (I, J : Integer)           --  FLAG
     return Integer renames Standard."-";

end Renamings;

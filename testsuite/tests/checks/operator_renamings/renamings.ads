package Renamings is
   I : Integer;
   J : Integer renames I;     -- NOFLAG

   function Foo (I, J : Integer) return Integer;

   function "+" (I, J : Integer)           --  FLAG
     return Integer renames Standard."+";
   function "+" (I, J : Integer)           -- NOFLAG
     return Integer renames Foo;

end Renamings;

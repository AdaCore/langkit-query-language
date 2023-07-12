package Decls is
   type Valid is digits 8 range -1.0 .. 1.0;     

   type Invalid1 is digits 8;                           --  FLAG
   type Invalid2 is delta 0.01 digits 8;                --  FLAG
   
   subtype InvalidSub is Invalid2;                      -- NOFLAG (subtype)
end Decls;

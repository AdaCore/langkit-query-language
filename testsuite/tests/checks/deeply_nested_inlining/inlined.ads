package Inlined is

   procedure P0;                --  NOFLAG
   pragma Inline (P0);

   procedure P1;                --  NOFLAG
   pragma Inline (P1);

   procedure P2;                --  NOFLAG
   pragma Inline (P2);

   function F0 return Boolean;  --  NOFLAG
   pragma Inline (F0);

   function F1 return Boolean;  --  NOFLAG
   pragma Inline (F1);

   function F2 return Boolean;  --  NOFLAG
   pragma Inline (F2);

end Inlined;

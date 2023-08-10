package Deeply_Inlined is

   procedure P3;  --  NOFLAG
   pragma Inline (P3);

   procedure P5;  --  FLAG
   pragma Inline (P5);

   procedure P6;  --  FLAG
   pragma Inline (P6);

   procedure P7;  --  FLAG
   pragma Inline (P7);

   function F3 return Boolean;  --  NOFLAG
   pragma Inline (F3);

   function F4 return Boolean;  --  FLAG
   pragma Inline (F4);

   function F6 return Boolean;  --  FLAG
   pragma Inline (F6);

   --  F7 is not an inlined subprogram
   function F7 return Boolean;

   function F8 return Boolean;  --  NOFLAG
   pragma Inline (F8);

   function F9 return Boolean   --  FLAG
   with Inline;

   function F10 return Boolean  --  FLAG
   with Inline;

end Deeply_Inlined;

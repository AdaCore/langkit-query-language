package Inlined is

   procedure P0;                -- NO FLAG
   pragma Inline (P0);

   procedure P1;                -- NO FLAG
   pragma Inline (P1);

   procedure P2;                -- NO FLAG
   pragma Inline (P2);

   function F0 return Boolean;  -- NO FLAG
   pragma Inline (F0);

   function F1 return Boolean;  -- NO FLAG
   pragma Inline (F1);

   function F2 return Boolean;  -- NO FLAG
   pragma Inline (F2);

end Inlined;

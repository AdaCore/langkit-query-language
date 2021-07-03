package Complex is

   procedure Swap (I, J : in out Integer);   --  FLAG
   pragma Inline (Swap);

end Complex;

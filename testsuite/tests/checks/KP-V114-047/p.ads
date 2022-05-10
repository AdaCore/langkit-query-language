package P is
   protected Handler is   --  FLAG
      pragma Interrupt_Priority (1);

      procedure Handle_Interrupt;
      pragma Attach_Handler (Handle_Interrupt, 1);
   end Handler;
end P;

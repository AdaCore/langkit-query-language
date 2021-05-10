with Ada.Interrupts;
package Interrupts is

   function Get_Interrupt return Ada.Interrupts.Interrupt_ID with Import;

   protected type Pr_T is
      procedure Handler1_Hdl with Interrupt_Handler;
      procedure Handler1 with Interrupt_Handler;                --  FLAG

      procedure Handler2;                                       --  FLAG
      pragma Interrupt_Handler (Handler2);

      procedure Handler2_Hdl;
      pragma Interrupt_Handler (Handler2_Hdl);

      procedure Handler4;

      procedure Handler5_Hdl with Attach_Handler => Get_Interrupt;
      procedure Handler5 with Attach_Handler => Get_Interrupt;  --  FLAG

      procedure Handler6;                                       --  FLAG
      pragma Attach_Handler (Handler6, Get_Interrupt);

      procedure Handler6_Hdl;
      pragma Attach_Handler (Handler6_Hdl, Get_Interrupt);

   private
      B : Boolean;
   end Pr_T;
end Interrupts;

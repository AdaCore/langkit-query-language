with Ada.Exceptions; use Ada.Exceptions;

package body P is

   procedure P1 is  --  FLAG
   begin
      raise Constraint_Error;
   exception
      when others =>
         Raise_Exception (Constraint_Error'Identity);
   end P1;

   procedure P2 with Export is   --  FLAG
   begin
      Raise_Exception (Constraint_Error'Identity);
   exception
      when E: others =>
         Reraise_Occurrence (E);
   end P2;

   procedure P3 is   --  NO FLAG (no export)
   begin
      raise Constraint_Error;
   end P2;

end P;

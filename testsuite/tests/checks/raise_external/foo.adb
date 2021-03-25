with Exception_Declarations;
package body Foo is
   procedure Proc (I : in out Integer) is
   begin
      if I < 0 then
         raise Exception_Declarations.Ex;   --  FLAG
      else
         I := I - 1;
         raise Local_Exc;      --  NO FLAG
      end if;

      raise Constraint_Error;  --  NO FLAG
   end Proc;
end Foo;

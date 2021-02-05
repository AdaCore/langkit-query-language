package body Abort_Statements is

   task T;
   task body T is
   begin null;
   end T;

   procedure Do_Abort is
   begin
      abort T; -- FLAG
   end Do_Abort;

end Abort_Statements;

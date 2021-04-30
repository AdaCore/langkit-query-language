package body Root.Child_1 is

   procedure Process_1 (X : in out PT_1; I : Integer) is
   begin
      if I > 1 then
         Process_1 (PT_1'Class (X), I); --  Dispatching call
      end if;
   end Process_1;

   procedure Process_2 (X : in out PT_1; I : Integer) is
   begin
      Process_All (X, I); -- Next step in call chain - dispatching calls
   end Process_2;

end Root.Child_1;

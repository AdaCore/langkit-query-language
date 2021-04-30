package body Root.Child_2 is

   procedure Process_1 (X : in out PT_2; I : Integer) is
   begin
      if I > 1 then
         Process_2 (PT_2'Class (X), I); --  Dispatching call
      end if;
   end Process_1;

   procedure Process_2 (X : in out PT_2; I : Integer) is
   begin
      Process_1 (X, I);
   end Process_2;

end Root.Child_2;

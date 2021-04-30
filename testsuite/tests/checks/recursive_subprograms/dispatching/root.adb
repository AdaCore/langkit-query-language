package body Root is

   procedure Process_All (X : in out PT'Class; I : Integer) is
   begin
      Process_1 (X, I);              --  Dispatching call
      Process_2 (X, I);              --  Dispatching call
   end Process_All;

end Root;

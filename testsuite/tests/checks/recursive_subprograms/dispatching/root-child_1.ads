package Root.Child_1 is

   type PT_1 is new PT with private;
   procedure Process_1 (X : in out PT_1; I : Integer);   --  FLAG if Follow_Dispatching_Calls
   procedure Process_2 (X : in out PT_1; I : Integer);    --  FLAG if Follow_Dispatching_Calls

private

   type PT_1 is new PT with null record;

end Root.Child_1;

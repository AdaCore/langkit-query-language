package Root.Child_2 is

   type PT_2 is new PT with private;
   procedure Process_1 (X : in out PT_2; I : Integer);  --  FLAG if Follow_Dispatching_Calls
   procedure Process_2 (X : in out PT_2; I : Integer);  --  FLAG if Follow_Dispatching_Calls

private

   type PT_2 is new PT with null record;

end Root.Child_2;

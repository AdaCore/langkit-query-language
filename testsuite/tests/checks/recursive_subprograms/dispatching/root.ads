package Root is

   type PT is tagged private;

   procedure Process_1 (X : in out PT; I : Integer) is null;
   procedure Process_2 (X : in out PT; I : Integer) is null;

   procedure Process_All (X : in out PT'Class; I : Integer); --  FLAG if Skip_Dispatching_Calls not set

private

   type PT is tagged null record;

end Root;

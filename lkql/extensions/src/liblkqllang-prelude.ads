with Liblkqllang.Implementation; use Liblkqllang.Implementation;

private package Liblkqllang.Prelude is

   Prelude_Unit : Internal_Unit :=
     No_Analysis_Unit;
   --  Analysis_Unit containing the Prelude's declarations

   procedure Fetch_Prelude (Context : Internal_Context);
   --  Load symbols from LKQL's "standard library".

end Liblkqllang.Prelude;

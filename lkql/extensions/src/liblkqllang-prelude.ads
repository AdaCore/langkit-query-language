with Liblkqllang.Analysis;
with Liblkqllang.Implementation; use Liblkqllang.Implementation;

private package Liblkqllang.Prelude is

    procedure Fetch_Prelude (Context : Internal_Context);
    --  Load symbols from LKQL's "standard library".

end Liblkqllang.Prelude;
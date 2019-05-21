with Langkit_Support.Text; use Langkit_Support.Text;

with Liblkqllang.Converters; use Liblkqllang.Converters;

package body Liblkqllang.Prelude is

    Prelude_Content : String :=
        "selector children" & ASCII.LF &
        "   | AdaNode => rec *it.children" & ASCII.LF &
        "   | _        => ()";   

    -------------------
    -- Fetch_Prelude --
    -------------------

    procedure Fetch_Prelude (Context : Internal_Context) is
       use Liblkqllang.Analysis;
       Std : constant Analysis_Unit :=
          Get_From_Buffer (Wrap_Context (Context), "prelude", "ascii", Prelude_Content);
    begin
       Populate_Lexical_Env (Std);
    end Fetch_Prelude;

end Liblkqllang.Prelude;
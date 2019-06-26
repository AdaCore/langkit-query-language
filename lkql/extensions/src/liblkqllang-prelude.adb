with Langkit_Support.Text; use Langkit_Support.Text;

with Liblkqllang.Common;     use Liblkqllang.Common;
with Liblkqllang.Converters; use Liblkqllang.Converters;
with Liblkqllang.Implementation; use Liblkqllang.Implementation;

package body Liblkqllang.Prelude is

    Prelude_Content : String :=
        "BUILTIN_DECL int;"    & ASCII.LF &
        "BUILTIN_DECL string;" & ASCII.LF & 
        "BUILTIN_DECL bool;"   & ASCII.LF &
        "BUILTIN_DECL unit;"   & ASCII.LF &  
        "BUILTIN_DECL error;"   & ASCII.LF & 
        "selector nextSiblings" & ASCII.LF &
        "   | AdaNode => rec it.next_sibling" & ASCII.LF &
        "   | _       => ()" & ASCII.LF &
        ""                   & ASCII.LF &
        "selector prevSiblings" & ASCII.LF &
        "   | AdaNode => rec it.previous_sibling" & ASCII.LF &
        "   | _       => ()" & ASCII.LF &
        ""                   & ASCII.LF &
        "selector parent" & ASCII.LF &
        "   | AdaNode => rec *it.parent" & ASCII.LF &
        "   | _       => ()" & ASCII.LF &
        ""                   & ASCII.LF &
        "selector children" & ASCII.LF &
        "   | AdaNode => rec *it.children" & ASCII.LF &
        "   | _       => ()";   

    -------------------
    -- Fetch_Prelude --
    -------------------

    procedure Fetch_Prelude (Context : Internal_Context) is
       use Liblkqllang.Analysis;
       Std : constant Internal_Unit :=
          Get_From_Buffer 
                   (Context  => Context, 
                    Filename => "prelude", 
                    Charset  => "ascii", 
                    Buffer   => Prelude_Content,
                    Rule     => Default_Grammar_Rule);
    begin
       Prelude_Unit := Std;
       Populate_Lexical_Env (Std);
    end Fetch_Prelude;

end Liblkqllang.Prelude;
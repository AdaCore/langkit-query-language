package body Liblkqllang.Prelude is

   Prelude_Content : constant String :=
        "selector next_siblings" & ASCII.LF &
        "   | AdaNode => rec it.next_sibling" & ASCII.LF &
        "   | *       => ()" & ASCII.LF &
        ""                   & ASCII.LF &
        "selector prev_siblings" & ASCII.LF &
        "   | AdaNode => rec it.previous_sibling" & ASCII.LF &
        "   | *       => ()" & ASCII.LF &
        ""                   & ASCII.LF &
        "selector parent" & ASCII.LF &
        "   | AdaNode => rec *it.parent" & ASCII.LF &
        "   | *       => ()" & ASCII.LF &
        ""                   & ASCII.LF &
        "selector children" & ASCII.LF &
        "   | AdaNode => rec *it.children" & ASCII.LF &
        "   | *       => ()" & ASCII.LF &
        ""                   & ASCII.LF &
        "selector super_types" & ASCII.LF &
        "    | BaseTypeDecl      => rec *it.p_base_types()" & ASCII.LF &
        "    | *                 => ()" & ASCII.LF;

   ------------------
   -- Prelude_Unit --
   ------------------

   function Prelude_Unit (Context : Analysis_Context) return Analysis_Unit is
   begin
      return Context.Get_From_Buffer
        (Filename => "prelude", Buffer => Prelude_Content);
   end Prelude_Unit;

end Liblkqllang.Prelude;

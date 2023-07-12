--  One of the restrictions in the Safety Base and Safety Extended
--  capability sets for Ada 2012 is the following:
--
--     * The only permitted syntax for a formal_package_actual_part in a
--       formal_package_declaration (Ada 2012 LRM §12.7) is:
--
--       formal_package_actual_part ::= (<>) | [generic_actual_part]
--
--    For reference, the (Ada 2012) production including formal_package_actual_part is:
--
--       formal_package_declaration ::=
--           with package defining_identifier is new generic_package_name
--           formal_package_actual_part
--           [aspect_specification];
--
--       formal_package_actual_part ::=
--           ([others =>] <>)
--         | [generic_actual_part]
--         | (formal_package_association {, formal_package_association} [, others => <>])
--
--  So the FACE rule removes "others" from the first alternative for
--  formal_package_actual_part and omits the entire third alternative.
--
--  To complete the remaining productions:
--
--       generic_actual_part ::=
--           (generic_association {, generic_association})
--
--       generic_association ::=
--          [generic_formal_parameter_selector_name =>]
--          explicit_generic_actual_parameter
--
--  In all Ada syntax productions, the symbol "<>" is referred to as the
--  "box", and can be thought as indicating something unspecified at that place
--  in the production, essentially TBD ("To Be Determined later").
--
--  This FACE restriction corresponds to the Ada 95 facilities for specifying
--  generic formal packages. It removes the Ada 2005 syntax extensions, thus
--  restricting where the box can be used and removing the reserved word
--  "others".
--
--  As a result, in a formal_package_actual_part, either all generic actual
--  parameters are given explicit values, or they all get the box. The Ada
--  95 syntax does not allow combinations, in which some generic actuals are
--  the box and some are not. Said another way, the box can be specified for all
--  generic actual parameters, or none.
--
--  In order to specify the box for all the generic actuals, the "(<>)" syntax
--  of the first modified alternative must be used, exactly. That is the only
--  syntax allowed. The reserved word "others" is not included in the first
--  alternative under this FACE rule.
--
--  Furthermore, it is not possible to associate each formal with the box
--  individually.

--  Finally, the reserved word "others" cannot be used under this rule to
--  specify the box for any generic actual package parameter.
--
--  In short, when specifying the generic actuals for a generic formal
--  package's formal parameters, either all actuals are explicitly specified,
--  or all actuals are the box. Furthermore, the "others" reserved word cannot
--  be used to specify that any of the generic actuals is the box.

procedure Formal is
begin
   --------------------  First, examples of allowed usage --------------------

   --  Use (modified) first alternative of the formal_package_actual_part
   --  production to specify that all actuals are the box (without the
   --  "others")
   Allowed_1 : declare

      generic
         type Component is private;
         type Index is (<>);
         type List is array (Index range <>) of Component;
      package P is
      end P;

      generic
         with package NP is new P (<>);  -- NOFLAG
      package R is
      end R;

   begin
      null;
   end Allowed_1;

   --  Normal generic_association, using directly visible language-defined type
   Allowed_2 : declare

      generic
         type T is private;
      package P is
      end P;

      generic
         with package NP is new P (T => Integer);  -- NOFLAG
      package R is
      end R;

   begin
      null;
   end Allowed_2;

   --  Normal generic_association, using previous generic formal type
   Allowed_3 : declare

      generic
         type T is private;
      package P is
      end P;

      generic
         type Item is private;
         with package NP is new P (T => Item);  -- NOFLAG
      package R is
      end R;

   begin
      null;
   end Allowed_3;

   --  Normal generic_association, all explicity specified, none as the box,
   --  using previous generic formal types
   Allowed_4: declare

      generic
         type Component is private;
         type Index is (<>);
         type List is array (Index range <>) of Component;
      package P is
      end P;

      generic
         type Item is private;
         type Items is array (Positive range <>) of Item;
         with package NP is new P
                 (Component => Item, Index => Positive, List => Items);  -- NOFLAG
         --  FACE 3.1 allows the above in Safety Extended and Safety Base
         --  (and Security) because the generic formal package parameter
         --  associations are all specified explicitly, none are the box.
      package R is
      end R;

   begin
      null;
   end Allowed_4;

   -----------------------------  error cases  -------------------------------

   --  Attempt to use removed formal_package_association production (second
   --  alternative) to specify that all actuals are the box
   --
   --       formal_package_association ::=
   --           generic_association
   --         | generic_formal_parameter_selector_name => <>
   --
   Fail_1 : declare

      generic
         type T is private;
      package P is
      end P;

      generic
         with package NP is new P (T => <>);  --  FLAG
      package R is
      end R;

   begin
      null;
   end Fail_1;

   --  Attempt to use removed formal_package_association production (second
   --  alternative) to specify that only some actuals are the box
   Fail_2 : declare

      generic
         type Component is private;
         type Index is (<>);
         type List is array (Index range <>) of Component;
      package P is
      end P;

      generic
         type Item is private;
         with package NP is new P (Component => Item, Index => <>, List => <>);  --  FLAG
         --  FACE 3.1 disallows the above in Safety Extended and Safety Base
         --  (and Security) because the generic formal parameter associations
         --  are mixed: some actuals are specified, and some are the box. The
         --  syntax alternative for doing so has been removed by FACE.
      package R is
      end R;

   begin
      null;
   end Fail_2;

   --  Attempt to use removed formal_package_association production (second
   --  alternative) to specify that some actuals are the box using "others"
   Fail_3 : declare

      generic
         type Component is private;
         type Index is (<>);
         type List is array (Index range <>) of Component;
      package P is
      end P;

      generic
         type Item is private;
         with package NP is new P (Component => Item, others => <>);  --  FLAG
         --  FACE 3.1 disallows the above in Safety Extended and Safety Base
         --  (and Security) because the generic formal parameter associations
         --  are mixed: some actuals are specified, and some are the box. The
         --  syntax alternative for doing so has been removed by FACE.
      package R is
      end R;

   begin
      null;
   end Fail_3;

   --  Attempt to use "others" in (modified) first alternative in the
   --  formal_package_actual_part production
   Fail_4 : declare

      generic
         type Component is private;
         type Index is (<>);
         type List is array (Index range <>) of Component;
      package P is
      end P;

      generic
         type Item is private;
         type Items is array (Positive range <>) of Item;
         with package NP is new P (others => <>);  --  FLAG
         --  FACE 3.1 disallows the above in Safety Extended and Safety
         --  Base (and Security) because the "others" is not included in
         --  the production's first alternative.
      package R is
      end R;

   begin
      null;
   end Fail_4;

   --  Attempt to use removed formal_package_association production, second
   --  alternative, to specify individually that all generic actuals are the
   --  box:
   --
   --     formal_package_association ::=
   --         generic_association
   --       | generic_formal_parameter_selector_name => <>
   --
   --  The only allowed syntax in FACE is "(<>)".
   Fail_5 : declare

      generic
         type Component is private;
         type Index is (<>);
         type List is array (Index range <>) of Component;
      package P is
      end P;

      generic
         type Item is private;
         with package NP is new P (Component => <>, Index => <>, List => <>);  --  FLAG
         --  FACE 3.1 disallows the above in Safety Extended and Safety
         --  Base (and Security) because we are trying to specify the box
         --  individually for each actual, using the second alternative
      package R is
      end R;

   begin
      null;
   end Fail_5;
end Formal;

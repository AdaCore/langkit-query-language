with Libadalang.Introspection;

package body Ada_AST_Node is

   -----------------------
   -- Matches_Kind_Name --
   -----------------------

   overriding function Matches_Kind_Name
     (Node : Ada_AST_Node; Kind_Name : String) return Boolean
   is
      use Libadalang.Introspection;
      Expected_Kind : constant Any_Node_Type_Id :=
        Lookup_DSL_Name (Kind_Name);
      Actual_Kind   : constant Any_Node_Type_Id :=
        Id_For_Kind (Node.Node.Kind);
   begin
      pragma Assert
        (Expected_Kind /= None, "Invalid kind name: " & Kind_Name);

      return Actual_Kind = Expected_Kind or else
             Is_Derived_From (Actual_Kind, Expected_Kind);
   end Matches_Kind_Name;

end Ada_AST_Node;

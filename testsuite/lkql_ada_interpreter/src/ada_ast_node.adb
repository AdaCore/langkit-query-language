with Libadalang.Introspection; use Libadalang.Introspection;
with Libadalang.Common; use Libadalang.Common;

with Ada.Unchecked_Conversion;
with Ada.Assertions; use Ada.Assertions;
with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;

package body Ada_AST_Node is

   subtype Built_In_LAL_Field is Node_Data_Reference
      range Ada_Node_Parent .. Ada_Node_Is_Ghost;

   Empty_Value_Array : constant Value_Array (1 .. 0) := (others => <>);
   --  Empty Array of Value_Type values

   function Data_Reference_For_Name (Receiver : Ada_AST_Node;
                                     Name     : Text_Type)
                                     return Any_Node_Data_Reference;
   --  Return the node data type corresponding to 'Name' on the receiver
   --  node. Return None if the name is invalid.

   function Is_Built_In (Name : Text_Type) return Boolean;
   --  Return whether the property named 'Name' is built-in

   function Built_In_Field (Receiver      : Ada_AST_Node;
                            Property_Name : Text_Type)
                            return Introspection_Value;
   --  Return the value of the built-in property named 'Property_Name' on
   --  'Receiver'.

   function Make_Introspection_Value
     (Value : Text_Type) return Introspection_Value;
   --  Create an Introspection value from the given Text_Type value

   function Make_Introspection_Value
     (Value : Value_Type) return Introspection_Value;
   --  Create an Introspection_value value from the given Value_type value

   function Make_Introspection_Value
     (Value : Unbounded_Text_Type_Array) return Introspection_Value;
   --  Create an Introspection value from the given string array

   function Get_Property_Ref (Node          : Ada_AST_Node;
                              Property_Name : Text_Type)
                              return Property_Reference;
   --  Return the reference of the property named `Property_Name` on `Node`.
   --  Raise an exception if there is no such property.

   function Make_Value_Type (Value       : Introspection_Value;
                             Target_Kind : Value_Kind)
                             return Value_Type;
   --  Create a Value_Type value from the given Introspection value

   function Array_To_Value_Type (Value       : AST_Node_Array;
                                 Target_Kind : Value_Kind)
                                 return Value_Type;

   function String_To_Value_Type (Value       : Unbounded_Text_Type;
                                  Target_Kind : Value_Kind)
                                  return Value_Type;

   --------------------------------------------------
   -- Node array to introspection value conversion --
   --------------------------------------------------

   generic
      type Array_Elem is new Ada_Node with private;

      type Array_Type is array (Positive range <>) of Array_Elem;

   function Introspection_Value_From_Array
     (Nodes : Array_Type) return Introspection_Value;

   ------------------------------------
   -- Introspection_Value_From_Array --
   ------------------------------------

   function Introspection_Value_From_Array
     (Nodes : Array_Type) return Introspection_Value
   is
      Result        : constant AST_Node_Array_Access :=
        new AST_Node_Array (1 .. Nodes'Length);
   begin
      for I in Nodes'Range loop
         Result (I) := new Ada_AST_Node'(Node => Nodes (I).As_Ada_Node);
      end loop;

      return (Kind => Kind_Node_Array, Node_Array_Val => Result);
   end Introspection_Value_From_Array;

   function Make_Introspection_Value is new
     Introspection_Value_From_Array (Ada_Node, Ada_Node_Array);

   function Make_Introspection_Value is new
     Introspection_Value_From_Array (Basic_Decl, Basic_Decl_Array);

   function Make_Introspection_Value is new
     Introspection_Value_From_Array
       (Base_Formal_Param_Decl, Base_Formal_Param_Decl_Array);

   function Make_Introspection_Value is new
     Introspection_Value_From_Array
       (Generic_Instantiation, Generic_Instantiation_Array);

   function Make_Introspection_Value is new
     Introspection_Value_From_Array (Defining_Name, Defining_Name_Array);

   -----------------------------------------------
   -- Node array to Value_Type array conversion --
   -----------------------------------------------

   generic

      type Node_Type is new Ada_Node with private;

      type Node_Array is array (Positive range <>) of Node_Type;

      with function Convert (Node : Ada_Node'Class) return Node_Type;

   function Node_Array_From_List
     (Nodes : AST_Node_Array) return Node_Array;

   function Introspection_Node_To_Ada_Node is new Ada.Unchecked_Conversion
     (AST_Node_Access, Ada_AST_Node_Access);

   --------------------------
   -- Node_Array_From_List --
   --------------------------

   function Node_Array_From_List
     (Nodes : AST_Node_Array) return Node_Array
   is
      Result       : Node_Array (1 .. Nodes'Length);
   begin
      for I in Nodes'Range loop
         Result (I) :=
           Convert (Introspection_Node_To_Ada_Node (Nodes (I)).Node);
      end loop;

      return Result;
   end Node_Array_From_List;

   function Ada_Node_Array_From_AST_Array is new Node_Array_From_List
     (Ada_Node, Ada_Node_Array, As_Ada_Node);

   function Base_Formal_Param_Decl_Array_From_AST_Array is
     new Node_Array_From_List
        (Base_Formal_Param_Decl,
         Base_Formal_Param_Decl_Array,
         As_Base_Formal_Param_Decl);

   function Basic_Decl_Array_From_AST_Array is new Node_Array_From_List
     (Basic_Decl, Basic_Decl_Array, As_Basic_Decl);

   function Defining_Name_Array_From_AST_Array is new Node_Array_From_List
     (Defining_Name, Defining_Name_Array, As_Defining_Name);

   function Generic_Instantiation_Array_From_AST_Array is
     new Node_Array_From_List
       (Generic_Instantiation,
        Generic_Instantiation_Array,
        As_Generic_Instantiation);

   function Param_Spec_Array_From_AST_Array is new Node_Array_From_List
     (Param_Spec, Param_Spec_Array, As_Param_Spec);

   -----------------------------
   -- Data_Reference_For_Name --
   -----------------------------

   function Data_Reference_For_Name
     (Receiver : Ada_AST_Node; Name : Text_Type) return Any_Node_Data_Reference
   is
      Receiver_Type_Id : constant Node_Type_Id :=
        Id_For_Kind (Receiver.Node.Kind);
   begin
      return Lookup_Node_Data (Receiver_Type_Id, To_UTF8 (Name));
   end Data_Reference_For_Name;
   
   ---------------------
   -- Node_Prototypes --
   ---------------------
   
   function Node_Prototypes (Node : Ada_AST_Node) return String is
      ("astnode AdaNode: ASTNode {" & ASCII.LF &
      "    field text() -> string" & ASCII.LF &
      "    field image() -> string" & ASCII.LF &
      "    field child_index() -> int" & ASCII.LF &
      "" & ASCII.LF &
      "    property parent() -> AdaNode" & ASCII.LF &
      "    property parents() -> List<AdaNode>" & ASCII.LF &
      "    property children() -> List<AdaNode>" & ASCII.LF &
      "    property token_start() -> Token" & ASCII.LF &
      "    property token_end() -> Token" & ASCII.LF &
      "    property previous_sibling() -> AdaNode" & ASCII.LF &
      "    property next_sibling() -> AdaNode" & ASCII.LF &
      "    property unit() -> AnalysisUnit" & ASCII.LF &
      "    property is_ghost() -> bool" & ASCII.LF &
      "    property xref(imprecise_fallback: bool) -> DefiningName" & ASCII.LF &
      "    property complete() -> List<BasicDecl>" & ASCII.LF &
      "    property generic_instantiations() -> List<GenericInstantiation>" & ASCII.LF &
      "    property semantic_parent() -> AdaNode" & ASCII.LF &
      "    property filter_is_imported_by(units: List<AnalysisUnit>, transitive: bool) -> List<AnalysisUnit>" & ASCII.LF &
      "    property xref_entry_point() -> bool" & ASCII.LF &
      "    property resolve_names() -> bool" & ASCII.LF &
      "    property body_unit() -> AnalysisUnit" & ASCII.LF &
      "    property spec_unit() -> AnalysisUnit" & ASCII.LF &
      "    property standard_unit() -> AnalysisUnit" & ASCII.LF &
      "    property std_entity(sym: Symbol) -> AdaNode" & ASCII.LF &
      "    property bool_type() -> AdaNode" & ASCII.LF &
      "    property int_type() -> AdaNode" & ASCII.LF &
      "    property universal_int_type() -> AdaNode" & ASCII.LF &
      "    property universal_real_type() -> AdaNode" & ASCII.LF &
      "    property top_level_decl(unit: AnalysisUnit) -> BasicDecl" & ASCII.LF &
      "    property gnat_xref(imprecise_fallback: bool) -> DefiningName" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode Abort: AdaNode {" & ASCII.LF &
      "" & ASCII.LF &
      "    property as_bool() -> bool" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode AbortAbsent: Abort {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode AbortPresent: Abort {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode Abstract: AdaNode {" & ASCII.LF &
      "" & ASCII.LF &
      "    property as_bool() -> bool" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode AbstractAbsent: Abstract {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode AbstractPresent: Abstract {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode AdaList: AdaNode {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode AdaNode.list: AdaList {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode AlternativesList: AdaNode.list {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode ConstraintList: AdaNode.list {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode DeclList: AdaNode.list {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode StmtList: AdaNode.list {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode AspectAssoc.list: AdaList {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode BaseAssoc.list: AdaList {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode BasicAssoc.list: AdaList {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode AssocList: BasicAssoc.list {" & ASCII.LF &
      "" & ASCII.LF &
      "    property zip_with_params(imprecise_fallback: bool) -> List<ParamActual>" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode CaseExprAlternative.list: AdaList {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode CaseStmtAlternative.list: AdaList {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode CompilationUnit.list: AdaList {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode ContractCaseAssoc.list: AdaList {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode DefiningName.list: AdaList {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode DiscriminantSpec.list: AdaList {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode ElsifExprPart.list: AdaList {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode ElsifStmtPart.list: AdaList {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode EnumLiteralDecl.list: AdaList {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode Expr.list: AdaList {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode ExprAlternativesList: Expr.list {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode Identifier.list: AdaList {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode DiscriminantChoiceList: Identifier.list {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode Name.list: AdaList {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode ParentList: Name.list {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode ParamSpec.list: AdaList {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode Pragma.list: AdaList {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode SelectWhenPart.list: AdaList {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode UnconstrainedArrayIndex.list: AdaList {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode Variant.list: AdaList {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode Aliased: AdaNode {" & ASCII.LF &
      "" & ASCII.LF &
      "    property as_bool() -> bool" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode AliasedAbsent: Aliased {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode AliasedPresent: Aliased {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode All: AdaNode {" & ASCII.LF &
      "" & ASCII.LF &
      "    property as_bool() -> bool" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode AllAbsent: All {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode AllPresent: All {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode ArrayIndices: AdaNode {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode ConstrainedArrayIndices: ArrayIndices {" & ASCII.LF &
      "    field list() -> ConstraintList" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode UnconstrainedArrayIndices: ArrayIndices {" & ASCII.LF &
      "    field types() -> UnconstrainedArrayIndex.list" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode AspectAssoc: AdaNode {" & ASCII.LF &
      "    field id() -> Name" & ASCII.LF &
      "    field expr() -> Expr" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode AspectClause: AdaNode {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode AtClause: AspectClause {" & ASCII.LF &
      "    field name() -> BaseId" & ASCII.LF &
      "    field expr() -> Expr" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode AttributeDefClause: AspectClause {" & ASCII.LF &
      "    field attribute_expr() -> Name" & ASCII.LF &
      "    field expr() -> Expr" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode EnumRepClause: AspectClause {" & ASCII.LF &
      "    field type_name() -> Name" & ASCII.LF &
      "    field aggregate() -> BaseAggregate" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode RecordRepClause: AspectClause {" & ASCII.LF &
      "    field name() -> Name" & ASCII.LF &
      "    field at_expr() -> Expr" & ASCII.LF &
      "    field components() -> AdaNode.list" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode AspectSpec: AdaNode {" & ASCII.LF &
      "    field aspect_assocs() -> AspectAssoc.list" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode BaseAssoc: AdaNode {" & ASCII.LF &
      "" & ASCII.LF &
      "    property assoc_expr() -> Expr" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode ContractCaseAssoc: BaseAssoc {" & ASCII.LF &
      "    field guard() -> AdaNode" & ASCII.LF &
      "    field consequence() -> Expr" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode PragmaArgumentAssoc: BaseAssoc {" & ASCII.LF &
      "    field id() -> Identifier" & ASCII.LF &
      "    field expr() -> Expr" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode BaseFormalParamHolder: AdaNode {" & ASCII.LF &
      "" & ASCII.LF &
      "    property abstract_formal_params() -> List<BaseFormalParamDecl>" & ASCII.LF &
      "    property nb_min_params() -> int" & ASCII.LF &
      "    property nb_max_params() -> int" & ASCII.LF &
      "    property return_type(origin: AdaNode) -> BaseTypeDecl" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode BaseSubpSpec: BaseFormalParamHolder {" & ASCII.LF &
      "" & ASCII.LF &
      "    property returns() -> TypeExpr" & ASCII.LF &
      "    property params() -> List<ParamSpec>" & ASCII.LF &
      "    property primitive_subp_of() -> List<BaseTypeDecl>" & ASCII.LF &
      "    property first_primitive_subp_of() -> BaseTypeDecl" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode EntrySpec: BaseSubpSpec {" & ASCII.LF &
      "    field entry_name() -> DefiningName" & ASCII.LF &
      "    field family_type() -> AdaNode" & ASCII.LF &
      "    field entry_params() -> Params" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode EnumSubpSpec: BaseSubpSpec {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode SubpSpec: BaseSubpSpec {" & ASCII.LF &
      "    field subp_kind() -> SubpKind" & ASCII.LF &
      "    field subp_name() -> DefiningName" & ASCII.LF &
      "    field subp_params() -> Params" & ASCII.LF &
      "    field subp_returns() -> TypeExpr" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode ComponentList: BaseFormalParamHolder {" & ASCII.LF &
      "    field components() -> AdaNode.list" & ASCII.LF &
      "    field variant_part() -> VariantPart" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode DiscriminantPart: BaseFormalParamHolder {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode KnownDiscriminantPart: DiscriminantPart {" & ASCII.LF &
      "    field discr_specs() -> DiscriminantSpec.list" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode UnknownDiscriminantPart: DiscriminantPart {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode GenericFormalPart: BaseFormalParamHolder {" & ASCII.LF &
      "    field decls() -> AdaNode.list" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode BaseRecordDef: AdaNode {" & ASCII.LF &
      "    field components() -> ComponentList" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode NullRecordDef: BaseRecordDef {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode RecordDef: BaseRecordDef {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode BasicAssoc: AdaNode {" & ASCII.LF &
      "" & ASCII.LF &
      "    property get_params(imprecise_fallback: bool) -> List<DefiningName>" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode AggregateAssoc: BasicAssoc {" & ASCII.LF &
      "    field designators() -> AlternativesList" & ASCII.LF &
      "    field r_expr() -> Expr" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode MultiDimArrayAssoc: AggregateAssoc {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode DiscriminantAssoc: BasicAssoc {" & ASCII.LF &
      "    field ids() -> DiscriminantChoiceList" & ASCII.LF &
      "    field discr_expr() -> Expr" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode ParamAssoc: BasicAssoc {" & ASCII.LF &
      "    field designator() -> AdaNode" & ASCII.LF &
      "    field r_expr() -> Expr" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode BasicDecl: AdaNode {" & ASCII.LF &
      "    field aspects() -> AspectSpec" & ASCII.LF &
      "" & ASCII.LF &
      "    property doc_annotations() -> List<DocAnnotation>" & ASCII.LF &
      "    property doc() -> List<Character>" & ASCII.LF &
      "    property previous_part_for_decl() -> BasicDecl" & ASCII.LF &
      "    property canonical_part() -> BasicDecl" & ASCII.LF &
      "    property is_static_decl(imprecise_fallback: bool) -> bool" & ASCII.LF &
      "    property is_imported() -> bool" & ASCII.LF &
      "    property get_aspect(name: Symbol) -> AspectAssoc" & ASCII.LF &
      "    property get_aspect_expr(name: Symbol) -> Expr" & ASCII.LF &
      "    property get_attribute(name: Symbol, imprecise_fallback: bool) -> AdaNode" & ASCII.LF &
      "    property get_pragma(name: Symbol) -> Pragma" & ASCII.LF &
      "    property get_representation_clause(name: Symbol, imprecise_fallback: bool) -> AttributeDefClause" & ASCII.LF &
      "    property is_unit_root() -> bool" & ASCII.LF &
      "    property defining_names() -> List<DefiningName>" & ASCII.LF &
      "    property defining_name() -> DefiningName" & ASCII.LF &
      "    property type_expression() -> TypeExpr" & ASCII.LF &
      "    property subp_spec_or_null(follow_generic: bool) -> BaseSubpSpec" & ASCII.LF &
      "    property is_subprogram() -> bool" & ASCII.LF &
      "    property declarative_scope() -> DeclarativePart" & ASCII.LF &
      "    property relative_name() -> SingleTokNode" & ASCII.LF &
      "    property relative_name_text() -> Symbol" & ASCII.LF &
      "    property next_part_for_decl() -> BasicDecl" & ASCII.LF &
      "    property body_part_for_decl() -> Body" & ASCII.LF &
      "    property fully_qualified_name_array() -> List<Symbol>" & ASCII.LF &
      "    property fully_qualified_name() -> List<Character>" & ASCII.LF &
      "    property unique_identifying_name() -> List<Character>" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode BaseFormalParamDecl: BasicDecl {" & ASCII.LF &
      "" & ASCII.LF &
      "    property formal_type() -> BaseTypeDecl" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode ComponentDecl: BaseFormalParamDecl {" & ASCII.LF &
      "    field ids() -> DefiningName.list" & ASCII.LF &
      "    field component_def() -> ComponentDef" & ASCII.LF &
      "    field default_expr() -> Expr" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode DiscriminantSpec: BaseFormalParamDecl {" & ASCII.LF &
      "    field ids() -> DefiningName.list" & ASCII.LF &
      "    field type_expr() -> TypeExpr" & ASCII.LF &
      "    field default_expr() -> Expr" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode GenericFormal: BaseFormalParamDecl {" & ASCII.LF &
      "    field decl() -> BasicDecl" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode GenericFormalObjDecl: GenericFormal {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode GenericFormalPackage: GenericFormal {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode GenericFormalSubpDecl: GenericFormal {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode GenericFormalTypeDecl: GenericFormal {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode ParamSpec: BaseFormalParamDecl {" & ASCII.LF &
      "    field ids() -> DefiningName.list" & ASCII.LF &
      "    field has_aliased() -> Aliased" & ASCII.LF &
      "    field mode() -> Mode" & ASCII.LF &
      "    field type_expr() -> TypeExpr" & ASCII.LF &
      "    field default_expr() -> Expr" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode BasePackageDecl: BasicDecl {" & ASCII.LF &
      "    field package_name() -> DefiningName" & ASCII.LF &
      "    field public_part() -> PublicPart" & ASCII.LF &
      "    field private_part() -> PrivatePart" & ASCII.LF &
      "    field end_name() -> EndName" & ASCII.LF &
      "" & ASCII.LF &
      "    property body_part() -> PackageBody" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode GenericPackageInternal: BasePackageDecl {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode PackageDecl: BasePackageDecl {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode BaseTypeDecl: BasicDecl {" & ASCII.LF &
      "    field name() -> DefiningName" & ASCII.LF &
      "" & ASCII.LF &
      "    property private_completion() -> BaseTypeDecl" & ASCII.LF &
      "    property get_record_representation_clause(imprecise_fallback: bool) -> RecordRepClause" & ASCII.LF &
      "    property is_record_type() -> bool" & ASCII.LF &
      "    property is_array_type(origin: AdaNode) -> bool" & ASCII.LF &
      "    property is_real_type() -> bool" & ASCII.LF &
      "    property is_float_type() -> bool" & ASCII.LF &
      "    property is_fixed_point() -> bool" & ASCII.LF &
      "    property is_access_type(origin: AdaNode) -> bool" & ASCII.LF &
      "    property is_discrete_type(origin: AdaNode) -> bool" & ASCII.LF &
      "    property is_int_type(origin: AdaNode) -> bool" & ASCII.LF &
      "    property accessed_type(origin: AdaNode) -> BaseTypeDecl" & ASCII.LF &
      "    property base_type() -> BaseTypeDecl" & ASCII.LF &
      "    property comp_type(is_subscript: bool, origin: AdaNode) -> BaseTypeDecl" & ASCII.LF &
      "    property index_type(dim: int, origin: AdaNode) -> BaseTypeDecl" & ASCII.LF &
      "    property is_derived_type(other_type: BaseTypeDecl, origin: AdaNode) -> bool" & ASCII.LF &
      "    property matching_type(expected_type: BaseTypeDecl, origin: AdaNode) -> bool" & ASCII.LF &
      "    property canonical_type(origin: AdaNode) -> BaseTypeDecl" & ASCII.LF &
      "    property previous_part(go_to_incomplete: bool) -> BaseTypeDecl" & ASCII.LF &
      "    property next_part() -> BaseTypeDecl" & ASCII.LF &
      "    property is_private() -> bool" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode BaseSubtypeDecl: BaseTypeDecl {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode DiscreteBaseSubtypeDecl: BaseSubtypeDecl {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode SubtypeDecl: BaseSubtypeDecl {" & ASCII.LF &
      "    field subtype() -> SubtypeIndication" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode ClasswideTypeDecl: BaseTypeDecl {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode IncompleteTypeDecl: BaseTypeDecl {" & ASCII.LF &
      "    field discriminants() -> DiscriminantPart" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode IncompleteTaggedTypeDecl: IncompleteTypeDecl {" & ASCII.LF &
      "    field has_abstract() -> Abstract" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode ProtectedTypeDecl: BaseTypeDecl {" & ASCII.LF &
      "    field discriminants() -> DiscriminantPart" & ASCII.LF &
      "    field interfaces() -> ParentList" & ASCII.LF &
      "    field definition() -> ProtectedDef" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode TaskTypeDecl: BaseTypeDecl {" & ASCII.LF &
      "    field discriminants() -> DiscriminantPart" & ASCII.LF &
      "    field definition() -> TaskDef" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode SingleTaskTypeDecl: TaskTypeDecl {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode TypeDecl: BaseTypeDecl {" & ASCII.LF &
      "    field discriminants() -> DiscriminantPart" & ASCII.LF &
      "    field type_def() -> TypeDef" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode AnonymousTypeDecl: TypeDecl {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode SynthAnonymousTypeDecl: AnonymousTypeDecl {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode BasicSubpDecl: BasicDecl {" & ASCII.LF &
      "" & ASCII.LF &
      "    property subp_decl_spec() -> BaseSubpSpec" & ASCII.LF &
      "    property body_part() -> BaseSubpBody" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode ClassicSubpDecl: BasicSubpDecl {" & ASCII.LF &
      "    field overriding() -> Overriding" & ASCII.LF &
      "    field subp_spec() -> SubpSpec" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode AbstractSubpDecl: ClassicSubpDecl {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode FormalSubpDecl: ClassicSubpDecl {" & ASCII.LF &
      "    field default_expr() -> Expr" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode AbstractFormalSubpDecl: FormalSubpDecl {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode ConcreteFormalSubpDecl: FormalSubpDecl {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode SubpDecl: ClassicSubpDecl {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode EnumLiteralDecl: BasicSubpDecl {" & ASCII.LF &
      "    field name() -> DefiningName" & ASCII.LF &
      "" & ASCII.LF &
      "    property enum_type() -> TypeDecl" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode GenericSubpInternal: BasicSubpDecl {" & ASCII.LF &
      "    field subp_spec() -> SubpSpec" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode Body: BasicDecl {" & ASCII.LF &
      "" & ASCII.LF &
      "    property previous_part() -> BasicDecl" & ASCII.LF &
      "    property decl_part() -> BasicDecl" & ASCII.LF &
      "    property subunit_root() -> BasicDecl" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode BaseSubpBody: Body {" & ASCII.LF &
      "    field overriding() -> Overriding" & ASCII.LF &
      "    field subp_spec() -> SubpSpec" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode ExprFunction: BaseSubpBody {" & ASCII.LF &
      "    field expr() -> Expr" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode NullSubpDecl: BaseSubpBody {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode SubpBody: BaseSubpBody {" & ASCII.LF &
      "    field decls() -> DeclarativePart" & ASCII.LF &
      "    field stmts() -> HandledStmts" & ASCII.LF &
      "    field end_name() -> EndName" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode SubpRenamingDecl: BaseSubpBody {" & ASCII.LF &
      "    field renames() -> RenamingClause" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode BodyStub: Body {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode PackageBodyStub: BodyStub {" & ASCII.LF &
      "    field name() -> DefiningName" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode ProtectedBodyStub: BodyStub {" & ASCII.LF &
      "    field name() -> DefiningName" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode SubpBodyStub: BodyStub {" & ASCII.LF &
      "    field overriding() -> Overriding" & ASCII.LF &
      "    field subp_spec() -> SubpSpec" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode TaskBodyStub: BodyStub {" & ASCII.LF &
      "    field name() -> DefiningName" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode EntryBody: Body {" & ASCII.LF &
      "    field entry_name() -> DefiningName" & ASCII.LF &
      "    field index_spec() -> EntryIndexSpec" & ASCII.LF &
      "    field params() -> Params" & ASCII.LF &
      "    field barrier() -> Expr" & ASCII.LF &
      "    field decls() -> DeclarativePart" & ASCII.LF &
      "    field stmts() -> HandledStmts" & ASCII.LF &
      "    field end_name() -> EndName" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode PackageBody: Body {" & ASCII.LF &
      "    field package_name() -> DefiningName" & ASCII.LF &
      "    field decls() -> DeclarativePart" & ASCII.LF &
      "    field stmts() -> HandledStmts" & ASCII.LF &
      "    field end_name() -> EndName" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode ProtectedBody: Body {" & ASCII.LF &
      "    field name() -> DefiningName" & ASCII.LF &
      "    field decls() -> DeclarativePart" & ASCII.LF &
      "    field end_name() -> EndName" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode TaskBody: Body {" & ASCII.LF &
      "    field name() -> DefiningName" & ASCII.LF &
      "    field decls() -> DeclarativePart" & ASCII.LF &
      "    field stmts() -> HandledStmts" & ASCII.LF &
      "    field end_name() -> EndName" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode EntryDecl: BasicDecl {" & ASCII.LF &
      "    field overriding() -> Overriding" & ASCII.LF &
      "    field spec() -> EntrySpec" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode EntryIndexSpec: BasicDecl {" & ASCII.LF &
      "    field id() -> DefiningName" & ASCII.LF &
      "    field subtype() -> AdaNode" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode ErrorDecl: BasicDecl {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode ExceptionDecl: BasicDecl {" & ASCII.LF &
      "    field ids() -> DefiningName.list" & ASCII.LF &
      "    field renames() -> RenamingClause" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode ExceptionHandler: BasicDecl {" & ASCII.LF &
      "    field exception_name() -> DefiningName" & ASCII.LF &
      "    field handled_exceptions() -> AlternativesList" & ASCII.LF &
      "    field stmts() -> StmtList" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode ForLoopVarDecl: BasicDecl {" & ASCII.LF &
      "    field id() -> DefiningName" & ASCII.LF &
      "    field id_type() -> SubtypeIndication" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode GenericDecl: BasicDecl {" & ASCII.LF &
      "    field formal_part() -> GenericFormalPart" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode GenericPackageDecl: GenericDecl {" & ASCII.LF &
      "    field package_decl() -> GenericPackageInternal" & ASCII.LF &
      "" & ASCII.LF &
      "    property body_part() -> PackageBody" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode GenericSubpDecl: GenericDecl {" & ASCII.LF &
      "    field subp_decl() -> GenericSubpInternal" & ASCII.LF &
      "" & ASCII.LF &
      "    property body_part() -> BaseSubpBody" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode GenericInstantiation: BasicDecl {" & ASCII.LF &
      "" & ASCII.LF &
      "    property designated_generic_decl() -> BasicDecl" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode GenericPackageInstantiation: GenericInstantiation {" & ASCII.LF &
      "    field name() -> DefiningName" & ASCII.LF &
      "    field generic_pkg_name() -> Name" & ASCII.LF &
      "    field params() -> AssocList" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode GenericSubpInstantiation: GenericInstantiation {" & ASCII.LF &
      "    field overriding() -> Overriding" & ASCII.LF &
      "    field kind() -> SubpKind" & ASCII.LF &
      "    field subp_name() -> DefiningName" & ASCII.LF &
      "    field generic_subp_name() -> Name" & ASCII.LF &
      "    field params() -> AssocList" & ASCII.LF &
      "" & ASCII.LF &
      "    property designated_subp() -> AdaNode" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode GenericRenamingDecl: BasicDecl {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode GenericPackageRenamingDecl: GenericRenamingDecl {" & ASCII.LF &
      "    field name() -> DefiningName" & ASCII.LF &
      "    field renames() -> Name" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode GenericSubpRenamingDecl: GenericRenamingDecl {" & ASCII.LF &
      "    field kind() -> SubpKind" & ASCII.LF &
      "    field name() -> DefiningName" & ASCII.LF &
      "    field renames() -> Name" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode LabelDecl: BasicDecl {" & ASCII.LF &
      "    field name() -> DefiningName" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode NamedStmtDecl: BasicDecl {" & ASCII.LF &
      "    field name() -> DefiningName" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode NumberDecl: BasicDecl {" & ASCII.LF &
      "    field ids() -> DefiningName.list" & ASCII.LF &
      "    field expr() -> Expr" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode ObjectDecl: BasicDecl {" & ASCII.LF &
      "    field ids() -> DefiningName.list" & ASCII.LF &
      "    field has_aliased() -> Aliased" & ASCII.LF &
      "    field has_constant() -> Constant" & ASCII.LF &
      "    field mode() -> Mode" & ASCII.LF &
      "    field type_expr() -> TypeExpr" & ASCII.LF &
      "    field default_expr() -> Expr" & ASCII.LF &
      "    field renaming_clause() -> RenamingClause" & ASCII.LF &
      "" & ASCII.LF &
      "    property public_part_decl() -> BasicDecl" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode ExtendedReturnStmtObjectDecl: ObjectDecl {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode PackageRenamingDecl: BasicDecl {" & ASCII.LF &
      "    field name() -> DefiningName" & ASCII.LF &
      "    field renames() -> RenamingClause" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode SingleProtectedDecl: BasicDecl {" & ASCII.LF &
      "    field name() -> DefiningName" & ASCII.LF &
      "    field interfaces() -> ParentList" & ASCII.LF &
      "    field definition() -> ProtectedDef" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode SingleTaskDecl: BasicDecl {" & ASCII.LF &
      "    field task_type() -> SingleTaskTypeDecl" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode CaseStmtAlternative: AdaNode {" & ASCII.LF &
      "    field choices() -> AlternativesList" & ASCII.LF &
      "    field stmts() -> StmtList" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode CompilationUnit: AdaNode {" & ASCII.LF &
      "    field prelude() -> AdaNode.list" & ASCII.LF &
      "    field body() -> AdaNode" & ASCII.LF &
      "    field pragmas() -> Pragma.list" & ASCII.LF &
      "" & ASCII.LF &
      "    property syntactic_fully_qualified_name() -> List<Symbol>" & ASCII.LF &
      "    property unit_kind() -> AnalysisUnitKind" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode ComponentClause: AdaNode {" & ASCII.LF &
      "    field id() -> Identifier" & ASCII.LF &
      "    field position() -> Expr" & ASCII.LF &
      "    field range() -> RangeSpec" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode ComponentDef: AdaNode {" & ASCII.LF &
      "    field has_aliased() -> Aliased" & ASCII.LF &
      "    field has_constant() -> Constant" & ASCII.LF &
      "    field type_expr() -> TypeExpr" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode Constant: AdaNode {" & ASCII.LF &
      "" & ASCII.LF &
      "    property as_bool() -> bool" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode ConstantAbsent: Constant {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode ConstantPresent: Constant {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode Constraint: AdaNode {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode DeltaConstraint: Constraint {" & ASCII.LF &
      "    field digits() -> Expr" & ASCII.LF &
      "    field range() -> RangeSpec" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode DigitsConstraint: Constraint {" & ASCII.LF &
      "    field digits() -> Expr" & ASCII.LF &
      "    field range() -> RangeSpec" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode DiscriminantConstraint: Constraint {" & ASCII.LF &
      "    field constraints() -> AssocList" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode IndexConstraint: Constraint {" & ASCII.LF &
      "    field constraints() -> ConstraintList" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode RangeConstraint: Constraint {" & ASCII.LF &
      "    field range() -> RangeSpec" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode DeclarativePart: AdaNode {" & ASCII.LF &
      "    field decls() -> AdaNode.list" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode PrivatePart: DeclarativePart {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode PublicPart: DeclarativePart {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode ElsifExprPart: AdaNode {" & ASCII.LF &
      "    field cond_expr() -> Expr" & ASCII.LF &
      "    field then_expr() -> Expr" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode ElsifStmtPart: AdaNode {" & ASCII.LF &
      "    field cond_expr() -> Expr" & ASCII.LF &
      "    field stmts() -> StmtList" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode Expr: AdaNode {" & ASCII.LF &
      "" & ASCII.LF &
      "    property expression_type() -> BaseTypeDecl" & ASCII.LF &
      "    property is_static_expr(imprecise_fallback: bool) -> bool" & ASCII.LF &
      "    property first_corresponding_decl() -> BasicDecl" & ASCII.LF &
      "    property eval_as_int() -> BigInt" & ASCII.LF &
      "    property matching_nodes() -> List<AdaNode>" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode Allocator: Expr {" & ASCII.LF &
      "    field subpool() -> Name" & ASCII.LF &
      "    field type_or_expr() -> AdaNode" & ASCII.LF &
      "" & ASCII.LF &
      "    property get_allocated_type() -> BaseTypeDecl" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode BaseAggregate: Expr {" & ASCII.LF &
      "    field ancestor_expr() -> Expr" & ASCII.LF &
      "    field assocs() -> AssocList" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode Aggregate: BaseAggregate {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode NullRecordAggregate: BaseAggregate {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode BinOp: Expr {" & ASCII.LF &
      "    field left() -> Expr" & ASCII.LF &
      "    field op() -> Op" & ASCII.LF &
      "    field right() -> Expr" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode RelationOp: BinOp {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode BoxExpr: Expr {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode CaseExpr: Expr {" & ASCII.LF &
      "    field expr() -> Expr" & ASCII.LF &
      "    field cases() -> CaseExprAlternative.list" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode CaseExprAlternative: Expr {" & ASCII.LF &
      "    field choices() -> AlternativesList" & ASCII.LF &
      "    field expr() -> Expr" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode ContractCases: Expr {" & ASCII.LF &
      "    field contract_cases() -> ContractCaseAssoc.list" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode IfExpr: Expr {" & ASCII.LF &
      "    field cond_expr() -> Expr" & ASCII.LF &
      "    field then_expr() -> Expr" & ASCII.LF &
      "    field alternatives() -> ElsifExprPart.list" & ASCII.LF &
      "    field else_expr() -> Expr" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode MembershipExpr: Expr {" & ASCII.LF &
      "    field expr() -> Expr" & ASCII.LF &
      "    field op() -> Op" & ASCII.LF &
      "    field membership_exprs() -> ExprAlternativesList" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode Name: Expr {" & ASCII.LF &
      "" & ASCII.LF &
      "    property enclosing_defining_name() -> DefiningName" & ASCII.LF &
      "    property is_defining() -> bool" & ASCII.LF &
      "    property name_is(sym: Symbol) -> bool" & ASCII.LF &
      "    property is_call() -> bool" & ASCII.LF &
      "    property is_dot_call(imprecise_fallback: bool) -> bool" & ASCII.LF &
      "    property referenced_id(ref_decl: BasicDecl) -> DefiningName" & ASCII.LF &
      "    property all_env_elements(seq: bool, seq_from: AdaNode) -> List<AdaNode>" & ASCII.LF &
      "    property referenced_decl(imprecise_fallback: bool) -> BasicDecl" & ASCII.LF &
      "    property referenced_decl_internal(try_immediate: bool, imprecise_fallback: bool) -> BasicDecl" & ASCII.LF &
      "    property name_designated_type() -> BaseTypeDecl" & ASCII.LF &
      "    property is_static_subtype(imprecise_fallback: bool) -> bool" & ASCII.LF &
      "    property name_matches(n: Name) -> bool" & ASCII.LF &
      "    property relative_name() -> SingleTokNode" & ASCII.LF &
      "    property is_write_reference(imprecise_fallback: bool) -> bool" & ASCII.LF &
      "    property is_dispatching_call(imprecise_fallback: bool) -> bool" & ASCII.LF &
      "    property is_static_call(imprecise_fallback: bool) -> bool" & ASCII.LF &
      "    property as_symbol_array() -> List<Symbol>" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode AttributeRef: Name {" & ASCII.LF &
      "    field prefix() -> Name" & ASCII.LF &
      "    field attribute() -> Identifier" & ASCII.LF &
      "    field args() -> AdaNode" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode UpdateAttributeRef: AttributeRef {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode CallExpr: Name {" & ASCII.LF &
      "    field name() -> Name" & ASCII.LF &
      "    field suffix() -> AdaNode" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode DefiningName: Name {" & ASCII.LF &
      "    field name() -> Name" & ASCII.LF &
      "" & ASCII.LF &
      "    property basic_decl() -> BasicDecl" & ASCII.LF &
      "    property find_all_refs_in(x: AdaNode, origin: AdaNode, imprecise_fallback: bool) -> List<BaseId>" & ASCII.LF &
      "    property find_all_references(units: List<AnalysisUnit>, imprecise_fallback: bool) -> List<BaseId>" & ASCII.LF &
      "    property is_called_by(units: List<AnalysisUnit>, imprecise_fallback: bool) -> List<BaseId>" & ASCII.LF &
      "    property next_part() -> DefiningName" & ASCII.LF &
      "    property previous_part() -> DefiningName" & ASCII.LF &
      "    property canonical_part() -> DefiningName" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode DiscreteSubtypeName: Name {" & ASCII.LF &
      "    field subtype() -> DiscreteSubtypeIndication" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode DottedName: Name {" & ASCII.LF &
      "    field prefix() -> Name" & ASCII.LF &
      "    field suffix() -> BaseId" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode EndName: Name {" & ASCII.LF &
      "    field name() -> Name" & ASCII.LF &
      "" & ASCII.LF &
      "    property basic_decl() -> BasicDecl" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode ExplicitDeref: Name {" & ASCII.LF &
      "    field prefix() -> Name" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode QualExpr: Name {" & ASCII.LF &
      "    field prefix() -> Name" & ASCII.LF &
      "    field suffix() -> Expr" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode SingleTokNode: Name {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode BaseId: SingleTokNode {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode CharLiteral: BaseId {" & ASCII.LF &
      "" & ASCII.LF &
      "    property denoted_value() -> Character" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode Identifier: BaseId {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode Op: BaseId {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode OpAbs: Op {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode OpAnd: Op {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode OpAndThen: Op {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode OpConcat: Op {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode OpDiv: Op {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode OpDoubleDot: Op {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode OpEq: Op {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode OpGt: Op {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode OpGte: Op {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode OpIn: Op {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode OpLt: Op {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode OpLte: Op {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode OpMinus: Op {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode OpMod: Op {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode OpMult: Op {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode OpNeq: Op {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode OpNot: Op {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode OpNotIn: Op {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode OpOr: Op {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode OpOrElse: Op {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode OpPlus: Op {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode OpPow: Op {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode OpRem: Op {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode OpXor: Op {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode StringLiteral: BaseId {" & ASCII.LF &
      "" & ASCII.LF &
      "    property denoted_value() -> List<Character>" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode NullLiteral: SingleTokNode {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode NumLiteral: SingleTokNode {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode IntLiteral: NumLiteral {" & ASCII.LF &
      "" & ASCII.LF &
      "    property denoted_value() -> BigInt" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode RealLiteral: NumLiteral {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode TargetName: Name {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode ParenExpr: Expr {" & ASCII.LF &
      "    field expr() -> Expr" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode QuantifiedExpr: Expr {" & ASCII.LF &
      "    field quantifier() -> Quantifier" & ASCII.LF &
      "    field loop_spec() -> ForLoopSpec" & ASCII.LF &
      "    field expr() -> Expr" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode RaiseExpr: Expr {" & ASCII.LF &
      "    field exception_name() -> Name" & ASCII.LF &
      "    field error_message() -> Expr" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode UnOp: Expr {" & ASCII.LF &
      "    field op() -> Op" & ASCII.LF &
      "    field expr() -> Expr" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode HandledStmts: AdaNode {" & ASCII.LF &
      "    field stmts() -> StmtList" & ASCII.LF &
      "    field exceptions() -> AdaNode.list" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode InterfaceKind: AdaNode {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode InterfaceKindLimited: InterfaceKind {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode InterfaceKindProtected: InterfaceKind {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode InterfaceKindSynchronized: InterfaceKind {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode InterfaceKindTask: InterfaceKind {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode IterType: AdaNode {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode IterTypeIn: IterType {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode IterTypeOf: IterType {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode LibraryItem: AdaNode {" & ASCII.LF &
      "    field has_private() -> Private" & ASCII.LF &
      "    field item() -> BasicDecl" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode Limited: AdaNode {" & ASCII.LF &
      "" & ASCII.LF &
      "    property as_bool() -> bool" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode LimitedAbsent: Limited {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode LimitedPresent: Limited {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode LoopSpec: AdaNode {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode ForLoopSpec: LoopSpec {" & ASCII.LF &
      "    field var_decl() -> ForLoopVarDecl" & ASCII.LF &
      "    field loop_type() -> IterType" & ASCII.LF &
      "    field has_reverse() -> Reverse" & ASCII.LF &
      "    field iter_expr() -> AdaNode" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode WhileLoopSpec: LoopSpec {" & ASCII.LF &
      "    field expr() -> Expr" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode Mode: AdaNode {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode ModeDefault: Mode {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode ModeIn: Mode {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode ModeInOut: Mode {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode ModeOut: Mode {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode NotNull: AdaNode {" & ASCII.LF &
      "" & ASCII.LF &
      "    property as_bool() -> bool" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode NotNullAbsent: NotNull {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode NotNullPresent: NotNull {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode NullComponentDecl: AdaNode {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode OthersDesignator: AdaNode {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode Overriding: AdaNode {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode OverridingNotOverriding: Overriding {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode OverridingOverriding: Overriding {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode OverridingUnspecified: Overriding {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode Params: AdaNode {" & ASCII.LF &
      "    field params() -> ParamSpec.list" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode Pragma: AdaNode {" & ASCII.LF &
      "    field id() -> Identifier" & ASCII.LF &
      "    field args() -> BaseAssoc.list" & ASCII.LF &
      "" & ASCII.LF &
      "    property associated_decls() -> List<BasicDecl>" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode PrimTypeAccessor: AdaNode {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode Private: AdaNode {" & ASCII.LF &
      "" & ASCII.LF &
      "    property as_bool() -> bool" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode PrivateAbsent: Private {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode PrivatePresent: Private {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode ProtectedDef: AdaNode {" & ASCII.LF &
      "    field public_part() -> PublicPart" & ASCII.LF &
      "    field private_part() -> PrivatePart" & ASCII.LF &
      "    field end_name() -> EndName" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode Protected: AdaNode {" & ASCII.LF &
      "" & ASCII.LF &
      "    property as_bool() -> bool" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode ProtectedAbsent: Protected {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode ProtectedPresent: Protected {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode Quantifier: AdaNode {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode QuantifierAll: Quantifier {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode QuantifierSome: Quantifier {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode RangeSpec: AdaNode {" & ASCII.LF &
      "    field range() -> Expr" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode RenamingClause: AdaNode {" & ASCII.LF &
      "    field renamed_object() -> Name" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode Reverse: AdaNode {" & ASCII.LF &
      "" & ASCII.LF &
      "    property as_bool() -> bool" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode ReverseAbsent: Reverse {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode ReversePresent: Reverse {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode SelectWhenPart: AdaNode {" & ASCII.LF &
      "    field cond_expr() -> Expr" & ASCII.LF &
      "    field stmts() -> StmtList" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode Stmt: AdaNode {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode CompositeStmt: Stmt {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode AcceptStmt: CompositeStmt {" & ASCII.LF &
      "    field name() -> Identifier" & ASCII.LF &
      "    field entry_index_expr() -> Expr" & ASCII.LF &
      "    field params() -> Params" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode AcceptStmtWithStmts: AcceptStmt {" & ASCII.LF &
      "    field stmts() -> HandledStmts" & ASCII.LF &
      "    field end_name() -> EndName" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode BaseLoopStmt: CompositeStmt {" & ASCII.LF &
      "    field spec() -> LoopSpec" & ASCII.LF &
      "    field stmts() -> StmtList" & ASCII.LF &
      "    field end_name() -> EndName" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode ForLoopStmt: BaseLoopStmt {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode LoopStmt: BaseLoopStmt {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode WhileLoopStmt: BaseLoopStmt {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode BlockStmt: CompositeStmt {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode BeginBlock: BlockStmt {" & ASCII.LF &
      "    field stmts() -> HandledStmts" & ASCII.LF &
      "    field end_name() -> EndName" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode DeclBlock: BlockStmt {" & ASCII.LF &
      "    field decls() -> DeclarativePart" & ASCII.LF &
      "    field stmts() -> HandledStmts" & ASCII.LF &
      "    field end_name() -> EndName" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode CaseStmt: CompositeStmt {" & ASCII.LF &
      "    field expr() -> Expr" & ASCII.LF &
      "    field alternatives() -> CaseStmtAlternative.list" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode ExtendedReturnStmt: CompositeStmt {" & ASCII.LF &
      "    field decl() -> ExtendedReturnStmtObjectDecl" & ASCII.LF &
      "    field stmts() -> HandledStmts" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode IfStmt: CompositeStmt {" & ASCII.LF &
      "    field cond_expr() -> Expr" & ASCII.LF &
      "    field then_stmts() -> StmtList" & ASCII.LF &
      "    field alternatives() -> ElsifStmtPart.list" & ASCII.LF &
      "    field else_stmts() -> StmtList" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode NamedStmt: CompositeStmt {" & ASCII.LF &
      "    field decl() -> NamedStmtDecl" & ASCII.LF &
      "    field stmt() -> CompositeStmt" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode SelectStmt: CompositeStmt {" & ASCII.LF &
      "    field guards() -> SelectWhenPart.list" & ASCII.LF &
      "    field else_stmts() -> StmtList" & ASCII.LF &
      "    field abort_stmts() -> StmtList" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode ErrorStmt: Stmt {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode SimpleStmt: Stmt {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode AbortStmt: SimpleStmt {" & ASCII.LF &
      "    field names() -> Name.list" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode AssignStmt: SimpleStmt {" & ASCII.LF &
      "    field dest() -> Name" & ASCII.LF &
      "    field expr() -> Expr" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode CallStmt: SimpleStmt {" & ASCII.LF &
      "    field call() -> Name" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode DelayStmt: SimpleStmt {" & ASCII.LF &
      "    field has_until() -> Until" & ASCII.LF &
      "    field expr() -> Expr" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode ExitStmt: SimpleStmt {" & ASCII.LF &
      "    field loop_name() -> Identifier" & ASCII.LF &
      "    field cond_expr() -> Expr" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode GotoStmt: SimpleStmt {" & ASCII.LF &
      "    field label_name() -> Name" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode Label: SimpleStmt {" & ASCII.LF &
      "    field decl() -> LabelDecl" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode NullStmt: SimpleStmt {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode RaiseStmt: SimpleStmt {" & ASCII.LF &
      "    field exception_name() -> Name" & ASCII.LF &
      "    field error_message() -> Expr" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode RequeueStmt: SimpleStmt {" & ASCII.LF &
      "    field call_name() -> Expr" & ASCII.LF &
      "    field has_abort() -> Abort" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode ReturnStmt: SimpleStmt {" & ASCII.LF &
      "    field return_expr() -> Expr" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode TerminateAlternative: SimpleStmt {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode SubpKind: AdaNode {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode SubpKindFunction: SubpKind {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode SubpKindProcedure: SubpKind {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode Subunit: AdaNode {" & ASCII.LF &
      "    field name() -> Name" & ASCII.LF &
      "    field body() -> Body" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode Synchronized: AdaNode {" & ASCII.LF &
      "" & ASCII.LF &
      "    property as_bool() -> bool" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode SynchronizedAbsent: Synchronized {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode SynchronizedPresent: Synchronized {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode Tagged: AdaNode {" & ASCII.LF &
      "" & ASCII.LF &
      "    property as_bool() -> bool" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode TaggedAbsent: Tagged {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode TaggedPresent: Tagged {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode TaskDef: AdaNode {" & ASCII.LF &
      "    field interfaces() -> ParentList" & ASCII.LF &
      "    field public_part() -> PublicPart" & ASCII.LF &
      "    field private_part() -> PrivatePart" & ASCII.LF &
      "    field end_name() -> EndName" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode TypeDef: AdaNode {" & ASCII.LF &
      "" & ASCII.LF &
      "    property is_tagged_type() -> bool" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode AccessDef: TypeDef {" & ASCII.LF &
      "    field has_not_null() -> NotNull" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode AccessToSubpDef: AccessDef {" & ASCII.LF &
      "    field has_protected() -> Protected" & ASCII.LF &
      "    field subp_spec() -> SubpSpec" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode BaseTypeAccessDef: AccessDef {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode AnonymousTypeAccessDef: BaseTypeAccessDef {" & ASCII.LF &
      "    field type_decl() -> BaseTypeDecl" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode TypeAccessDef: BaseTypeAccessDef {" & ASCII.LF &
      "    field has_all() -> All" & ASCII.LF &
      "    field has_constant() -> Constant" & ASCII.LF &
      "    field subtype_indication() -> SubtypeIndication" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode ArrayTypeDef: TypeDef {" & ASCII.LF &
      "    field indices() -> ArrayIndices" & ASCII.LF &
      "    field component_type() -> ComponentDef" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode DerivedTypeDef: TypeDef {" & ASCII.LF &
      "    field has_abstract() -> Abstract" & ASCII.LF &
      "    field has_limited() -> Limited" & ASCII.LF &
      "    field has_synchronized() -> Synchronized" & ASCII.LF &
      "    field subtype_indication() -> SubtypeIndication" & ASCII.LF &
      "    field interfaces() -> ParentList" & ASCII.LF &
      "    field record_extension() -> BaseRecordDef" & ASCII.LF &
      "    field has_with_private() -> WithPrivate" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode EnumTypeDef: TypeDef {" & ASCII.LF &
      "    field enum_literals() -> EnumLiteralDecl.list" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode FormalDiscreteTypeDef: TypeDef {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode InterfaceTypeDef: TypeDef {" & ASCII.LF &
      "    field interface_kind() -> InterfaceKind" & ASCII.LF &
      "    field interfaces() -> ParentList" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode ModIntTypeDef: TypeDef {" & ASCII.LF &
      "    field expr() -> Expr" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode PrivateTypeDef: TypeDef {" & ASCII.LF &
      "    field has_abstract() -> Abstract" & ASCII.LF &
      "    field has_tagged() -> Tagged" & ASCII.LF &
      "    field has_limited() -> Limited" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode RealTypeDef: TypeDef {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode DecimalFixedPointDef: RealTypeDef {" & ASCII.LF &
      "    field delta() -> Expr" & ASCII.LF &
      "    field digits() -> Expr" & ASCII.LF &
      "    field range() -> RangeSpec" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode FloatingPointDef: RealTypeDef {" & ASCII.LF &
      "    field num_digits() -> Expr" & ASCII.LF &
      "    field range() -> RangeSpec" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode OrdinaryFixedPointDef: RealTypeDef {" & ASCII.LF &
      "    field delta() -> Expr" & ASCII.LF &
      "    field range() -> RangeSpec" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode RecordTypeDef: TypeDef {" & ASCII.LF &
      "    field has_abstract() -> Abstract" & ASCII.LF &
      "    field has_tagged() -> Tagged" & ASCII.LF &
      "    field has_limited() -> Limited" & ASCII.LF &
      "    field record_def() -> BaseRecordDef" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode SignedIntTypeDef: TypeDef {" & ASCII.LF &
      "    field range() -> RangeSpec" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode TypeExpr: AdaNode {" & ASCII.LF &
      "" & ASCII.LF &
      "    property type_name() -> Name" & ASCII.LF &
      "    property designated_type_decl() -> BaseTypeDecl" & ASCII.LF &
      "    property designated_type_decl_from(origin_node: AdaNode) -> BaseTypeDecl" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode AnonymousType: TypeExpr {" & ASCII.LF &
      "    field type_decl() -> AnonymousTypeDecl" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode EnumLitSynthTypeExpr: TypeExpr {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode SubtypeIndication: TypeExpr {" & ASCII.LF &
      "    field has_not_null() -> NotNull" & ASCII.LF &
      "    field name() -> Name" & ASCII.LF &
      "    field constraint() -> Constraint" & ASCII.LF &
      "" & ASCII.LF &
      "    property is_static_subtype(imprecise_fallback: bool) -> bool" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode ConstrainedSubtypeIndication: SubtypeIndication {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode DiscreteSubtypeIndication: SubtypeIndication {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode UnconstrainedArrayIndex: AdaNode {" & ASCII.LF &
      "    field subtype_indication() -> SubtypeIndication" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode Until: AdaNode {" & ASCII.LF &
      "" & ASCII.LF &
      "    property as_bool() -> bool" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode UntilAbsent: Until {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode UntilPresent: Until {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode UseClause: AdaNode {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode UsePackageClause: UseClause {" & ASCII.LF &
      "    field packages() -> Name.list" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode UseTypeClause: UseClause {" & ASCII.LF &
      "    field has_all() -> All" & ASCII.LF &
      "    field types() -> Name.list" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode Variant: AdaNode {" & ASCII.LF &
      "    field choices() -> AlternativesList" & ASCII.LF &
      "    field components() -> ComponentList" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode VariantPart: AdaNode {" & ASCII.LF &
      "    field discr_name() -> Identifier" & ASCII.LF &
      "    field variant() -> Variant.list" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode WithClause: AdaNode {" & ASCII.LF &
      "    field has_limited() -> Limited" & ASCII.LF &
      "    field has_private() -> Private" & ASCII.LF &
      "    field packages() -> Name.list" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode WithPrivate: AdaNode {" & ASCII.LF &
      "" & ASCII.LF &
      "    property as_bool() -> bool" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode WithPrivateAbsent: WithPrivate {" & ASCII.LF &
      "" & ASCII.LF &
      "}" & ASCII.LF &
      "" & ASCII.LF &
      "astnode WithPrivatePresent: WithPrivate {" & ASCII.LF &
      "" & ASCII.LF &
      "}");

   -----------------
   -- Is_Built_In --
   -----------------

   function Is_Built_In (Name : Text_Type) return Boolean is
     (Name = "image" or else Name = "text");

   --------------------
   -- Built_In_Field --
   --------------------

   function Built_In_Field (Receiver      : Ada_AST_Node;
                            Property_Name : Text_Type)
                            return Introspection_Value
   is
   begin
      if Property_Name = "image" then
         return Make_Introspection_Value (Receiver.Node.Text_Image);
      elsif Property_Name = "text" then
         return Make_Introspection_Value (Receiver.Node.Text);
      end if;

      raise Introspection_Error with
        "Invalid built-in property: " & To_UTF8 (Property_Name);
   end Built_In_Field;

   ------------------------------
   -- Make_Introspection_Value --
   ------------------------------

   function Make_Introspection_Value
     (Value : Text_Type) return Introspection_Value
   is
     (Kind => Kind_Text, Text_Val => To_Unbounded_Text (Value));

   ------------------------------
   -- Make_Introspection_Value --
   ------------------------------

   function Make_Introspection_Value
     (Value : Unbounded_Text_Type_Array) return Introspection_Value
   is
      Result : constant Unbounded_Text_Array_Access :=
        new Unbounded_Text_Array (1 .. Value'Length);
   begin
      for I in Value'Range loop
         Result (I) := Value (I);
      end loop;

      return (Kind => Kind_Text_Array, Text_Array_Val => Result);
   end Make_Introspection_Value;

   ------------------------------
   -- Make_Introspection_Value --
   ------------------------------

   function Make_Introspection_Value
     (Value : Value_Type) return Introspection_Value
   is
   begin
      case Kind (Value) is
         when Boolean_Value =>
            return (Kind => Kind_Bool, Bool_Val => As_Boolean (Value));
         when Integer_Value =>
            return (Kind => Kind_Int, Int_Val => As_Integer (Value));
         when Node_Value =>
            return (Kind     => Kind_Node,
                    Node_Val => new Ada_AST_Node'(Node => As_Node (Value)));
         when Text_Type_Value =>
            return (Kind     => Kind_Text,
                    Text_Val => To_Unbounded_Text (As_Text_Type (Value)));
         when Unbounded_Text_Value =>
            return (Kind => Kind_Text, Text_Val => As_Unbounded_Text (Value));
         when Unbounded_Text_Type_Array_Value =>
            return Make_Introspection_Value
              (As_Unbounded_Text_Type_Array (Value));
         when Ada_Node_Array_Value =>
            return Make_Introspection_Value (As_Ada_Node_Array (Value));
         when Basic_Decl_Array_Value =>
            return Make_Introspection_Value (As_Basic_Decl_Array (Value));
         when Defining_Name_Array_Value =>
            return Make_Introspection_Value (As_Defining_Name_Array (Value));
         when Base_Formal_Param_Decl_Array_Value =>
            return Make_Introspection_Value
              (As_Base_Formal_Param_Decl_Array (Value));
         when Generic_Instantiation_Array_Value =>
            return Make_Introspection_Value
              (As_Generic_Instantiation_Array (Value));
         when others =>
            raise Introspection_Error with
              "Unsupported value type from the introspection API: " &
                 Value_Kind'Image (Kind (Value));
      end case;
   end Make_Introspection_Value;

   ----------------------
   -- Get_Property_Ref --
   ----------------------

   function Get_Property_Ref (Node          : Ada_AST_Node;
                              Property_Name : Text_Type)
                              return Property_Reference
   is
      Ref : constant Any_Node_Data_Reference :=
        Data_Reference_For_Name (Node, Property_Name);
   begin
      if not (Ref in Property_Reference) then
         raise Introspection_Error with "No property named: "
           & To_UTF8 (Property_Name);
      end if;

      return Property_Reference (Ref);
   end Get_Property_Ref;

   ---------------------
   -- Make_Value_Type --
   ---------------------

   function Make_Value_Type (Value       : Introspection_Value;
                             Target_Kind : Value_Kind)
                             return Value_Type
   is
   begin
      if Value.Kind = Kind_Node_Array then
         return Array_To_Value_Type (Value.Node_Array_Val.all, Target_Kind);
      elsif Value.Kind = Kind_Text then
         return String_To_Value_Type (Value.Text_Val, Target_Kind);
      elsif Value.Kind = Kind_Text_Array then
         declare
            Result : Unbounded_Text_Type_Array
              (1 .. Value.Text_Array_Val'Length);
         begin
            for I in Result'Range loop
               Result (I) := Value.Text_Array_Val (I);
            end loop;

            return Create_Unbounded_Text_Type_Array (Result);
         end;
      elsif Value.Kind = Kind_Int and then Target_Kind = Integer_Value then
         return Create_Integer (Value.Int_Val);
      elsif Value.Kind = Kind_Bool and then Target_Kind = Boolean_Value then
         return Create_Boolean (Value.Bool_Val);
      elsif Value.Kind = Kind_Node and then Target_Kind = Node_Value then
         return Create_Node
           (Introspection_Node_To_Ada_Node (Value.Node_Val).Node);
      end if;

      raise Introspection_Error with "Cannot convert a " &
        Introspection_Value_Kind'Image (Value.Kind) & " to a " &
        Value_Kind'Image (Target_Kind);
   end Make_Value_Type;

   -------------------------
   -- Array_To_Value_Type --
   -------------------------

   function Array_To_Value_Type (Value       : AST_Node_Array;
                                 Target_Kind : Value_Kind)
                                 return Value_Type
   is
   begin
      case Target_Kind is
         when Ada_Node_Array_Value =>
            return Create_Ada_Node_Array
              (Ada_Node_Array_From_AST_Array (Value));
         when Base_Formal_Param_Decl_Array_Value =>
            return Create_Base_Formal_Param_Decl_Array
              (Base_Formal_Param_Decl_Array_From_AST_Array (Value));
         when Basic_Decl_Array_Value =>
            return Create_Basic_Decl_Array
              (Basic_Decl_Array_From_AST_Array (Value));
         when Defining_Name_Array_Value =>
            return Create_Defining_Name_Array
              (Defining_Name_Array_From_AST_Array (Value));
         when Generic_Instantiation_Array_Value =>
            return Create_Generic_Instantiation_Array
              (Generic_Instantiation_Array_From_AST_Array (Value));
         when Param_Spec_Array_Value =>
            return Create_Param_Spec_Array
              (Param_Spec_Array_From_AST_Array (Value));
         when others =>
            raise Introspection_Error with "Cannot create Value_Type of kind" &
              Value_Kind'Image (Target_Kind) & " from an introspection " &
              "value array";
      end case;
   end Array_To_Value_Type;

   --------------------------
   -- String_To_Value_Type --
   --------------------------

   function String_To_Value_Type (Value       : Unbounded_Text_Type;
                                  Target_Kind : Value_Kind)
                                  return Value_Type
   is
   begin
      if Target_Kind = Unbounded_Text_Value then
         return Create_Unbounded_Text (Value);
      elsif Target_Kind = Text_Type_Value then
         return Create_Text_Type (To_Text (Value));
      elsif Target_Kind = Character_Value and then Length (Value) = 1 then
         return Create_Character (Element (Value, 1));
      end if;

      raise Introspection_Error with "Cannot create a value of kind" &
        Value_Kind'Image (Target_Kind) & " from an unbounded String";
   end String_To_Value_Type;

   -----------------------
   -- Matches_Kind_Name --
   -----------------------

   overriding function Matches_Kind_Name
     (Node : Ada_AST_Node; Kind_Name : String) return Boolean
   is
      Expected_Kind : constant Any_Node_Type_Id :=
        Lookup_DSL_Name (Kind_Name);
      Actual_Kind   : constant Any_Node_Type_Id :=
        Id_For_Kind (Node.Node.Kind);
   begin
      if Expected_Kind = None then
         raise Assertion_Error with "Invalid kind name: " & Kind_Name;
      end if;

      return Actual_Kind = Expected_Kind or else
             Is_Derived_From (Actual_Kind, Expected_Kind);
   end Matches_Kind_Name;


   -------------------
   -- Is_Field_Name --
   -------------------

   overriding function Is_Field_Name
     (Node : Ada_AST_Node; Name : Text_Type) return Boolean
   is
      Data_Ref : constant Any_Node_Data_Reference :=
        Data_Reference_For_Name (Node, Name);
   begin
      return (Data_Ref in Field_Reference) or else
             (Data_Ref in Built_In_LAL_Field) or else
              Is_Built_In (Name);
   end Is_Field_Name;

   ----------------------
   -- Is_Property_Name --
   ----------------------

   function Is_Property_Name
     (Node : Ada_AST_Node; Name : Text_Type) return Boolean
   is
     (Data_Reference_For_Name (Node, Name) in Property_Reference);

   ------------------
   -- Access_Field --
   ------------------

   overriding function Access_Field
     (Node : Ada_AST_Node; Field : Text_Type) return Introspection_Value
   is
      Data_Ref : constant Any_Node_Data_Reference :=
         Data_Reference_For_Name (Node, Field);
   begin
      if Is_Built_In (Field) then
         return Built_In_Field (Node, Field);
      end if;

      return Make_Introspection_Value
        (Eval_Node_Data (Node.Node, Data_Ref, Empty_Value_Array));
   end Access_Field;

   --------------------
   -- Property_Arity --
   --------------------

   overriding function Property_Arity
     (Node : Ada_AST_Node; Property_Name : Text_Type) return Natural
   is
     (Property_Argument_Types
        (Get_Property_Ref (Node, Property_Name))'Length);

   ------------------------
   -- Default_Args_Value --
   ------------------------

   overriding function Default_Arg_Value (Node          : Ada_AST_Node;
                                          Property_Name : Text_Type;
                                          Arg_Position  : Positive)
                                          return Introspection_Value
   is
      (Make_Introspection_Value
         (Property_Argument_Default_Value
              (Get_Property_Ref (Node, Property_Name), Arg_Position)));

   ----------------------
   -- Evalute_Property --
   ----------------------

   function Evaluate_Property
     (Node          : Ada_AST_Node;
      Property_Name : Text_Type;
      Arguments     : Introspection_Value_Array)
      return Introspection_Value
   is
      Property_Args : Value_Array (1 .. Arguments'Length);
      Property_Ref  : constant Property_Reference :=
        Get_Property_Ref (Node, Property_Name);
      Contraints    : constant Value_Constraint_Array :=
        Property_Argument_Types (Property_Ref);
   begin
      if Arguments'Length /= Contraints'Length then
         raise Introspection_Error with "Expected " &
           Positive'Image (Contraints'Length) &  " arguments but got" &
           Positive'Image (Arguments'Length);
      end if;

      for I in Arguments'Range loop
         Property_Args (I) :=
           Make_Value_Type (Arguments (I), Contraints (I).Kind);
      end loop;

      return Make_Introspection_Value
        (Eval_Node_Data (Node.Node, Property_Ref, Property_Args));

   end Evaluate_Property;

end Ada_AST_Node;

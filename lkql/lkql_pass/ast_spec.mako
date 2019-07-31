with LKQL.AST_Nodes; use LKQL.AST_Nodes;

with ${lib_name}.Analysis; use ${lib_name}.Analysis;

with Langkit_Support.Text; use Langkit_Support.Text;

with Ada.Containers; use Ada.Containers;

package ${lang_name}_AST_Nodes is

   type ${lang_name}_AST_Node is new AST_Node with record
      Node : ${root_type};
   end record;

   type ${lang_name}_AST_Node_Access is access all ${lang_name}_AST_Node;

   overriding function Node_Prototypes (Node : ${lang_name}_AST_Node) return String;

   overriding function "=" (Left, Right : ${lang_name}_AST_Node) return Boolean is
     (Left.Node = Right.Node);

   overriding function Hash (Node : ${lang_name}_AST_Node) return Hash_Type is
     (Hash (Node.Node));

   overriding function Text_Image (Node : ${lang_name}_AST_Node) return Text_Type is
      (Node.Node.Text_Image);

   overriding function Kind_Name (Node : ${lang_name}_AST_Node) return String is
     (Kind_Name (Node.Node));

   overriding function Is_Null_Node (Node : ${lang_name}_AST_Node) return Boolean is
     (Node.Node.Is_Null);

   overriding function Children_Count (Node : ${lang_name}_AST_Node) return Natural is
     (Node.Node.Children_Count);

   overriding function Nth_Child
     (Node : ${lang_name}_AST_Node; N : Positive) return ${lang_name}_AST_Node
   is
     (${lang_name}_AST_Node'(Node => Node.Node.Child (N)));

   overriding function Matches_Kind_Name
     (Node : ${lang_name}_AST_Node; Kind_Name : String) return Boolean;

   overriding function Is_Field_Name
     (Node : ${lang_name}_AST_Node; Name : Text_Type) return Boolean;

   overriding function Is_Property_Name
     (Node : ${lang_name}_AST_Node; Name : Text_Type) return Boolean;

   overriding function Access_Field
     (Node : ${lang_name}_AST_Node; Field : Text_Type) return Introspection_Value;

   overriding function Property_Arity
     (Node : ${lang_name}_AST_Node; Property_Name : Text_Type) return Natural;

   overriding function Default_Arg_Value (Node          : ${lang_name}_AST_Node;
                                          Property_Name : Text_Type;
                                          Arg_Position  : Positive)
                                          return Introspection_Value;

   function Evaluate_Property
     (Node          : ${lang_name}_AST_Node;
      Property_Name : Text_Type;
      Arguments     : Introspection_Value_Array)
      return Introspection_Value;

   function Make_${lang_name}_AST_Node (Node : ${root_type}) return AST_Node_Rc
     is (Make_AST_Node_Rc (${lang_name}_AST_Node'(Node => Node)));

end ${lang_name}_AST_Nodes;
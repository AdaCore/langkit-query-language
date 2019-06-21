with LKQL.AST_Nodes; use LKQL.AST_Nodes;

with ${lib_name}.Analysis; use ${lib_name}.Analysis;

with Langkit_Support.Text; use Langkit_Support.Text;

with Ada.Containers; use Ada.Containers;

package Ext_AST_Nodes is

   type Ext_AST_Node is new AST_Node with record
      Node : ${root_type};
   end record;

   type Ext_AST_Node_Access is access all Ext_AST_Node;

   overriding function "=" (Left, Right : Ext_AST_Node) return Boolean is
     (Left.Node = Right.Node);

   overriding function Hash (Node : Ext_AST_Node) return Hash_Type is
     (Hash (Node.Node));

   overriding function Text_Image (Node : Ext_AST_Node) return Text_Type is
      (Node.Node.Text_Image);

   overriding function Kind_Name (Node : Ext_AST_Node) return String is
     (Kind_Name (Node.Node));

   overriding function Is_Null_Node (Node : Ext_AST_Node) return Boolean is
     (Node.Node.Is_Null);

   overriding function Children_Count (Node : Ext_AST_Node) return Natural is
     (Node.Node.Children_Count);

   overriding function Nth_Child
     (Node : Ext_AST_Node; N : Positive) return Ext_AST_Node
   is
     (Ext_AST_Node'(Node => Node.Node.Child (N)));

   overriding function Matches_Kind_Name
     (Node : Ext_AST_Node; Kind_Name : String) return Boolean;

   overriding function Is_Field_Name
     (Node : Ext_AST_Node; Name : Text_Type) return Boolean;

   overriding function Is_Property_Name
     (Node : Ext_AST_Node; Name : Text_Type) return Boolean;

   overriding function Access_Field
     (Node : Ext_AST_Node; Field : Text_Type) return Introspection_Value;

   overriding function Property_Arity
     (Node : Ext_AST_Node; Property_Name : Text_Type) return Natural;

   overriding function Default_Arg_Value (Node          : Ext_AST_Node;
                                          Property_Name : Text_Type;
                                          Arg_Position  : Positive)
                                          return Introspection_Value;

   function Evaluate_Property
     (Node          : Ext_AST_Node;
      Property_Name : Text_Type;
      Arguments     : Introspection_Value_Array)
      return Introspection_Value;

   function Make_Ext_AST_Node (Node : ${root_type}) return AST_Node_Rc
     is (Make_AST_Node_Rc (Ext_AST_Node'(Node => Node)));

end Ext_AST_Nodes;

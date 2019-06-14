with LKQL.AST_Nodes; use LKQL.AST_Nodes;

with Libadalang.Analysis; use Libadalang.Analysis;

with Langkit_Support.Text; use Langkit_Support.Text;

with Ada.Containers; use Ada.Containers;

package Ada_AST_Node is

   type Ada_AST_Node is new AST_Node with record
      Node : Ada_Node;
   end record;

   overriding function "=" (Left, Right : Ada_AST_Node) return Boolean is
     (Left.Node = Right.Node);

   overriding function Hash (Node : Ada_AST_Node) return Hash_Type is
     (Hash (Node.Node));

   overriding function Text_Image (Node : Ada_AST_Node) return Text_Type is
      (Node.Node.Text_Image);

   overriding function Kind_Name (Node : Ada_AST_Node) return String is
     (Kind_Name (Node.Node));

   overriding function Is_Null (Node : Ada_AST_Node) return Boolean is
     (Node.Node.Is_Null);

   overriding function Children_Count (Node : Ada_AST_Node) return Natural is
     (Node.Node.Children_Count);

   overriding function Nth_Child
     (Node : Ada_AST_Node; N : Positive) return Ada_AST_Node
   is
     (Ada_AST_Node'(Node => Node.Node.Child (N)));

   overriding function Matches_Kind_Name
     (Node : Ada_AST_Node; Kind_Name : String) return Boolean;

   overriding function Is_Field_Name
     (Node : Ada_AST_Node; Name : Text_Type) return Boolean;

   overriding function Is_Property_Name
     (Node : Ada_AST_Node; Name : Text_Type) return Boolean;

   overriding function Access_Field
     (Node : Ada_AST_Node; Field : Text_Type) return Primitive;

   function Make_Ada_AST_Node (Node : Ada_Node) return AST_Node_Rc
     is (Make_AST_Node_Rc (Ada_AST_Node'(Node => Node)));

end Ada_AST_Node;

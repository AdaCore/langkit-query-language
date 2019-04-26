with Ada.Containers.Vectors;

with Libadalang.Analysis;

with Langkit_Support.Text; use Langkit_Support.Text;
with String_Utils;         use String_Utils;

--  Stores the result of a rule's execution
package Rule_Results is

   package LAL renames Libadalang.Analysis;

   package Ada_Node_vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Libadalang.Analysis.Ada_Node,
      "="          => Libadalang.Analysis."=");

   subtype Ada_Node_Vector is Ada_Node_vectors.Vector;
   --  Vector of Ada_Node values.

   type Rule_Result is tagged private;
   --  Record storing the result of a rule's execution

   function To_Unbounded_Text (Self : Rule_Result) return Unbounded_Text_Type;
   --  Return a pretty-printed String representing a Rule_Result value

   procedure Display (Self : Rule_Result);
   --  Print the result of 'To_Unbounded_Text'

   function Make_Rule_Result (Rule_Name     : Unbounded_Text_Type;
                              Flagged_Nodes : Ada_Node_Vector)
                              return Rule_Result;
   --  Create a Rule_Result value with the given name and list of flagged nodes

private

   type Rule_Result is tagged record
      Rule_Name     : Unbounded_Text_Type;
      --  Name of the rule
      Messages      : String_Vector;
      --  vector of messages representing the nodes flagged by the rule's
      --  execution.
   end record;

end Rule_Results;

with Ada.Containers.Vectors;
with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;

with Project_Utils; use Project_Utils;

with LKQL.Primitives;    use LKQL.Primitives;
with LKQL.Eval_Contexts; use LKQL.Eval_Contexts;

with Liblkqllang.Analysis;

with Libadalang.Analysis;

with Langkit_Support.Text; use Langkit_Support.Text;

--  A diagnostic is composed of a collection of individual rule commands
package Rule_Commands is

   package L renames Liblkqllang.Analysis;
   package LAL renames Libadalang.Analysis;

   package Message_Vectors is new Ada.Containers.Vectors
     (Positive, Unbounded_Text_Type);

   Rule_Error : exception;

   type Rule_Command is tagged record
      Name          : Unbounded_Text_Type;
      --  Name of the Rule
      Code          : Unbounded_Text_Type;
      --  Code for the rule
      LKQL_Root     : L.LKQL_Node;
      --  Root of the LKQL AST
      LKQL_Context  : L.Analysis_Context;
      --  Analysis context that was used to create the LKQL AST
   end record;

   function Evaluate
     (Self : Rule_Command;
      Unit : LAL.Analysis_Unit)
      return Message_Vectors.Vector;
   --  Execute the LKQL script of the rule and return a Rule_Result value
   --  containing the flagged nodes.

   function Create_Rule_Command
     (Name                : Text_Type;
      LKQL_Script_Path    : String;
      Code                : Text_Type := "result()")
      return Rule_Command;
   --  Create a Rule_Command value with the given name and arguments

   procedure Check_Kind
     (Expected_Kind : Valid_Primitive_Kind; Actual_Kind : Valid_Primitive_Kind;
      Context       : String);
   --  Raise a Rule_error if 'Expected_Kind' is different from 'Actual_Kind'.
   --  The error message will start with the context String.

end Rule_Commands;

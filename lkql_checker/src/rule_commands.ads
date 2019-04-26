with Rule_Results;  use Rule_Results;
with Project_Utils; use Project_Utils;

with Interpreter.Primitives;    use Interpreter.Primitives;
with Interpreter.Eval_Contexts; use Interpreter.Eval_Contexts;

with Liblkqllang.Analysis;

with Libadalang.Analysis;

with Langkit_Support.Text; use Langkit_Support.Text;

--  A diagnostic is composed of a collection of individual rule commands
package Rule_Commands is

   package L renames Liblkqllang.Analysis;
   package LAL renames Libadalang.Analysis;

   Rule_Error : exception;

   type Rule_Command is tagged private;

   function Evaluate (Self      : Rule_Command;
                      Ada_Units : Ada_Unit_Vectors.Vector)
                      return Rule_Result;
   --  Execute the LKQL script of the rule and return a Rule_Result value
   --  containing the flagged nodes.

   function Create_Rule_Command
     (Name                : Text_Type;
      LKQL_Script_Path    : String;
      Function_Name       : Text_Type := "result";
      Arguments           : Environment_Map := String_Value_Maps.Empty_Map)
      return Rule_Command;
   --  Create a Rule_Command value with the given name and arguments

   procedure Check_Kind
     (Expected_Kind : Valid_Primitive_Kind; Actual_Kind : Valid_Primitive_Kind;
      Context       : String);
   --  Raise a Rule_error if 'Expected_Kind' is different from 'Actual_Kind'.
   --  The error message will start with the context String.

private

   type Rule_Command is tagged record
      Name          : Unbounded_Text_Type;
      --  Name of the Rule
      Function_Name : Unbounded_Text_Type;
      --  Name of the LKQL function that will be called to retrive the flagged
      --  nodes.
      Arguments     : Environment_Map;
      --  Arguments to use for the function call
      LKQL_Root     : L.LKQL_Node;
      --  Root of the LKQL AST
      LKQL_Context  : L.Analysis_Context;
      --  Analysis context that was used to create the LKQL AST
   end record;

   function Get_Flagged_Nodes
     (Command : Rule_Command; Ctx : Eval_Context) return Ada_Node_Vector;
   --  Return a vector containing the Ada_Node values returned by the rule's
   --  LKQL function

end Rule_Commands;

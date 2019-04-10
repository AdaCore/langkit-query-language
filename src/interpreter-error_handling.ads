with Interpreter.Errors;        use Interpreter.Errors;
with Interpreter.Eval_Contexts; use Interpreter.Eval_Contexts;
with Interpreter.Primitives;    use Interpreter.Primitives;

with Langkit_Support.Text; use Langkit_Support.Text;

package Interpreter.Error_Handling is

   procedure Raise_And_Record_Error
     (Ctx : Eval_Context; Error : Error_Data)
      with No_Return;
   --  Add the given error to the evaluation context and:
   --    * raise a Recoverable_Error if error recovery is enabled and the
   --      user chooses to ignore the error
   --    * raise a Stop_Evaluation_Error if error recovery is disabled or the
   --      user chooses not to ignore the error

   procedure Raise_Invalid_Member (Ctx      : Eval_Context;
                                   Node     : L.Dot_Access;
                                   Receiver : Primitive)
      with No_Return;
   --  Raise an exception signaling an invalid member access, and add an
   --  Error_Data describing the error to the evaluation context.

   procedure Raise_Null_Root (Ctx : Eval_Context; Node : L.Query)
      with No_Return;
   --  Raise an exception signaling the absence of a proper AST root while
   --  trying to execute a query, and add an Error_Data describing the error
   --  to the evaluation context.

   procedure Raise_Invalid_Kind (Ctx      : Eval_Context;
                                 Node     : L.LKQL_Node;
                                 Expected : Valid_Primitive_Kind;
                                 Value    : Primitive)
      with No_Return;
   --  Raise an exception signaling a type error, and add an Error_Data
   --  describing the error to the evaluation context.

   procedure Raise_Invalid_Selector_Name (Ctx  : Eval_Context;
                                          Node : L.Selector_Pattern'Class)
     with No_Return;
   --  Raise an exception signaling the use of an invalid selector name,
   --  and add an Error_Data describing the error to the evaluation context.

   procedure Raise_Unknown_Symbol (Ctx  : Eval_Context;
                                   Node : L.Identifier)
     with No_Return;
   --  Raise an exception signaling the use of an unknwown identifier

   procedure Raise_Already_Existing_Symbol (Ctx        : Eval_Context;
                                            Identifier : Unbounded_Text_Type;
                                            Node       : L.LKQL_Node)
     with No_Return;
   --  Raise an exception signaling an attempt to create a binding using a name
   --  that is already bound to a value in the local context.

   procedure Raise_Invalid_Arity (Ctx            : Eval_Context;
                                  Call           : L.Fun_Call;
                                  Expected_Arity : Positive)
     with No_Return;
   --  Raise an exception signaling an attempt to call a function with an
   --  incorrect number of arguments.

end Interpreter.Error_Handling;

with LKQL.AST_Nodes;     use LKQL.AST_Nodes;
with LKQL.Primitives;    use LKQL.Primitives;
with LKQL.Eval_Contexts; use LKQL.Eval_Contexts;
with LKQL.String_Utils;   use LKQL.String_Utils;

with Langkit_Support.Text; use Langkit_Support.Text;

package LKQL.Node_Data is

   function Access_Node_Field (Ctx        : Eval_Context;
                               Receiver   : AST_Node_Rc;
                               Field_Name : L.Identifier) return Primitive;
   --  Return the value of the field designated by 'Field_Name' on 'Receiver'.
   --  An exception will be raised if there is no such field.

   function Eval_Node_Property (Ctx           : Eval_Context;
                                Receiver      : AST_Node_Rc;
                                Property_Name : L.Identifier;
                                Args          : L.Arg_List) return Primitive
     with Pre => not Args.Is_Null;
   --  Evaluate the property designated by 'Property_Name' on 'Receiver'.
   --  An exception will be raised if there is no such property or if the call
   --  arity doesn't match the arity of the property.

   Builtin_Fields, Builtin_Properties : LKQL.String_Utils.Unbounded_Text_Set;
   --  Builtin fields and properties. Public because used in completion.
   --  TODO: The whole white list thing is a kludge that we want to remove
   --  as soon as the Langkit introspection API handles mapping properties
   --  to their prefixed names, and so handles built-in properties and
   --  fields.

private

   function Introspection_Value_Array_From_Args
     (Ctx           : Eval_Context;
      Node          : AST_Node_Rc;
      Property_Name : Text_Type;
      Args          : L.Arg_List)
      return Introspection_Value_Array;
   --  Evaluate the given arguments and convert them to Value_Type values.

end LKQL.Node_Data;

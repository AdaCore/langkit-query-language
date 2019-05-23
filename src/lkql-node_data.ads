with LKQL.Primitives;    use LKQL.Primitives;
with LKQL.Eval_Contexts; use LKQL.Eval_Contexts;

with Libadalang.Introspection; use Libadalang.Introspection;

with Langkit_Support.Text; use Langkit_Support.Text;

package LKQL.Node_Data is

   function Is_Field_Name (Receiver : LAL.Ada_Node;
                           Name     : Text_Type) return Boolean;
   --  Return whether 'Receiver' has a field called 'Name'

   function Is_Property_Name (Receiver : LAL.Ada_Node;
                              Name     : Text_Type) return Boolean;
   --  Return whether 'Receiver' has a field called 'Name'

   function Access_Node_Field (Ctx        : Eval_Context;
                               Receiver   : LAL.Ada_Node;
                               Field_Name : L.Identifier) return Primitive;
   --  Return the value of the field designated by 'Field_Name' on 'Receiver'.
   --  An exception will be raised if there is no such field.

   function Eval_Node_Property (Ctx           : Eval_Context;
                                Receiver      : LAL.Ada_Node;
                                Property_Name : L.Identifier;
                                Args          : L.Arg_List) return Primitive
     with Pre => not Args.Is_Null;
   --  Evaluate the property designated by 'Property_Name' on 'Receiver'.
   --  An exception will be raised if there is no such property or if the call
   --  artity doesn't match the arity of the property.

private

   function Access_Node_Field (Ctx             : Eval_Context;
                               Receiver        : LAL.Ada_Node;
                               Field_Name      : L.Identifier;
                               Field_Reference : Node_Data_Reference)
                               return Primitive;
   --  Access the field designated by 'Field_Reference' on 'Receiver'.

   function Eval_Node_Property (Ctx        : Eval_Context;
                                Receiver   : LAL.Ada_Node;
                                Data_Ref   : Property_Reference;
                                Identifier : L.Identifier;
                                Args       : L.Arg_List) return Primitive;
   --  Call the property designated by 'Property_Ref' on 'Receiver' with the
   --  given arguments.

   function Value_Array_From_Args (Ctx          : Eval_Context;
                                   Data_Ref     : Node_Data_Reference;
                                   Args         : L.Arg_List)
                                   return Value_Array;
   --  Evalute the given arguments and convert them to Value_Type values.
   --  The converted argument's types will be compared to the expected
   --  argument types for the given poperty.

   function Data_Reference_For_Name (Receiver : LAL.Ada_Node;
                                     Name     : Text_Type)
                                     return Any_Node_Data_Reference;
   --  Return the node data type corresponding to 'Name' on the receiver
   --  node. Return None if the name is invalid.

   function Create_Primitive (Ctx    : Eval_Context;
                              Member : L.LKQL_Node'Class;
                              Value  : Value_Type) return Primitive;
   --  Converte the given 'Value_Type' value to a 'Primitive'.
   --  An exception will be raised if no Primitve kind match the kind of
   --  'Value'.

   function To_Value_Type (Ctx         : Eval_Context;
                           Value_Expr  : L.Expr;
                           Value       : Primitive;
                           Target_Kind : Value_Kind) return Value_Type;
   --  Create a Value_Type value of kind 'Target_Kind' from the given Primitive
   --  value. An exceptioin will be raised if the conversion is illegal.

   function Built_In_Field
     (Receiver : LAL.Ada_Node; Property_Name : Text_Type) return Primitive;
   --  Return the value of the built-in property named 'Property_Name' on
   --  'Receiver'.

   function Is_Built_In (Name : Text_Type) return Boolean;
   --  Return whether the property named 'Name' is built-in

end LKQL.Node_Data;

------------------------------------------------------------------------------
--                                                                          --
--                                   LKQL                                   --
--                                                                          --
--                     Copyright (C) 2019-2022, AdaCore                     --
--                                                                          --
-- LKQL is free software;  you can redistribute it and/or modify  it        --
-- under terms of the GNU General Public License  as published by the Free  --
-- Software Foundation;  either version 3,  or (at your option)  any later  --
-- version.   This  software  is distributed in the hope that it  will  be  --
-- useful but  WITHOUT ANY WARRANTY;  without even the implied warranty of  --
-- MERCHANTABILITY  or  FITNESS  FOR  A PARTICULAR PURPOSE.                 --
--                                                                          --
-- As a special  exception  under  Section 7  of  GPL  version 3,  you are  --
-- granted additional  permissions described in the  GCC  Runtime  Library  --
-- Exception, version 3.1, as published by the Free Software Foundation.    --
--                                                                          --
-- You should have received a copy of the GNU General Public License and a  --
-- copy of the GCC Runtime Library Exception along with this program;  see  --
-- the files COPYING3 and COPYING.RUNTIME respectively.  If not, see        --
-- <http://www.gnu.org/licenses/>.                                          --
------------------------------------------------------------------------------

with Ada.Exceptions; use Ada.Exceptions;

with LKQL.Errors;        use LKQL.Errors;
with LKQL.Partial_AST_Nodes;     use LKQL.Partial_AST_Nodes;
with LKQL.Eval_Contexts; use LKQL.Eval_Contexts;
with LKQL.Primitives;    use LKQL.Primitives;

with Langkit_Support.Text;        use Langkit_Support.Text;

private package LKQL.Error_Handling is

   procedure Raise_And_Record_Error
     (Ctx : Eval_Context; Error : Error_Data)
      with No_Return;
   --  Add the given error to the evaluation context and:
   --    * raise a Recoverable_Error if error recovery is enabled and the
   --      user chooses to ignore the error
   --    * raise a Stop_Evaluation_Error if error recovery is disabled or the
   --      user chooses not to ignore the error

   procedure Raise_From_Exception
     (Ctx : Eval_Context; E : Exception_Occurrence; N : L.LKQL_Node'Class)
     with No_Return;
   --  Shortcut around ``Raise_And_Record_Error`` that raises from an exception
   --  occurrence.

   procedure Raise_Invalid_Member (Ctx      : Eval_Context;
                                   Node     : L.Dot_Access;
                                   Receiver : Primitive)
      with No_Return;
   --  Raise an exception signaling an invalid member access, and add an
   --  Error_Data describing the error to the evaluation context.

   procedure Raise_Invalid_Member (Ctx      : Eval_Context;
                                   Node     : L.Identifier;
                                   Receiver : Primitive)
     with No_Return;
   --  Raise an exception signaling an invalid member access, and add an
   --  Error_Data describing the error to the evaluation context.

   procedure Raise_Null_Root (Ctx : Eval_Context; Node : L.Query)
      with No_Return;
   --  Raise an exception signaling the absence of a proper AST root while
   --  trying to execute a query, and add an Error_Data describing the error
   --  to the evaluation context.

   procedure Raise_Invalid_Type (Ctx      : Eval_Context;
                                 Node     : L.LKQL_Node;
                                 Expected : Text_Type;
                                 Value    : Primitive)
      with No_Return;
   --  Raise an exception signaling a type error, and add an Error_Data
   --  describing the error to the evaluation context.

   procedure Raise_Invalid_Kind (Ctx      : Eval_Context;
                                 Node     : L.LKQL_Node;
                                 Expected : Valid_Primitive_Kind;
                                 Value    : Primitive)
      with No_Return;
   --  Raise an exception signaling a type error, and add an Error_Data
   --  describing the error to the evaluation context.

   procedure Raise_Invalid_Selector_Name (Ctx  : Eval_Context;
                                          Node : L.Identifier)
     with No_Return;
   --  Raise an exception signaling the use of an invalid selector name,
   --  and add an Error_Data describing the error to the evaluation context.

   procedure Raise_Unknown_Symbol (Ctx  : Eval_Context;
                                   Node : L.Identifier)
     with No_Return;
   --  Raise an exception signaling the use of an unknown identifier,
   --  and add an Error_Data describing the error to the evaluation context.

   procedure Raise_Already_Existing_Symbol (Ctx        : Eval_Context;
                                            Identifier : Symbol_Type;
                                            Node       : L.LKQL_Node)
     with No_Return;
   --  Raise an exception signaling an attempt to create a binding using a name
   --  that is already bound to a value in the local context,
   --  and add an Error_Data describing the error to the evaluation context.

   procedure Raise_Invalid_Arity (Ctx            : Eval_Context;
                                  Expected_Arity : Natural;
                                  Arguments      : L.Arg_List)
     with No_Return;
   --  Raise an exception signaling an attempt to call a function with an
   --  incorrect number of arguments,
   --  and add an Error_Data describing the error to the evaluation context.

   procedure Raise_Unknown_Argument (Ctx        : Eval_Context;
                                     Identifier : L.Identifier)
     with No_Return;
   --  Raise an exception signaling an attempt to call a function with an
   --  argument which name doesn't match the name of a parameter, and add an
   --  Error_Data describing the error to the evaluation context.

   procedure Raise_Positionnal_After_Named (Ctx         : Eval_Context;
                                            Positionnal : L.Expr_Arg)
     with No_Return;
   --  Raise an exception signaling the use of a positional argument after a
   --  named argument, and add an Error_Data describing the error to the
   --  evaluation context.

   procedure Raise_Already_Seen_Arg (Ctx : Eval_Context; Arg : L.Arg)
     with No_Return;
   --  Raise an exception signaling an attempt to call a function with
   --  at least two identically-named arguments.

   procedure Raise_Invalid_Kind_For_Selector (Ctx   : Eval_Context;
                                              Node  : L.LKQL_Node'Class;
                                              Value : Primitive)
     with No_Return;
   --  Raise an exception signaling the use of a value that is neither a node
   --  or an unpacked collection of nodes in a selector, and add an Error_Data
   --  describing the error to the evaluation context.

   procedure Raise_No_Such_Field (Ctx        : Eval_Context;
                                  Node       : H.AST_Node_Holder;
                                  Field_Name : L.Identifier)
     with No_Return;
   --  Raise an exception signaling an attempt to access a field that doesn't
   --  exists, and add an Error_Data describing the error to the evaluation
   --  context.

   procedure Raise_No_Such_Property (Ctx           : Eval_Context;
                                     Node          : H.AST_Node_Holder;
                                     Property_Name : L.Identifier)
     with No_Return;
   --  Raise an exception signaling an attempt to access a property that
   --  doesn't exists, and add an Error_Data describing the error to the
   --  evaluation context.

   procedure Raise_Null_Access (Ctx         : Eval_Context;
                                Node        : Primitive;
                                Member_Name : L.Identifier)
     with No_Return,
          Pre => Kind (Node) = Kind_Node;
   --  Raise an exception signaling an attempt to directly access a
   --  field/property on a nullable node, and add an Error_Data describing the
   --  error to the evaluation context.

private

   type Data_Type is (Field, Property);

   procedure Raise_No_Such_Datum (Ctx            : Eval_Context;
                                  Node           : H.AST_Node_Holder;
                                  Field_Name     : L.Identifier;
                                  Data_Type_Name : Text_Type)
     with No_Return;
   --  Raise an exception signaling an attempt to access a node datum that
   --  doesn't exist, and add an Error_Data describing the error to the
   --  evaluation context.
   --  Data_Type_Name = "field" or "property".

end LKQL.Error_Handling;

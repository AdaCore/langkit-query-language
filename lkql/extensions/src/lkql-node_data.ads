------------------------------------------------------------------------------
--                                                                          --
--                                   LKQL                                   --
--                                                                          --
--                     Copyright (C) 2019-2021, AdaCore                     --
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

with LKQL.AST_Nodes;     use LKQL.AST_Nodes;
with LKQL.Primitives;    use LKQL.Primitives;
with LKQL.Eval_Contexts; use LKQL.Eval_Contexts;

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

private

   function Introspection_Value_Array_From_Args
     (Ctx           : Eval_Context;
      Node          : AST_Node_Rc;
      Property_Name : Text_Type;
      Args          : L.Arg_List)
      return Introspection_Value_Array;
   --  Evaluate the given arguments and convert them to Value_Type values.

end LKQL.Node_Data;

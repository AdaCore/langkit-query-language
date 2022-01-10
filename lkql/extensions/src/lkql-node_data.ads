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

with LKQL.Primitives;    use LKQL.Primitives;
with LKQL.Eval_Contexts; use LKQL.Eval_Contexts;

package LKQL.Node_Data is

   function Get_Struct_Member_Ref
     (Ctx        : Eval_Context;
      Receiver   : LK.Lk_Node;
      Field_Name : L.Identifier) return LKI.Struct_Member_Ref;
   --  Get the member reference for given receiver and field name. This
   --  function exists so that the evaluator can eventually cache the member
   --  reference rt. recompute it everytime.

   function Access_Node_Field
     (Ctx        : Eval_Context;
      Receiver   : LK.Lk_Node;
      Field_Name : L.Identifier) return Primitive;
   --  Return the value of the field designated by 'Field_Name' on 'Receiver'.
   --  An exception will be raised if there is no such field.

   function Eval_Node_Property
     (Ctx           : Eval_Context;
      Receiver      : LK.Lk_Node;
      Property_Name : L.Identifier;
      Args          : L.Arg_List) return Primitive
     with Pre => not Args.Is_Null;
   function Eval_Node_Property
     (Ctx           : Eval_Context;
      Receiver      : LK.Lk_Node;
      Property_Ref  : LKI.Struct_Member_Ref;
      Args          : L.Arg_List) return Primitive;
   --  Evaluate the property designated by 'Property_Name' on 'Receiver'.
   --  An exception will be raised if there is no such property or if the call
   --  arity doesn't match the arity of the property.

end LKQL.Node_Data;

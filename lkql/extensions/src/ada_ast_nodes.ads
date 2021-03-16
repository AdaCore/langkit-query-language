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

with LKQL; use LKQL;
with LKQL.AST_Nodes; use LKQL.AST_Nodes;

with Libadalang.Analysis; use Libadalang.Analysis;
with Libadalang.Common;

with Langkit_Support.Text; use Langkit_Support.Text;

with Ada.Containers; use Ada.Containers;
with LKQL.Eval_Contexts; use LKQL.Eval_Contexts;
with Libadalang.Helpers; use Libadalang.Helpers;

package Ada_AST_Nodes is

   subtype Node_Type_Id is Libadalang.Common.Node_Type_Id;

   type Ada_AST_Node is new AST_Node with record
      Node : Ada_Node;
   end record;

   type Ada_AST_Node_Access is access all Ada_AST_Node;

   overriding function "=" (Left, Right : Ada_AST_Node) return Boolean;

   overriding function Hash (Node : Ada_AST_Node) return Hash_Type;

   overriding function Text_Image (Node : Ada_AST_Node) return Text_Type;

   overriding function Kind_Name (Node : Ada_AST_Node) return String;

   overriding function Is_Null_Node (Node : Ada_AST_Node) return Boolean;

   overriding function Children_Count (Node : Ada_AST_Node) return Natural;

   function Get_Node_Type_Id (Node : Ada_AST_Node) return Node_Type_Id;
   --  Return the ``Node_Type_Id`` of ``Node``

   overriding function Nth_Child
     (Node : Ada_AST_Node; N : Positive) return Ada_AST_Node;

   overriding function Matches_Kind_Name
     (Node : Ada_AST_Node; Kind_Name : Text_Type) return Boolean;

   overriding function Is_Field_Name
     (Node : Ada_AST_Node; Name : Text_Type) return Boolean;

   overriding function Is_Property_Name
     (Node : Ada_AST_Node; Name : Text_Type) return Boolean;

   overriding function Access_Field
     (Node : Ada_AST_Node; Field : Text_Type) return Introspection_Value;

   overriding function Property_Arity
     (Node : Ada_AST_Node; Property_Name : Text_Type) return Natural;

   overriding function Default_Arg_Value (Node          : Ada_AST_Node;
                                          Property_Name : Text_Type;
                                          Arg_Position  : Positive)
                                          return Introspection_Value;

   function Evaluate_Property
     (Node          : Ada_AST_Node;
      Property_Name : Text_Type;
      Arguments     : Introspection_Value_Array)
      return Introspection_Value;

   function Make_Ada_AST_Node (Node : Ada_Node) return AST_Node_Rc;

   procedure Set_Ada_Ast_Node (Rc : in out AST_Node_Rc; Node : Ada_Node);

   function Kind_Names return Unbounded_Text_Array;
   --  List of all the node kinds' names

   function Kind (Name : Text_Type) return Node_Type_Id;
   --  Return the ``Node_Type_Id`` for a given ``Name``

   function Make_Eval_Context
     (Units        : Unit_Vectors.Vector;
      Analysis_Ctx : L.Analysis_Context := L.No_Analysis_Context;
      Err_Recovery : Boolean := False) return Eval_Context;

end Ada_AST_Nodes;

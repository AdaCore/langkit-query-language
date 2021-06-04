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
with LKQL.Primitives; use LKQL.Primitives;
with LKQL.Partial_AST_Nodes; use LKQL.Partial_AST_Nodes;

with Libadalang.Analysis; use Libadalang.Analysis;
with Libadalang.Common; use Libadalang.Common;

with Langkit_Support.Text; use Langkit_Support.Text;
with Langkit_Support.Slocs; use Langkit_Support.Slocs;

with Ada.Containers; use Ada.Containers;
with LKQL.Eval_Contexts; use LKQL.Eval_Contexts;
with Libadalang.Helpers; use Libadalang.Helpers;

package Ada_AST_Nodes is

   package LALCO renames Libadalang.Common;

   subtype Node_Type_Id is Libadalang.Common.Node_Type_Id;

   type Ada_AST_Node is new AST_Node with record
      Node : Ada_Node;
   end record;

   type Ada_Member_Reference
   is new LKQL.AST_Nodes.AST_Node_Member_Reference with record
      Ref : LALCO.Member_Reference;
   end record;

   type Ada_AST_Node_Access is access all Ada_AST_Node;

   type Ada_AST_Node_Kind is new AST_Node_Kind with record
      First, Last : LALCO.Ada_Node_Kind_Type;
   end record;

   function Get_Kind_From_Name
     (Kind_Name : Text_Type) return Ada_AST_Node_Kind;

   overriding function Matches_Kind_Of
     (Self : Ada_AST_Node_Kind; Node : AST_Node'Class) return Boolean;

   overriding function "=" (Left, Right : Ada_AST_Node) return Boolean;

   overriding function Hash (Node : Ada_AST_Node) return Hash_Type;

   overriding function Text_Image (Node : Ada_AST_Node) return Text_Type;

   overriding function Text (Node : Ada_AST_Node) return Text_Type;

   overriding function Kind_Name (Node : Ada_AST_Node) return String;

   overriding function Is_Null_Node (Node : Ada_AST_Node) return Boolean;

   overriding function Children_Count (Node : Ada_AST_Node) return Natural;

   function Get_Node_Type_Id (Node : Ada_AST_Node) return Node_Type_Id;
   --  Return the ``Node_Type_Id`` of ``Node``

   overriding function Nth_Child
     (Node : Ada_AST_Node; N : Positive) return Ada_AST_Node;

   overriding function Is_Field_Name
     (Node : Ada_AST_Node; Name : Text_Type) return Boolean;

   overriding function Is_Property_Name
     (Node : Ada_AST_Node; Name : Text_Type) return Boolean;

   overriding function Access_Field
     (Node  : AST_Node'Class;
      Ref   : Ada_Member_Reference;
      Ctx   : Eval_Context) return Primitive;

   overriding function Get_Member_Reference
     (Node : Ada_AST_Node;
      Name : Text_Type) return AST_Node_Member_Reference'Class;

   overriding function Token_Start
     (Node : Ada_AST_Node) return AST_Token'Class;
   overriding function Token_End
     (Node : Ada_AST_Node) return AST_Token'Class;

   overriding function Property_Arity
     (Ref : Ada_Member_Reference) return Natural;

   overriding function Default_Arg_Value
     (Ref           : Ada_Member_Reference;
      Arg_Position  : Positive;
      Ctx           : Eval_Context) return Primitive;

   overriding function Name
     (Ref : Ada_Member_Reference) return Text_Type;

   overriding function Evaluate_Property
     (Ref       : Ada_Member_Reference;
      Node      : AST_Node'Class;
      Arguments : Primitive_List;
      Ctx       : Eval_Context) return Primitive;

   function Make_Ada_AST_Node (Node : Ada_Node) return H.AST_Node_Holder;

   function Kind_Names return Unbounded_Text_Array;
   --  List of all the node kinds' names

   function Kind (Name : Text_Type) return Node_Type_Id;
   --  Return the ``Node_Type_Id`` for a given ``Name``

   function Make_Eval_Context
     (Units        : Unit_Vectors.Vector;
      Analysis_Ctx : L.Analysis_Context := L.No_Analysis_Context)
      return Eval_Context;

   function Data_Reference_For_Name (Receiver : Ada_AST_Node;
                                     Name     : Text_Type)
                                     return Any_Member_Reference;
   --  Return the node data type corresponding to 'Name' on the receiver
   --  node. Return None if the name is invalid.

   type Ada_AST_Token is new AST_Token with record
      Token : Token_Reference;
   end record;

   overriding function Sloc_Range
     (Self : Ada_AST_Token) return Source_Location_Range;
   overriding function Next (Self : Ada_AST_Token) return AST_Token'Class;
   overriding function Previous (Self : Ada_AST_Token) return AST_Token'Class;
   overriding function Text (Self : Ada_AST_Token) return Text_Type;
   overriding function Kind (Self : Ada_AST_Token) return Text_Type;
   overriding function Image (Self : Ada_AST_Token) return Text_Type;
   overriding function Is_Null (Self : Ada_AST_Token) return Boolean;
end Ada_AST_Nodes;

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

with LKQL.Primitives; use LKQL.Primitives;

with Langkit_Support.Text; use Langkit_Support.Text;
with Langkit_Support.Slocs; use Langkit_Support.Slocs;

with Ada.Containers;
with Ada.Unchecked_Deallocation;
with LKQL.Eval_Contexts; use LKQL.Eval_Contexts;

package LKQL.AST_Nodes is

   Introspection_Error : exception;

   --------------
   -- AST_Node --
   --------------

   type AST_Node is interface;
   --  Interface implemented by concrete AST node types

   type AST_Node_Member_Reference is abstract tagged null record;
   --  Reference to a node member (field or property)

   type AST_Node_Kind is abstract tagged null record;
   --  Opaque representation of a node kind. Provides capabilities to check
   --  if a given node is of this kind.

   function Matches_Kind_Of
     (Self : AST_Node_Kind; Node : AST_Node'Class) return Boolean is abstract;
   --  Check if the given node is of the given kind.

   type Unbounded_Text_Array is
     array (Positive range <>) of Unbounded_Text_Type;
   --  Array of unicode strings

   type Unbounded_Text_Array_Access is access all Unbounded_Text_Array;
   --  Pointer to an array of unicode strings

   function "=" (Left, Right : AST_Node) return Boolean is abstract;
   --  Checks for equality between two AST nodes

   function Hash (Node : AST_Node) return Ada.Containers.Hash_Type is abstract;
   --  Return the hash of an AST node

   function Text_Image (Node : AST_Node) return Text_Type is abstract;
   --  Return a short representation of an AST node

   function Text (Node : AST_Node) return Text_Type is abstract;
   --  Return the text of the node

   function Kind_Name (Node : AST_Node) return String is abstract;
   --  Return the kind name of 'Node'

   function Is_Null_Node (Node : AST_Node) return Boolean is abstract;
   --  Return whether 'Node' is null

   function Children_Count (Node : AST_Node) return Natural is abstract;
   --  Return the number of children of 'Node'

   function Nth_Child
     (Node : AST_Node; N : Positive) return AST_Node is abstract;
   --  Return the Nth child of 'Node'

   function Get_Member_Reference
     (Node : AST_Node; Name : Text_Type) return AST_Node_Member_Reference'Class
      is abstract;
   --  Return the member reference (property or field) for the given node and
   --  the given name.

   function Is_Field_Name
     (Node : AST_Node; Name : Text_Type) return Boolean is abstract;
   --  Return whether 'Name' is the name of one of 'Node's fields

   function Is_Property_Name
     (Node : AST_Node; Name : Text_Type) return Boolean is abstract;
   --  Return whether 'Name' is the name of one of 'Node's properties

   function Access_Field (Node    : AST_Node'Class;
                          Ref     : AST_Node_Member_Reference;
                          Context : Eval_Context)
                          return Primitive is abstract;
   --  Return the value of the 'Node's field named 'Field'.

   function Property_Arity
     (Ref : AST_Node_Member_Reference) return Natural is abstract;
   --  Return the arity of 'Node's property named 'Property_Name'

   function Default_Arg_Value
     (Ref           : AST_Node_Member_Reference;
      Arg_Position  : Positive;
      Ctx           : Eval_Context) return Primitive is abstract;
   --  Return the default value (if any) of the argument named
   --  'Arg_Poisition' of 'Node's property named 'Property_Name'.

   function Evaluate_Property
     (Ref           : AST_Node_Member_Reference;
      Node          : AST_Node'Class;
      Arguments     : Primitive_List;
      Ctx           : Eval_Context)
      return Primitive is abstract;
   --  Evaluate the 'Node's property named 'Property_Name' with the given
   --  arguments.

   function Name
     (Ref : AST_Node_Member_Reference) return Text_Type is abstract;
   --  Return the textual name of the member reference

   procedure Free_Unbounded_Text_Array is new Ada.Unchecked_Deallocation
     (Unbounded_Text_Array, Unbounded_Text_Array_Access);
   --  Free a pointer to an array of unicode strings
   --  TODO??? Is this used ?

   ---------------
   -- AST_Token --
   ---------------

   type AST_Token is interface;
   --  Interface representing an abstract token from a Langkit generated
   --  library.

   function Sloc_Range
     (Self : AST_Token) return Source_Location_Range is abstract;
   --  Return the source location range that this token spans.

   function Next (Self : AST_Token) return AST_Token'Class is abstract;
   --  Return the next token if there is one

   function Previous (Self : AST_Token) return AST_Token'Class is abstract;
   --  Return the previous token if there is one

   function Text (Self : AST_Token) return Text_Type is abstract;
   --  Return the text for this token

   function Kind (Self : AST_Token) return Text_Type is abstract;
   --  Return the kind of this token, as text

   function Image (Self : AST_Token) return Text_Type is abstract;
   --  Return a string representation of this token

   function Is_Null (Self : AST_Token) return Boolean is abstract;
   --  Return whether `Self` denotes a null token value or not

   function Token_Start (Node : AST_Node) return AST_Token'Class is abstract;
   --  Return the starting token for this AST node

   function Token_End (Node : AST_Node) return AST_Token'Class is abstract;
   --  Return the end token for this AST node

   --------------
   -- AST_Unit --
   --------------

   type AST_Unit is interface;
   --  Interface representing an abstract analysis unit from a Langkit
   --  generated library.

   function Name
     (Self : AST_Unit) return Text_Type is abstract;
   --  Return the name of this unit - practically, the file name for the file
   --  this unit encompasses.

   function Root
      (Self : AST_Unit) return AST_Node'Class is abstract;
   --  Return the root AST node for this unit

   function Unit (Node : AST_Node) return AST_Unit'Class is abstract;
   --  Given a node, return the unit it belongs to

end LKQL.AST_Nodes;

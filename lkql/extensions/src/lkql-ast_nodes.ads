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

with Iters.Iterators;
limited with LKQL.Primitives;

with LKQL.Adaptive_Integers; use LKQL.Adaptive_Integers;

with Langkit_Support.Text; use Langkit_Support.Text;

with GNATCOLL.Refcount;

with Ada.Containers;
with Ada.Unchecked_Deallocation;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Vectors;

package LKQL.AST_Nodes is

   Introspection_Error : exception;

   --------------
   -- AST_Node --
   --------------

   type AST_Node is interface;
   --  Interface implemented by concrete AST node types

   type AST_Node_Member_Reference is abstract tagged null record;
   --  Reference to a node member (field or property)

   type AST_Node_Member_Ref_Access
   is access all AST_Node_Member_Reference'Class;

   type AST_Node_Access is access all AST_Node'Class;
   --  Pointer to an AST node

   type AST_Node_Array is array (Positive range <>) of AST_Node_Access;
   --  Array of AST node pointers

   Empty_Ast_Node_Array : constant AST_Node_Array (1 .. 0) := (others => <>);

   type AST_Node_Array_Access is access all AST_Node_Array;
   --  Pointer to an array of AST node pointers

   type Introspection_Value_Kind is
     (Kind_Node,
      Kind_Node_Array,
      Kind_Bool,
      Kind_Int,
      Kind_Text,
      Kind_Text_Array,
      Kind_Empty_List);
   --  Denotes the kind of an Introspection value

   type Introspection_Value;
   --  Wrapper around Value_Type values exposed by the langkit library
   --  thought the introspection API.

   type Introspection_Value_Access is access all Introspection_Value;
   --  Pointer to an Introspection value

   type Unbounded_Text_Array is
     array (Positive range <>) of Unbounded_Text_Type;
   --  Array of unicode strings

   type Unbounded_Text_Array_Access is access all Unbounded_Text_Array;
   --  Pointer to an array of unicode strings

   type Introspection_Value (Kind : Introspection_Value_Kind := Kind_Node) is
   record
      case Kind is
         when Kind_Node =>
            Node_Val : AST_Node_Access;
         when Kind_Node_Array =>
            Node_Array_Val : AST_Node_Array_Access;
         when Kind_Bool =>
            Bool_Val : Boolean;
         when Kind_Int =>
            Int_Val : Integer;
         when Kind_Text =>
            Text_Val : Unbounded_Text_Type;
         when Kind_Text_Array =>
            Text_Array_Val : Unbounded_Text_Array_Access;
         when Kind_Empty_List =>
            null;
      end case;
   end record;

   type Introspection_Value_Array is
     array (Positive range <>) of Introspection_Value;
   --  Array of Introspection values

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

   function Matches_Kind_Name
     (Node : AST_Node; Kind_Name : Text_Type) return Boolean is abstract;
   --  Return True if 'Node's kind name is 'Kind_Name' or 'Node's type is a
   --  subtype of a type which kind name is 'Kind_Name'.

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

   function Access_Field (Node  : AST_Node'Class;
                          Ref   : AST_Node_Member_Reference)
                          return Introspection_Value is abstract;
   --  Return the value of the 'Node's field named 'Field'.

   function Property_Arity
     (Ref : AST_Node_Member_Reference) return Natural is abstract;
   --  Return the arity of 'Node's property named 'Property_Name'

   function Default_Arg_Value
     (Ref           : AST_Node_Member_Reference;
      Arg_Position  : Positive) return Introspection_Value is abstract;
   --  Return the default value (if any) of the argument named
   --  'Arg_Poisition' of 'Node's property named 'Property_Name'.

   function Evaluate_Property
     (Ref           : AST_Node_Member_Reference;
      Node          : AST_Node'Class;
      Arguments     : Introspection_Value_Array)
      return Introspection_Value is abstract;
   --  Evaluate the 'Node's property named 'Property_Name' with the given
   --  arguments.

   function Name
     (Ref : AST_Node_Member_Reference) return Text_Type is abstract;
   --  Return the textual name of the member reference

   ----------------------------
   -- Deallocation functions --
   ----------------------------

   procedure Release_Introspection_Value (Value : in out Introspection_Value);
   --  Release the memory allocated for the given Introspection value

   procedure Free_Introspection_Value_Access is new
     Ada.Unchecked_Deallocation
       (Introspection_Value, Introspection_Value_Access);
   --  Free a pointer to an Introspection value

   procedure Free_Unbounded_Text_Array is new Ada.Unchecked_Deallocation
     (Unbounded_Text_Array, Unbounded_Text_Array_Access);
   --  Free a pointer to an array of unicode strings

   -------------------
   -- Utility_Types --
   -------------------

   package AST_Node_Ptrs is
     new GNATCOLL.Refcount.Shared_Pointers
       (Element_Type => AST_Node'Class);
   --  Refcounted AST_Node pointers

   subtype AST_Node_Rc is AST_Node_Ptrs.Ref;
   --  Refcounted AST_Node pointer

   type AST_Node_Rc_Array is array (Positive range <>) of AST_Node_Rc;
   --  Array of refcounted AST node pointers

   Empty_Ast_Node_Rc_Array : constant AST_Node_Rc_Array (1 .. 0) :=
     (others => <>);

   function Hash_Rc (Node : AST_Node_Rc) return Ada.Containers.Hash_Type is
     (Node.Get.Hash);
   --  Return the hash of the AST node referenced by a refcounted pointer

   function "=" (Left, Right : AST_Node_Rc) return Boolean is
     (Left.Get = Right.Get);
   --  Check equality between two AST nodes referenced by refcounted pointers

   procedure Free_Member_Ref is new Ada.Unchecked_Deallocation
     (AST_Node_Member_Reference'Class, AST_Node_Member_Ref_Access);
   --  Free a member reference

   procedure Free_AST_Node is new Ada.Unchecked_Deallocation
     (AST_Node'Class, AST_Node_Access);
   --  Free a pointer to an AST node

   procedure Free_Ast_Node_Array is new Ada.Unchecked_Deallocation
     (AST_Node_Array, AST_Node_Array_Access);
   --  Free a pointer to an array of AST node pointers

   procedure Release_AST_Node_Array (Value : in out AST_Node_Array_Access);
   --  Free the node pointers contained in the array, and then free the
   --  array's pointer.

   function Make_AST_Node_Rc (Node : AST_Node'Class) return AST_Node_Rc;
   --  Create a refcounted AST node pointer from the given AST node value

   function Make_AST_Node_Rc
     (Node : AST_Node_Access) return AST_Node_Rc;
   --  Create a refcounted AST node pointer from the given AST node pointer

   package AST_Node_Lists is
     new Ada.Containers.Doubly_Linked_Lists
       (Element_Type => AST_Node_Rc,
        "="          => AST_Node_Ptrs."=");
   --  Doubly-linked lists of refcounted AST node pointers

   subtype AST_Node_List is AST_Node_Lists.List;
   --  Doubly-linked list of refcounted AST node pointers

   package AST_Node_Vectors is new Ada.Containers.Vectors
     (Element_Type => AST_Node_Rc,
      Index_Type   => Positive,
      "="          => AST_Node_Ptrs."=");
   --  Vectors of refcounted AST node pointers

   subtype AST_Node_Vector is AST_Node_Vectors.Vector;
   --  Vector of refcounted AST node pointers

   package AST_Node_Iterators is new Iters.Iterators (AST_Node_Rc);
   --  Iterators of refcounted AST node pointers

   subtype AST_Node_Iterator is AST_Node_Iterators.Iterator_Interface;
   --  Iterator of refcounted AST node pointers

   subtype AST_Node_Iterator_Access is AST_Node_Iterators.Iterator_Access;
   --  Pointer to an iterator of refcounted AST node pointers

   subtype AST_Node_Iterator_Predicate is AST_Node_Iterators.Predicates.Func;
   --  Predicate on refcounted AST node pointers

   subtype AST_Node_Predicate_Access is
     AST_Node_Iterators.Predicates.Func_Access;
   --  Pointer to a predicate on AST node pointers

   -----------------------------------
   -- Introspection_Value creation --
   -----------------------------------

   function To_Introspection_Value (Val : Boolean) return Introspection_Value;
   --  Create an Introspection value from the given Boolean value

   function To_Introspection_Value
     (Val : Adaptive_Integer) return Introspection_Value;
   --  Create an Introspection value from the given Adaptive_Integer value

   function To_Introspection_Value
     (Val : Unbounded_Text_Type) return Introspection_Value;
   --  Create an Introspection value from the given Unbounded_Text_Type value

   function To_Introspection_Value
     (Val : AST_Node_Rc) return Introspection_Value;
   --  Create an Introspection value from the given AST_Node_Rc

   function To_Introspection_Value
     (Val : LKQL.Primitives.Primitive_List_Access) return Introspection_Value;
   --  Create an Introspection value from a list of Primitive values.
   --  The values in the list must all have the same kind, and their kind must
   --  either be Kind_Node or Kind_Str.

   --------------------
   -- Child_Iterator --
   --------------------

   type Child_Iterator is new AST_Node_Iterator with private;
   --  Iterator that yields the children of a node in a depth-first fashion

   overriding function Next (Iter   : in out Child_Iterator;
                             Result : out AST_Node_Rc)
                             return Boolean;

   overriding function Clone (Iter : Child_Iterator) return Child_Iterator;

   --  function Make_Child_Iterator (Node : AST_Node_Rc) return Child_Iterator;

   function Make_Child_Iterator
     (Nodes : AST_Node_Array) return Child_Iterator;

   function Make_Child_Iterator
     (Nodes : AST_Node_Vector) return Child_Iterator;

private

   type Child_Iterator is new AST_Node_Iterator with record
      Roots         : AST_Node_Vector;
      Next_Elements : AST_Node_List;
   end record;

end LKQL.AST_Nodes;

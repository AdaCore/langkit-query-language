with Iters.Iterators;

with Langkit_Support.Text; use Langkit_Support.Text;

with GNATCOLL.Refcount;

with Ada.Containers;
with Ada.Unchecked_Deallocation;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Vectors;

package LKQL.AST_Nodes is

   --------------
   -- AST_Node --
   --------------

   type AST_Node is interface;

   type AST_Node_Access is access all AST_Node'Class;

   type AST_Node_Array is array (Positive range <>) of AST_Node_Access;

   type Introspection_Value_Kind is (Kind_Node, Kind_Bool);

   type Introspection_Value (Kind : Introspection_Value_Kind) is record
      case Kind is
         when Kind_Node =>
            Node_Val : AST_Node_Access;
         when Kind_Bool =>
            Bool_Val : Boolean;
      end case;
   end record;

   function "=" (Left, Right : AST_Node) return Boolean is abstract;
   --  Checks for equality between two AST nodes

   function Hash (Node : AST_Node) return Ada.Containers.Hash_Type is abstract;
   --  Return the hash of an AST node

   function Text_Image (Node : AST_Node) return Text_Type is abstract;

   function Kind_Name (Node : AST_Node) return String is abstract;

   function Is_Null (Node : AST_Node) return Boolean is abstract;

   function Children_Count (Node : AST_Node) return Natural is abstract;

   function Nth_Child
     (Node : AST_Node; N : Positive) return AST_Node is abstract;

   function Matches_Kind_Name
     (Node : AST_Node; Kind_Name : String) return Boolean is abstract;

   function Is_Field_Name
     (Node : AST_Node; Name : Text_Type) return Boolean is abstract;

   function Is_Property_Name
     (Node : AST_Node; Name : Text_Type) return Boolean is abstract;

   function Access_Field (Node : AST_Node;
                          Field : Text_Type)
                          return Introspection_Value is abstract;

   -------------------
   -- Utility_Types --
   -------------------

   package AST_Node_Ptrs is
     new GNATCOLL.Refcount.Shared_Pointers
       (Element_Type => AST_Node'Class);

   subtype AST_Node_Rc is AST_Node_Ptrs.Ref;

   function Hash_Rc (Node : AST_Node_Rc) return Ada.Containers.Hash_Type is
      (Node.Get.Hash);

   procedure Free_AST_Node is new Ada.Unchecked_Deallocation
     (AST_Node'Class, AST_Node_Access);

   function Make_AST_Node_Rc (Node : AST_Node'Class) return AST_Node_Rc;

   package AST_Node_Lists is
     new Ada.Containers.Doubly_Linked_Lists
       (Element_Type => AST_Node_Rc,
        "="          => AST_Node_Ptrs."=");

   subtype AST_Node_List is AST_Node_Lists.List;

   package AST_Node_Vectors is new Ada.Containers.Vectors
     (Element_Type => AST_Node_Rc,
      Index_Type   => Positive,
      "="          => AST_Node_Ptrs."=");

   subtype AST_Node_Vector is AST_Node_Vectors.Vector;

   package AST_Node_Iterators is new Iters.Iterators (AST_Node_Rc);

   subtype AST_Node_Iterator is AST_Node_Iterators.Iterator_Interface;

   subtype AST_Node_Iterator_Access is AST_Node_Iterators.Iterator_Access;

   subtype AST_Node_Iterator_Predicate is AST_Node_Iterators.Predicates.Func;

   subtype AST_Node_Predicate_Access is
     AST_Node_Iterators.Predicates.Func_Access;

   --------------------
   -- Child_Iterator --
   --------------------

   type Child_Iterator is new AST_Node_Iterator with private;

   overriding function Next (Iter   : in out Child_Iterator;
                             Result : out AST_Node_Rc)
                             return Boolean;

   overriding function Clone (Iter : Child_Iterator) return Child_Iterator;

   function Make_Child_Iterator (Node : AST_Node_Rc) return Child_Iterator;

private

   type Child_Iterator is new AST_Node_Iterator with record
      Root          : AST_Node_Rc;
      Next_Elements : AST_Node_List;
   end record;

end LKQL.AST_Nodes;

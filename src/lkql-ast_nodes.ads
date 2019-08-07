with Iters.Iterators;
limited with LKQL.Primitives;

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

   type AST_Node_Access is access all AST_Node'Class;
   --  Pointer to an AST node

   type AST_Node_Array is array (Positive range <>) of AST_Node_Access;
   --  Array of AST node pointers

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
   --  Wrapper arround Value_Type values exposed by the langkit library
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

   function Node_Prototypes (Node : AST_Node) return String is abstract;
   --  Return the LKQL prototypes of the target language's node types.

   function "=" (Left, Right : AST_Node) return Boolean is abstract;
   --  Checks for equality between two AST nodes

   function Hash (Node : AST_Node) return Ada.Containers.Hash_Type is abstract;
   --  Return the hash of an AST node

   function Text_Image (Node : AST_Node) return Text_Type is abstract;
   --  Return a short representation of an AST node

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
     (Node : AST_Node; Kind_Name : String) return Boolean is abstract;
   --  Return True if 'Node's kind name is 'Kind_Name' or 'Node's type is a
   --  subtype of a type which kind name is 'Kind_Name'.

   function Is_Field_Name
     (Node : AST_Node; Name : Text_Type) return Boolean is abstract;
   --  Return whether 'Name' is the name of one of 'Node's fields

   function Is_Property_Name
     (Node : AST_Node; Name : Text_Type) return Boolean is abstract;
   --  Return whether 'Name' is the name of one of 'Node's properties

   function Access_Field (Node  : AST_Node;
                          Field : Text_Type)
                          return Introspection_Value is abstract;
   --  Return the value of the 'Node's field named 'Field'.

   function Property_Arity (Node          : AST_Node;
                            Property_Name : Text_Type)
                            return Natural is abstract;
   --  Return the arity of 'Node's property named 'Property_Name'

   function Default_Arg_Value (Node          : AST_Node;
                               Property_Name : Text_Type;
                               Arg_Position  : Positive)
                               return Introspection_Value is abstract;
   --  Return the default value (if any) of the argument named
   --  'Arg_Poisition' of 'Node's property named 'Property_Name'.

   function Evaluate_Property
     (Node          : AST_Node;
      Property_Name : Text_Type;
      Arguments     : Introspection_Value_Array)
      return Introspection_Value is abstract;
   --  Evaluate the 'Node's property named 'Property_Name' with the given
   --  arguments.

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

   function Hash_Rc (Node : AST_Node_Rc) return Ada.Containers.Hash_Type is
     (Node.Get.Hash);
   --  Return the hash of the AST node referenced by a refcounted pointer

   function "=" (Left, Right : AST_Node_Rc) return Boolean is
     (Left.Get = Right.Get);
   --  Check equality between two AST nodes referenced by refcounted pointers

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

   function To_Introspection_Value (Val : Boolean) return Introspection_Value
   is (Kind => Kind_Bool, Bool_Val => Val);
   --  Create an Introspection value from the given Boolean value

   function To_Introspection_Value (Val : Integer) return Introspection_Value
   is (Kind => Kind_Int, Int_Val => Val);
   --  Create an Introspection value from the given Integer value

   function To_Introspection_Value
     (Val : Unbounded_Text_Type) return Introspection_Value
   is
     (Kind => Kind_Text, Text_Val => Val);
   --  Create an Introspection value from the given Unbounded_Text_Type value

   function To_Introspection_Value
     (Val : AST_Node_Rc) return Introspection_Value
   is
      (Kind     => Kind_Node,
       Node_Val => new AST_Node'Class'(Val.Unchecked_Get.all));
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

   function Make_Child_Iterator (Node : AST_Node_Rc) return Child_Iterator;

private

   type Child_Iterator is new AST_Node_Iterator with record
      Root          : AST_Node_Rc;
      Next_Elements : AST_Node_List;
   end record;

end LKQL.AST_Nodes;

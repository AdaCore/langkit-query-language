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

with Libadalang.Common; use Libadalang.Common;
with Libadalang.Introspection; use Libadalang.Introspection;

with Ada.Unchecked_Conversion;
with Ada.Containers.Hashed_Maps;
with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;
with Ada.Strings.Wide_Wide_Unbounded.Wide_Wide_Hash;
with LKQL.Primitives; use LKQL.Primitives;
with Ada.Wide_Wide_Characters.Handling; use Ada.Wide_Wide_Characters.Handling;

package body Ada_AST_Nodes is

   package I renames Libadalang.Introspection;

   subtype Built_In_LAL_Field is Member_Reference
   range Ada_Node_Parent .. Ada_Node_Is_Ghost;

   Empty_Value_Array : constant Value_Array (1 .. 0) := (others => <>);
   --  Empty Array of Value_Type values

   function Data_Reference_For_Name (Receiver : Ada_AST_Node;
                                     Name     : Text_Type)
                                     return Any_Member_Reference;
   --  Return the node data type corresponding to 'Name' on the receiver
   --  node. Return None if the name is invalid.

   function Is_Built_In (Name : Text_Type) return Boolean;
   --  Return whether the property named 'Name' is built-in

   function Built_In_Field (Receiver      : Ada_AST_Node;
                            Property_Name : Text_Type)
                            return Introspection_Value;
   --  Return the value of the built-in property named 'Property_Name' on
   --  'Receiver'.

   function Make_Introspection_Value
     (Value : Text_Type) return Introspection_Value;
   --  Create an Introspection value from the given Text_Type value

   function Make_Introspection_Value
     (Value : I.Value_Type) return Introspection_Value;
   --  Create an Introspection_value value from the given Value_type value

   function Get_Property_Ref (Node          : Ada_AST_Node;
                              Property_Name : Text_Type)
                              return Property_Reference;
   --  Return the reference of the property named `Property_Name` on `Node`.
   --  Raise an exception if there is no such property.

   function Make_Value_Type (Value       : Introspection_Value;
                             Target_Kind : Value_Kind)
                             return I.Value_Type;
   --  Create a Value_Type value from the given Introspection value

   function Array_To_Value_Type (Value       : AST_Node_Array;
                                 Target_Kind : Value_Kind)
                                 return I.Value_Type;

   function String_To_Value_Type (Value       : Unbounded_Text_Type;
                                  Target_Kind : Value_Kind)
                                  return I.Value_Type;

   overriding function "=" (Left, Right : Ada_AST_Node) return Boolean is
     (Left.Node = Right.Node);

   overriding function Hash (Node : Ada_AST_Node) return Hash_Type is
     (Hash (Node.Node));

   overriding function Text_Image (Node : Ada_AST_Node) return Text_Type is
      (To_Text (Node.Node.Image));

   overriding function Kind_Name (Node : Ada_AST_Node) return String is
     (Kind_Name (Node.Node));

   overriding function Is_Null_Node (Node : Ada_AST_Node) return Boolean is
     (Node.Node.Is_Null);

   overriding function Children_Count (Node : Ada_AST_Node) return Natural is
     (Node.Node.Children_Count);

   overriding function Nth_Child
     (Node : Ada_AST_Node; N : Positive) return Ada_AST_Node
   is
     (Ada_AST_Node'(Node => Node.Node.Child (N)));

   function Make_Ada_AST_Node (Node : Ada_Node) return AST_Node_Rc
   is (Make_AST_Node_Rc (Ada_AST_Node'(Node => Node)));

   ----------------------
   -- Set_Ada_Ast_Node --
   ----------------------

   procedure Set_Ada_Ast_Node (Rc : in out AST_Node_Rc; Node : Ada_Node) is
   begin
      Rc.Set (Ada_AST_Node'(Node => Node));
   end Set_Ada_Ast_Node;

   --------------------------------------------------
   -- Node array to introspection value conversion --
   --------------------------------------------------

   generic
      type Array_Elem is new Ada_Node with private;

      type Array_Type is array (Positive range <>) of Array_Elem;

   function Introspection_Value_From_Array
     (Nodes : Array_Type) return Introspection_Value;

   ------------------------------------
   -- Introspection_Value_From_Array --
   ------------------------------------

   function Introspection_Value_From_Array
     (Nodes : Array_Type) return Introspection_Value
   is
      Result        : constant AST_Node_Array_Access :=
        new AST_Node_Array (1 .. Nodes'Length);
   begin
      for I in Nodes'Range loop
         Result (I) := new Ada_AST_Node'(Node => Nodes (I).As_Ada_Node);
      end loop;

      return (Kind => Kind_Node_Array, Node_Array_Val => Result);
   end Introspection_Value_From_Array;

   function Make_Introspection_Value is new
     Introspection_Value_From_Array (Base_Type_Decl, Base_Type_Decl_Array);

   function Make_Introspection_Value is new
     Introspection_Value_From_Array (Ada_Node, Ada_Node_Array);

   function Make_Introspection_Value is new
     Introspection_Value_From_Array
       (Generic_Instantiation, Generic_Instantiation_Array);

   function Make_Introspection_Value is new
     Introspection_Value_From_Array (Expr, Expr_Array);

   function Make_Introspection_Value is new
     Introspection_Value_From_Array
       (Base_Formal_Param_Decl, Base_Formal_Param_Decl_Array);

   function Make_Introspection_Value is new
     Introspection_Value_From_Array (Type_Decl, Type_Decl_Array);

   function Make_Introspection_Value is new
     Introspection_Value_From_Array (Defining_Name, Defining_Name_Array);

   function Make_Introspection_Value is new
     Introspection_Value_From_Array (Param_Spec, Param_Spec_Array);

   function Make_Introspection_Value is new
     Introspection_Value_From_Array (Basic_Decl, Basic_Decl_Array);

   function Make_Introspection_Value is new
     Introspection_Value_From_Array (Compilation_Unit, Compilation_Unit_Array);

   -----------------------------------------------
   -- Node array to Value_Type array conversion --
   -----------------------------------------------

   generic

      type Node_Type is new Ada_Node with private;

      type Node_Array is array (Positive range <>) of Node_Type;

      with function Convert (Node : Ada_Node'Class) return Node_Type;

   function Node_Array_From_List
     (Nodes : AST_Node_Array) return Node_Array;

   function Introspection_Node_To_Ada_Node is new Ada.Unchecked_Conversion
     (AST_Node_Access, Ada_AST_Node_Access);

   --------------------------
   -- Node_Array_From_List --
   --------------------------

   function Node_Array_From_List
     (Nodes : AST_Node_Array) return Node_Array
   is
      Result       : Node_Array (1 .. Nodes'Length);
   begin
      for I in Nodes'Range loop
         Result (I) :=
           Convert (Introspection_Node_To_Ada_Node (Nodes (I)).Node);
      end loop;

      return Result;
   end Node_Array_From_List;

   function Base_Type_Decl_Array_From_AST_Array is new Node_Array_From_List
     (Base_Type_Decl,
      Base_Type_Decl_Array,
      As_Base_Type_Decl);

   function Ada_Node_Array_From_AST_Array is new Node_Array_From_List
     (Ada_Node,
      Ada_Node_Array,
      As_Ada_Node);

   function Generic_Instantiation_Array_From_AST_Array
   is new Node_Array_From_List
     (Generic_Instantiation,
      Generic_Instantiation_Array,
      As_Generic_Instantiation);

   function Expr_Array_From_AST_Array is new Node_Array_From_List
     (Expr,
      Expr_Array,
      As_Expr);

   function Base_Formal_Param_Decl_Array_From_AST_Array
   is new Node_Array_From_List
     (Base_Formal_Param_Decl,
      Base_Formal_Param_Decl_Array,
      As_Base_Formal_Param_Decl);

   function Type_Decl_Array_From_AST_Array is new Node_Array_From_List
     (Type_Decl,
      Type_Decl_Array,
      As_Type_Decl);

   function Defining_Name_Array_From_AST_Array is new Node_Array_From_List
     (Defining_Name,
      Defining_Name_Array,
      As_Defining_Name);

   function Param_Spec_Array_From_AST_Array is new Node_Array_From_List
     (Param_Spec,
      Param_Spec_Array,
      As_Param_Spec);

   function Basic_Decl_Array_From_AST_Array is new Node_Array_From_List
     (Basic_Decl,
      Basic_Decl_Array,
      As_Basic_Decl);

   function Compilation_Unit_Array_From_AST_Array is new Node_Array_From_List
     (Compilation_Unit,
      Compilation_Unit_Array,
      As_Compilation_Unit);

   -----------------------------
   -- Data_Reference_For_Name --
   -----------------------------

   function Data_Reference_For_Name
     (Receiver : Ada_AST_Node; Name : Text_Type) return Any_Member_Reference
   is
      Receiver_Type_Id : constant Node_Type_Id :=
        Id_For_Kind (Receiver.Node.Kind);
   begin
      return Lookup_Member (Receiver_Type_Id, Name);
   end Data_Reference_For_Name;

   -----------------
   -- Is_Built_In --
   -----------------

   function Is_Built_In (Name : Text_Type) return Boolean is
     (Name = "image" or else Name = "text");

   --------------------
   -- Built_In_Field --
   --------------------

   function Built_In_Field (Receiver      : Ada_AST_Node;
                            Property_Name : Text_Type)
                            return Introspection_Value
   is
   begin
      if Property_Name = "image" then
         return Make_Introspection_Value (To_Text (Receiver.Node.Image));
      elsif Property_Name = "text" then
         return Make_Introspection_Value (Receiver.Node.Text);
      end if;

      raise Introspection_Error with
        "Invalid built-in property: " & To_UTF8 (Property_Name);
   end Built_In_Field;

   ------------------------------
   -- Make_Introspection_Value --
   ------------------------------

   function Make_Introspection_Value
     (Value : Text_Type) return Introspection_Value
   is
     (Kind => Kind_Text, Text_Val => To_Unbounded_Text (Value));

   ------------------------------
   -- Make_Introspection_Value --
   ------------------------------

   function Make_Introspection_Value
     (Value : I.Value_Type) return Introspection_Value
   is
   begin
      case Kind (Value) is
         when Boolean_Value =>
            return (Kind => Kind_Bool, Bool_Val => As_Boolean (Value));
         when Integer_Value =>
            return (Kind => Kind_Int, Int_Val => As_Integer (Value));
         when Node_Value =>
            return (Kind     => Kind_Node,
                    Node_Val => new Ada_AST_Node'(Node => As_Node (Value)));
         when Text_Type_Value =>
            return (Kind     => Kind_Text,
                    Text_Val => To_Unbounded_Text (As_Text_Type (Value)));
         when Unbounded_Text_Value =>
            return (Kind => Kind_Text, Text_Val => As_Unbounded_Text (Value));
         when Base_Type_Decl_Array_Value =>
            return Make_Introspection_Value (As_Base_Type_Decl_Array (Value));
         when Ada_Node_Array_Value =>
            return Make_Introspection_Value (As_Ada_Node_Array (Value));
         when Generic_Instantiation_Array_Value =>
            return Make_Introspection_Value
              (As_Generic_Instantiation_Array (Value));
         when Expr_Array_Value =>
            return Make_Introspection_Value (As_Expr_Array (Value));
         when Base_Formal_Param_Decl_Array_Value =>
            return Make_Introspection_Value
              (As_Base_Formal_Param_Decl_Array (Value));
         when Type_Decl_Array_Value =>
            return Make_Introspection_Value (As_Type_Decl_Array (Value));
         when Defining_Name_Array_Value =>
            return Make_Introspection_Value (As_Defining_Name_Array (Value));
         when Param_Spec_Array_Value =>
            return Make_Introspection_Value (As_Param_Spec_Array (Value));
         when Basic_Decl_Array_Value =>
            return Make_Introspection_Value (As_Basic_Decl_Array (Value));
         when Compilation_Unit_Array_Value =>
            return Make_Introspection_Value
              (As_Compilation_Unit_Array (Value));
         when others =>
            raise Introspection_Error with
              "Unsupported value type from the introspection API: " &
              Value_Kind'Image (Kind (Value));
      end case;
   end Make_Introspection_Value;

   ----------------------
   -- Get_Property_Ref --
   ----------------------

   function Get_Property_Ref (Node          : Ada_AST_Node;
                              Property_Name : Text_Type)
                              return Property_Reference
   is
      Ref : constant Any_Member_Reference :=
        Data_Reference_For_Name (Node, Property_Name);
   begin
      if not (Ref in Property_Reference) then
         raise Introspection_Error with "No property named: "
           & To_UTF8 (Property_Name);
      end if;

      return Property_Reference (Ref);
   end Get_Property_Ref;

   ---------------------
   -- Make_Value_Type --
   ---------------------

   function Make_Value_Type (Value       : Introspection_Value;
                             Target_Kind : Value_Kind)
                             return I.Value_Type
   is
   begin
      if Value.Kind = Kind_Node_Array then
         return Array_To_Value_Type (Value.Node_Array_Val.all, Target_Kind);
      elsif Value.Kind = Kind_Text then
         return String_To_Value_Type (Value.Text_Val, Target_Kind);
      elsif Value.Kind = Kind_Int and then Target_Kind = Integer_Value then
         return Create_Integer (Value.Int_Val);
      elsif Value.Kind = Kind_Bool and then Target_Kind = Boolean_Value then
         return Create_Boolean (Value.Bool_Val);
      elsif Value.Kind = Kind_Node and then Target_Kind = Node_Value then
         return Create_Node
           (Introspection_Node_To_Ada_Node (Value.Node_Val).Node);
      end if;

      raise Introspection_Error with "Cannot convert a " &
        Introspection_Value_Kind'Image (Value.Kind) & " to a " &
        Value_Kind'Image (Target_Kind);
   end Make_Value_Type;

   -------------------------
   -- Array_To_Value_Type --
   -------------------------

   function Array_To_Value_Type (Value       : AST_Node_Array;
                                 Target_Kind : Value_Kind)
                                 return I.Value_Type
   is
   begin
      case Target_Kind is
         when Base_Type_Decl_Array_Value =>
            return Create_Base_Type_Decl_Array
              (Base_Type_Decl_Array_From_AST_Array (Value));
         when Ada_Node_Array_Value =>
            return Create_Ada_Node_Array
              (Ada_Node_Array_From_AST_Array (Value));
         when Generic_Instantiation_Array_Value =>
            return Create_Generic_Instantiation_Array
              (Generic_Instantiation_Array_From_AST_Array (Value));
         when Expr_Array_Value =>
            return Create_Expr_Array
              (Expr_Array_From_AST_Array (Value));
         when Base_Formal_Param_Decl_Array_Value =>
            return Create_Base_Formal_Param_Decl_Array
              (Base_Formal_Param_Decl_Array_From_AST_Array (Value));
         when Type_Decl_Array_Value =>
            return Create_Type_Decl_Array
              (Type_Decl_Array_From_AST_Array (Value));
         when Defining_Name_Array_Value =>
            return Create_Defining_Name_Array
              (Defining_Name_Array_From_AST_Array (Value));
         when Param_Spec_Array_Value =>
            return Create_Param_Spec_Array
              (Param_Spec_Array_From_AST_Array (Value));
         when Basic_Decl_Array_Value =>
            return Create_Basic_Decl_Array
              (Basic_Decl_Array_From_AST_Array (Value));
         when Compilation_Unit_Array_Value =>
            return Create_Compilation_Unit_Array
              (Compilation_Unit_Array_From_AST_Array (Value));
         when others =>
            raise Introspection_Error with "Cannot create Value_Type of kind" &
              Value_Kind'Image (Target_Kind) & " from an introspection " &
              "value array";
      end case;
   end Array_To_Value_Type;

   --------------------------
   -- String_To_Value_Type --
   --------------------------

   function String_To_Value_Type (Value       : Unbounded_Text_Type;
                                  Target_Kind : Value_Kind)
                                  return I.Value_Type
   is
   begin
      if Target_Kind = Unbounded_Text_Value then
         return Create_Unbounded_Text (Value);
      elsif Target_Kind = Text_Type_Value then
         return Create_Text_Type (To_Text (Value));
      elsif Target_Kind = Character_Value and then Length (Value) = 1 then
         return Create_Character (Element (Value, 1));
      end if;

      raise Introspection_Error with "Cannot create a value of kind" &
        Value_Kind'Image (Target_Kind) & " from an unbounded String";
   end String_To_Value_Type;

   package Names_To_Node_Types_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Unbounded_Text_Type,
      Element_Type    => Node_Type_Id,
      Equivalent_Keys => "=",
      Hash            => Wide_Wide_Hash);

   Names_To_Node_Types : Names_To_Node_Types_Maps.Map;

   -----------------------
   -- Matches_Kind_Name --
   -----------------------

   overriding function Matches_Kind_Name
     (Node : Ada_AST_Node; Kind_Name : Text_Type) return Boolean
   is
      Expected_Kind : Any_Node_Type_Id;
      Actual_Kind   : constant Any_Node_Type_Id :=
        Id_For_Kind (Node.Node.Kind);
   begin
      begin
         Expected_Kind :=
           Names_To_Node_Types.Element (To_Unbounded_Text (Kind_Name));
      exception
         when Constraint_Error =>
            raise Unsupported_Error
              with "Invalid kind name: " & Image (Kind_Name);
      end;

      return Actual_Kind = Expected_Kind or else
        Is_Derived_From (Actual_Kind, Expected_Kind);
   end Matches_Kind_Name;

   -------------------
   -- Is_Field_Name --
   -------------------

   overriding function Is_Field_Name
     (Node : Ada_AST_Node; Name : Text_Type) return Boolean
   is
      Data_Ref : constant Any_Member_Reference :=
        Data_Reference_For_Name (Node, Name);
   begin
      return (Data_Ref in Member_Reference) or else
        (Data_Ref in Built_In_LAL_Field) or else
        Is_Built_In (Name);
   end Is_Field_Name;

   ----------------------
   -- Is_Property_Name --
   ----------------------

   function Is_Property_Name
     (Node : Ada_AST_Node; Name : Text_Type) return Boolean
   is
     (Data_Reference_For_Name (Node, Name) in Property_Reference);

   ------------------
   -- Access_Field --
   ------------------

   overriding function Access_Field
     (Node : Ada_AST_Node; Field : Text_Type) return Introspection_Value
   is
      Data_Ref : constant Any_Member_Reference :=
        Data_Reference_For_Name (Node, Field);
   begin
      if Is_Built_In (Field) then
         return Built_In_Field (Node, Field);
      end if;

      return Make_Introspection_Value
        (Eval_Member (Node.Node, Data_Ref, Empty_Value_Array));
   end Access_Field;

   --------------------
   -- Property_Arity --
   --------------------

   overriding function Property_Arity
     (Node : Ada_AST_Node; Property_Name : Text_Type) return Natural
   is
     (Property_Argument_Types
        (Get_Property_Ref (Node, Property_Name))'Length);

   ------------------------
   -- Default_Args_Value --
   ------------------------

   overriding function Default_Arg_Value (Node          : Ada_AST_Node;
                                          Property_Name : Text_Type;
                                          Arg_Position  : Positive)
                                          return Introspection_Value
   is
     (Make_Introspection_Value
        (Property_Argument_Default_Value
             (Get_Property_Ref (Node, Property_Name), Arg_Position)));

   ----------------------
   -- Evalute_Property --
   ----------------------

   function Evaluate_Property
     (Node          : Ada_AST_Node;
      Property_Name : Text_Type;
      Arguments     : Introspection_Value_Array)
      return Introspection_Value
   is
      Property_Args : Value_Array (1 .. Arguments'Length);
      Property_Ref  : constant Property_Reference :=
        Get_Property_Ref (Node, Property_Name);
      Contraints    : constant Type_Constraint_Array :=
        Property_Argument_Types (Property_Ref);
   begin
      if Arguments'Length /= Contraints'Length then
         raise Introspection_Error with "Expected " &
           Positive'Image (Contraints'Length) &  " arguments but got" &
           Positive'Image (Arguments'Length);
      end if;

      for I in Arguments'Range loop
         Property_Args (I) :=
           Make_Value_Type (Arguments (I), Contraints (I).Kind);
      end loop;

      return Make_Introspection_Value
        (Eval_Member (Node.Node, Property_Ref, Property_Args));
   end Evaluate_Property;

   ----------------
   -- Kind_Names --
   ----------------

   function Kind_Names return Unbounded_Text_Array is
      Ret : Unbounded_Text_Array
        (1 .. Positive (Names_To_Node_Types.Length));
      Idx : Positive := 1;
   begin
      for C in Names_To_Node_Types.Iterate loop
         Ret (Idx) := Names_To_Node_Types_Maps.Key (C);
         Idx := Idx + 1;
      end loop;
      return Ret;
   end Kind_Names;

   ----------
   -- Kind --
   ----------

   function Kind (Name : Text_Type) return Node_Type_Id is
   begin
      return
        Names_To_Node_Types.Element (To_Unbounded_Text (Name));
   end Kind;

   -----------------------
   -- Make_Eval_Context --
   -----------------------

   function Make_Eval_Context
     (Units        : Unit_Vectors.Vector;
      Analysis_Ctx : L.Analysis_Context := L.No_Analysis_Context;
      Err_Recovery : Boolean := False) return Eval_Context
   is
      Roots : AST_Node_Array (1 .. Units.Last_Index);
   begin

      for I in Units.First_Index .. Units.Last_Index
      loop
         Roots (I) := new Ada_AST_Node'(Node => Units.Element (I).Root);
      end loop;

      return Make_Eval_Context
        (Roots, Make_Ada_AST_Node (No_Ada_Node), Analysis_Ctx, Err_Recovery);
   end Make_Eval_Context;

   ----------------------
   -- Get_Node_Type_Id --
   ----------------------

   function Get_Node_Type_Id (Node : Ada_AST_Node) return Node_Type_Id is
   begin
      return Id_For_Kind (Node.Node.Kind);
   end Get_Node_Type_Id;

   --  TODO??? Magnificient hack because somehow elab of libadalang is not
   --  called ..
   procedure adalanginit;
   pragma Import (C, adalanginit, "adalanginit");
begin

   adalanginit;

   for Type_Id in Node_Type_Id loop
      declare
         Type_Name : Unbounded_Text_Type :=
           To_Unbounded_Text (DSL_Name (Type_Id));
         I : Positive := 1;
      begin
         --  Remove dots
         while I < Length (Type_Name) loop
            if Element (Type_Name, I) = '.' then
               Delete (Type_Name, I, I);
               if Is_Lower (Element (Type_Name, I)) then
                  Replace_Element
                    (Type_Name, I, To_Upper (Element (Type_Name, I)));
               end if;
            else
               I := I + 1;
            end if;
         end loop;
         Names_To_Node_Types.Include (Type_Name, Type_Id);
      end;
   end loop;
end Ada_AST_Nodes;

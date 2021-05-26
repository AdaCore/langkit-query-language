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

with Langkit_Support.Symbols; use Langkit_Support.Symbols;
with Libadalang.Introspection; use Libadalang.Introspection;

with Ada.Unchecked_Conversion;
with Ada.Containers.Hashed_Maps;
with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;
with Ada.Strings.Wide_Wide_Unbounded.Wide_Wide_Hash;
with Ada.Wide_Wide_Characters.Handling; use Ada.Wide_Wide_Characters.Handling;

with GNATCOLL.GMP.Integers;

with LKQL.Adaptive_Integers; use LKQL.Adaptive_Integers;

package body Ada_AST_Nodes is

   package I renames Libadalang.Introspection;

   Empty_Value_Array : constant Value_Array (1 .. 0) := (others => <>);
   --  Empty Array of Value_Type values

   function Make_Primitive
     (Ctx : Eval_Context; Value : I.Value_Type) return Primitive;
   --  Create an Introspection_value value from the given Value_type value

   function Make_Value_Type (Value       : Primitive;
                             Target_Kind : Value_Kind)
                             return I.Value_Type;
   --  Create a Value_Type value from the given Introspection value

   function List_To_Value_Type (Value       : Primitive_List;
                                 Target_Kind : Value_Kind)
                                 return I.Value_Type;

   function String_To_Value_Type (Value       : Unbounded_Text_Type;
                                  Target_Kind : Value_Kind)
                                  return I.Value_Type;

   function Get_Kind_From_Name
     (Kind_Name : Text_Type) return Ada_AST_Node_Kind
   is
      Type_Id : constant Any_Node_Type_Id := Kind (Kind_Name);
   begin
      return
        (First => I.First_Kind_For (Type_Id),
         Last  => I.Last_Kind_For (Type_Id));
   end Get_Kind_From_Name;

   overriding function Matches_Kind_Of
     (Self : Ada_AST_Node_Kind; Node : AST_Node'Class) return Boolean
   is
      Ada_Node : constant Ada_AST_Node := Ada_AST_Node (Node);
   begin
      return Ada_Node.Node.Kind in Self.First .. Self.Last;
   end Matches_Kind_Of;

   overriding function "=" (Left, Right : Ada_AST_Node) return Boolean is
     (Left.Node = Right.Node);

   overriding function Hash (Node : Ada_AST_Node) return Hash_Type is
     (Hash (Node.Node));

   overriding function Text_Image (Node : Ada_AST_Node) return Text_Type is
     (To_Text (Node.Node.Image));

   overriding function Text (Node : Ada_AST_Node) return Text_Type is
     (Node.Node.Text);

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

   function Make_Ada_AST_Node (Node : Ada_Node) return H.AST_Node_Holder
   is (Create_Node (Ada_AST_Node'(Node => Node)));

   --------------------------------------------------
   -- Node array to introspection value conversion --
   --------------------------------------------------

   generic
      type Array_Elem is new Ada_Node with private;

      type Array_Type is array (Positive range <>) of Array_Elem;

   function Primitive_From_Array
     (Nodes : Array_Type) return Primitive;

   ------------------------------------
   -- Introspection_Value_From_Array --
   ------------------------------------

   function Primitive_From_Array
     (Nodes : Array_Type) return Primitive
   is
      Res : constant Primitive := Make_Empty_List;
   begin
      for I in Nodes'Range loop
         Res.Unchecked_Get.List_Val.Elements.Append
           (To_Primitive
              (Create_Node
                   (Ada_AST_Node'(Node => Nodes (I).As_Ada_Node))));
      end loop;

      return Res;
   end Primitive_From_Array;

   function Make_Primitive is new
     Primitive_From_Array (Base_Type_Decl, Base_Type_Decl_Array);

   function Make_Primitive is new
     Primitive_From_Array (Ada_Node, Ada_Node_Array);

   function Make_Primitive is new
     Primitive_From_Array
       (Generic_Instantiation, Generic_Instantiation_Array);

   function Make_Primitive is new
     Primitive_From_Array (Expr, Expr_Array);

   function Make_Primitive is new
     Primitive_From_Array
       (Base_Formal_Param_Decl, Base_Formal_Param_Decl_Array);

   function Make_Primitive is new
     Primitive_From_Array (Type_Decl, Type_Decl_Array);

   function Make_Primitive is new
     Primitive_From_Array (Defining_Name, Defining_Name_Array);

   function Make_Primitive is new
     Primitive_From_Array (Param_Spec, Param_Spec_Array);

   function Make_Primitive is new
     Primitive_From_Array (Basic_Decl, Basic_Decl_Array);

   function Make_Primitive is new
     Primitive_From_Array (Compilation_Unit, Compilation_Unit_Array);

   -----------------------------------------------
   -- Node array to Value_Type array conversion --
   -----------------------------------------------

   generic

      type Node_Type is new Ada_Node with private;

      type Node_Array is array (Positive range <>) of Node_Type;

      with function Convert (Node : Ada_Node'Class) return Node_Type;

   function Node_Array_From_List
     (Nodes : Primitive_List) return Node_Array;

   function To_Ada_Node is new Ada.Unchecked_Conversion
     (H.AST_Node_Access, Ada_AST_Node_Access);

   --------------------------
   -- Node_Array_From_List --
   --------------------------

   function Node_Array_From_List
     (Nodes : Primitive_List) return Node_Array
   is
      Result       : Node_Array (1 .. Natural (Nodes.Elements.Length));
   begin
      for I in Nodes.Elements.First_Index .. Nodes.Elements.Last_Index loop
         Result (I) :=
           Convert (To_Ada_Node
                    (Nodes.Elements (I)
                       .Unchecked_Get.Node_Val.Unchecked_Get).Node);
      end loop;

      return Result;
   end Node_Array_From_List;

   function Base_Type_Decl_Array_From_List is new Node_Array_From_List
     (Base_Type_Decl,
      Base_Type_Decl_Array,
      As_Base_Type_Decl);

   function Ada_Node_Array_From_List is new Node_Array_From_List
     (Ada_Node,
      Ada_Node_Array,
      As_Ada_Node);

   function Generic_Instantiation_Array_From_List
   is new Node_Array_From_List
     (Generic_Instantiation,
      Generic_Instantiation_Array,
      As_Generic_Instantiation);

   function Expr_Array_From_List is new Node_Array_From_List
     (Expr,
      Expr_Array,
      As_Expr);

   function Base_Formal_Param_Decl_Array_From_List
   is new Node_Array_From_List
     (Base_Formal_Param_Decl,
      Base_Formal_Param_Decl_Array,
      As_Base_Formal_Param_Decl);

   function Type_Decl_Array_From_List is new Node_Array_From_List
     (Type_Decl,
      Type_Decl_Array,
      As_Type_Decl);

   function Defining_Name_Array_From_List is new Node_Array_From_List
     (Defining_Name,
      Defining_Name_Array,
      As_Defining_Name);

   function Param_Spec_Array_From_List is new Node_Array_From_List
     (Param_Spec,
      Param_Spec_Array,
      As_Param_Spec);

   function Basic_Decl_Array_From_List is new Node_Array_From_List
     (Basic_Decl,
      Basic_Decl_Array,
      As_Basic_Decl);

   function Compilation_Unit_Array_From_List is new Node_Array_From_List
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

   ------------------------------
   -- Make_Primitive --
   ------------------------------

   function Make_Primitive
     (Ctx : Eval_Context; Value : I.Value_Type) return Primitive
   is
   begin
      case Kind (Value) is
         when Boolean_Value =>
            return To_Primitive (As_Boolean (Value));
         when Integer_Value =>
            return To_Primitive (As_Integer (Value));
         when Big_Integer_Value =>
            return To_Primitive
              (Create (GNATCOLL.GMP.Integers.Image
               (As_Big_Integer (Value))));
         when Node_Value =>
            return To_Primitive
              (Create_Node (Ada_AST_Node'(Node => As_Node (Value))),
               Nullable => True);
         when Text_Type_Value =>
            return To_Primitive (To_Unbounded_Text (As_Text_Type (Value)));
         when Unbounded_Text_Value =>
            return To_Primitive (As_Unbounded_Text (Value));
         when Base_Type_Decl_Array_Value =>
            return Make_Primitive (As_Base_Type_Decl_Array (Value));
         when Ada_Node_Array_Value =>
            return Make_Primitive (As_Ada_Node_Array (Value));
         when Generic_Instantiation_Array_Value =>
            return Make_Primitive
              (As_Generic_Instantiation_Array (Value));
         when Expr_Array_Value =>
            return Make_Primitive (As_Expr_Array (Value));
         when Base_Formal_Param_Decl_Array_Value =>
            return Make_Primitive
              (As_Base_Formal_Param_Decl_Array (Value));
         when Type_Decl_Array_Value =>
            return Make_Primitive (As_Type_Decl_Array (Value));
         when Defining_Name_Array_Value =>
            return Make_Primitive (As_Defining_Name_Array (Value));
         when Param_Spec_Array_Value =>
            return Make_Primitive (As_Param_Spec_Array (Value));
         when Basic_Decl_Array_Value =>
            return Make_Primitive (As_Basic_Decl_Array (Value));
         when Compilation_Unit_Array_Value =>
            return Make_Primitive
              (As_Compilation_Unit_Array (Value));
         when Struct_Value_Kind =>
            --  Structs are mapped to LKQL objects
            declare
               Fields : constant Struct_Field_Reference_Array :=
                 Struct_Fields (Kind (Value));
               Ret    : constant Primitive := Make_Empty_Object;
            begin
               for Field of Fields loop
                  Ret.Unchecked_Get.Obj_Assocs.Elements.Include
                    (Find (Get_Context (Ctx.Kernel.all).Get_Symbol_Table,
                     Member_Name (Field)),
                     Make_Primitive (Ctx, Eval_Member (Value, Field)));
               end loop;
               return Ret;
            end;
         when others =>
            raise Introspection_Error with
              "Unsupported value type from the introspection API: " &
              Value_Kind'Image (Kind (Value));
      end case;
   end Make_Primitive;

   ---------------------
   -- Make_Value_Type --
   ---------------------

   function Make_Value_Type (Value       : Primitive;
                             Target_Kind : Value_Kind)
                             return I.Value_Type
   is
   begin
      if Value.Get.Kind = Kind_List then
         return List_To_Value_Type (Value.Unchecked_Get.List_Val.all,
                                    Target_Kind);
      elsif Value.Get.Kind = Kind_Str then
         return String_To_Value_Type
           (Value.Unchecked_Get.Str_Val, Target_Kind);
      elsif Value.Get.Kind = Kind_Int and then Target_Kind = Integer_Value then
         return Create_Integer (+Value.Get.Int_Val);
      elsif Value.Get.Kind = Kind_Int and then Target_Kind = Big_Integer_Value
      then
         return Create_Big_Integer
           (GNATCOLL.GMP.Integers.Make (Image (Value.Get.Int_Val)));
      elsif Value.Get.Kind = Kind_Bool and then Target_Kind = Boolean_Value
      then
         return Create_Boolean (Value.Get.Bool_Val);
      elsif Value.Get.Kind = Kind_Node and then Target_Kind = Node_Value then
         return Create_Node
           (To_Ada_Node (Value.Get.Node_Val.Unchecked_Get).Node);
      end if;

      raise Introspection_Error with "Cannot convert a " &
        Valid_Primitive_Kind'Image (Value.Get.Kind) & " to a " &
        Value_Kind'Image (Target_Kind);
   end Make_Value_Type;

   ------------------------
   -- List_To_Value_Type --
   ------------------------

   function List_To_Value_Type
     (Value       : Primitive_List;
      Target_Kind : Value_Kind)
      return I.Value_Type
   is
   begin
      case Target_Kind is
         when Base_Type_Decl_Array_Value =>
            return Create_Base_Type_Decl_Array
              (Base_Type_Decl_Array_From_List (Value));
         when Ada_Node_Array_Value =>
            return Create_Ada_Node_Array
              (Ada_Node_Array_From_List (Value));
         when Generic_Instantiation_Array_Value =>
            return Create_Generic_Instantiation_Array
              (Generic_Instantiation_Array_From_List (Value));
         when Expr_Array_Value =>
            return Create_Expr_Array
              (Expr_Array_From_List (Value));
         when Base_Formal_Param_Decl_Array_Value =>
            return Create_Base_Formal_Param_Decl_Array
              (Base_Formal_Param_Decl_Array_From_List (Value));
         when Type_Decl_Array_Value =>
            return Create_Type_Decl_Array
              (Type_Decl_Array_From_List (Value));
         when Defining_Name_Array_Value =>
            return Create_Defining_Name_Array
              (Defining_Name_Array_From_List (Value));
         when Param_Spec_Array_Value =>
            return Create_Param_Spec_Array
              (Param_Spec_Array_From_List (Value));
         when Basic_Decl_Array_Value =>
            return Create_Basic_Decl_Array
              (Basic_Decl_Array_From_List (Value));
         when Compilation_Unit_Array_Value =>
            return Create_Compilation_Unit_Array
              (Compilation_Unit_Array_From_List (Value));
         when others =>
            raise Introspection_Error with "Cannot create Value_Type of kind" &
              Value_Kind'Image (Target_Kind) & " from an introspection " &
              "value array";
      end case;
   end List_To_Value_Type;

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

   -------------------
   -- Is_Field_Name --
   -------------------

   overriding function Is_Field_Name
     (Node : Ada_AST_Node; Name : Text_Type) return Boolean
   is
      Data_Ref : constant Any_Member_Reference :=
        Data_Reference_For_Name (Node, Name);
   begin
      return Data_Ref in Syntax_Field_Reference
       or Data_Ref in
          Ada_Node_Parent | Ada_Node_Children | Ada_Node_Unit
          | Ada_Node_Previous_Sibling | Ada_Node_Next_Sibling;
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
     (Node  : AST_Node'Class;
      Ref   : Ada_Member_Reference;
      Ctx   : Eval_Context) return Primitive
   is
   begin
      return Make_Primitive
        (Ctx,
         Eval_Member (Ada_AST_Node (Node).Node, Ref.Ref, Empty_Value_Array));
   end Access_Field;

   --------------------
   -- Property_Arity --
   --------------------

   overriding function Property_Arity
     (Ref   : Ada_Member_Reference) return Natural
   is
     (Property_Argument_Types (Ref.Ref)'Length);

   ----------
   -- Name --
   ----------

   overriding function Name
     (Ref : Ada_Member_Reference) return Text_Type
   is
      (Ref.Ref'Wide_Wide_Image);

   ------------------------
   -- Default_Args_Value --
   ------------------------

   overriding function Default_Arg_Value
     (Ref           : Ada_Member_Reference;
      Arg_Position  : Positive;
      Ctx           : Eval_Context) return Primitive
   is
      V : constant Any_Value_Type :=
        Property_Argument_Default_Value (Ref.Ref, Arg_Position);
   begin
      if V = No_Value then
         return Primitive_Ptrs.Null_Ref;
      else
         return Make_Primitive (Ctx, V);
      end if;
   end Default_Arg_Value;

   ----------------------
   -- Evalute_Property --
   ----------------------

   function Evaluate_Property
     (Ref           : Ada_Member_Reference;
      Node          : AST_Node'Class;
      Arguments     : Primitive_List;
      Ctx           : Eval_Context)
      return Primitive
   is
      Args_Length : constant Natural := Natural (Arguments.Elements.Length);
      Property_Args : Value_Array (1 .. Args_Length);
      Contraints    : constant Type_Constraint_Array :=
        Property_Argument_Types (Ref.Ref);
   begin
      if Args_Length /= Contraints'Length then
         raise Introspection_Error with "Expected " &
           Positive'Image (Contraints'Length) &  " arguments but got" &
           Positive'Image (Args_Length);
      end if;

      for I in Arguments.Elements.First_Index .. Arguments.Elements.Last_Index
      loop
         Property_Args (I) :=
           Make_Value_Type (Arguments.Elements (I), Contraints (I).Kind);
      end loop;

      return Make_Primitive
        (Ctx, Eval_Member (Ada_AST_Node (Node).Node, Ref.Ref, Property_Args));
   end Evaluate_Property;

   --------------------------
   -- Get_Member_Reference --
   --------------------------

   overriding function Get_Member_Reference
     (Node : Ada_AST_Node;
      Name : Text_Type) return AST_Node_Member_Reference'Class
   is
   begin
      return Ada_Member_Reference'
        (Ref => Data_Reference_For_Name (Node, Name));
   end Get_Member_Reference;

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
      use Names_To_Node_Types_Maps;

      Cur : constant Cursor :=
        Names_To_Node_Types.Find (To_Unbounded_Text (Name));
   begin
      if Cur /= No_Element then
         return Element (Cur);
      else
         raise Unsupported_Error;
      end if;
   end Kind;

   -----------------------
   -- Make_Eval_Context --
   -----------------------

   function Make_Eval_Context
     (Units        : Unit_Vectors.Vector;
      Analysis_Ctx : L.Analysis_Context := L.No_Analysis_Context)
      return Eval_Context
   is
      Roots : AST_Node_Array (1 .. Units.Last_Index);
   begin

      for I in Units.First_Index .. Units.Last_Index
      loop
         Roots (I) := Create_Node
           (Ada_AST_Node'(Node => Units.Element (I).Root));
      end loop;

      return Make_Eval_Context
        (Roots, Make_Ada_AST_Node (No_Ada_Node), Analysis_Ctx);
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

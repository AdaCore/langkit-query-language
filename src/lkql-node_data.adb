with LKQL.Evaluation;     use LKQL.Evaluation;
with LKQL.Error_Handling; use LKQL.Error_Handling;

with Ada.Assertions; use Ada.Assertions;
with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;

package body LKQL.Node_Data is

   Empty_Value_Array : constant Value_Array (1 .. 0) := (others => <>);

   subtype Built_In_LAL_Field is Node_Data_Reference
      range Ada_Node_Parent .. Ada_Node_Is_Ghost;

   ---------------------------------------------
   --  Ada node array to Primitive conversion --
   ---------------------------------------------

   generic
      type Value_Type is new LAL.Ada_Node with private;

      type Value_Array is array (Positive range <>) of Value_Type;

   function List_From_Node_Array (Nodes : Value_Array) return Primitive;

   --------------------------
   -- List_From_Node_Array --
   --------------------------

   function List_From_Node_Array (Nodes : Value_Array) return Primitive is
   begin
      return Result : constant Primitive := Make_Empty_List do
         for N of Nodes loop
            Append (Result, To_Primitive (N.As_Ada_Node));
         end loop;
      end return;
   end List_From_Node_Array;

   function List_From_Ada_Nodes is new List_From_Node_Array
     (LAL.Ada_Node, LAL.Ada_Node_Array);

   function List_From_Basic_Decls is new List_From_Node_Array
     (LAL.Basic_Decl, LAL.Basic_Decl_Array);

   function List_From_Base_Formal_Param_Decls is new List_From_Node_Array
     (LAL.Base_Formal_Param_Decl, LAL.Base_Formal_Param_Decl_Array);

   function List_From_Generic_Instanciations is new List_From_Node_Array
     (LAL.Generic_Instantiation, LAL.Generic_Instantiation_Array);

   function List_From_Defining_Names is new List_From_Node_Array
     (LAL.Defining_Name, LAL.Defining_Name_Array);

   --------------------------------------------
   -- Primitive to Ada node array conversion --
   --------------------------------------------

   generic
      type Value_Type is new LAL.Ada_Node with private;

      type Value_Array is array (Positive range <>) of Value_Type;

      with function Convert (Node : LAL.Ada_Node'Class) return Value_Type;

   function Node_Array_From_List (Nodes : Primitive) return Value_Array;

   --------------------------
   -- Node_Array_From_List --
   --------------------------

   function Node_Array_From_List (Nodes : Primitive) return Value_Array is
      Elements     : constant Primitive :=
        (if Kind (Nodes) = Kind_List
         then Nodes
         else To_List (Iter_Val (Nodes).all));
      Elements_Vec : constant Primitive_Vectors.Vector :=
        List_Val (Elements).Elements;
      Result       : Value_Array (1 .. Length (Elements));
   begin
      for I in Elements_Vec.First_Index .. Elements_Vec.Last_Index loop
         Result (I) := Convert (Node_Val (Elements_Vec (I)));
      end loop;

      return Result;
   end Node_Array_From_List;

   function Ada_Node_Array_From_List is new Node_Array_From_List
     (LAL.Ada_Node, LAL.Ada_Node_Array, LAL.As_Ada_Node);

   function Base_Formal_Param_Decl_Array_From_List is new Node_Array_From_List
     (LAL.Base_Formal_Param_Decl,
      LAL.Base_Formal_Param_Decl_Array,
      LAL.As_Base_Formal_Param_Decl);

   function Basic_Decl_Array_From_List is new Node_Array_From_List
     (LAL.Basic_Decl, LAL.Basic_Decl_Array, LAL.As_Basic_Decl);

   function Defining_Name_Array_From_List is new Node_Array_From_List
     (LAL.Defining_Name, LAL.Defining_Name_Array, LAL.As_Defining_Name);

   function Generic_Instantiation_Array_From_List is new Node_Array_From_List
     (LAL.Generic_Instantiation,
      LAL.Generic_Instantiation_Array,
      LAL.As_Generic_Instantiation);

   function Param_Spec_Array_From_List is new Node_Array_From_List
     (LAL.Param_Spec, LAL.Param_Spec_Array, LAL.As_Param_Spec);

   -------------------
   -- Is_Field_Name --
   -------------------

   function Is_Field_Name (Receiver : LAL.Ada_Node;
                           Name     : Text_Type) return Boolean
   is
      Data_Ref : constant Any_Node_Data_Reference :=
        Data_Reference_For_Name (Receiver, Name);
   begin
      return (Data_Ref in Field_Reference) or else
             (Data_Ref in Built_In_LAL_Field);
   end Is_Field_Name;

   ----------------------
   -- Is_Property_Name --
   ----------------------

   function Is_Property_Name (Receiver : LAL.Ada_Node;
                              Name     : Text_Type) return Boolean
   is (Data_Reference_For_Name (Receiver, Name) in Property_Reference);

   -----------------------
   -- Access_Node_Field --
   -----------------------

   function Access_Node_Field (Ctx        : Eval_Context;
                               Receiver   : LAL.Ada_Node;
                               Field_Name : L.Identifier) return Primitive
   is
      Data_Ref : constant Any_Node_Data_Reference :=
         Data_Reference_For_Name (Receiver, Field_Name.Text);
   begin
      if Is_Built_In (Field_Name.Text) then
         return Built_In_Field (Receiver, Field_Name.Text);
      end if;

      if not Is_Field_Name (Receiver, Field_Name.Text) then
         Raise_No_Such_Field (Ctx, Receiver, Field_Name);
      end if;

      return Access_Node_Field (Ctx, Receiver, Field_Name, Data_Ref);
   end Access_Node_Field;

   ------------------------
   --  Access_Node_Field --
   ------------------------

   function Access_Node_Field (Ctx             : Eval_Context;
                               Receiver        : LAL.Ada_Node;
                               Field_Name      : L.Identifier;
                               Field_Reference : Node_Data_Reference)
                               return Primitive
   is
      Result : constant Value_Type :=
        Eval_Node_Data (Receiver, Field_Reference, Empty_Value_Array);
   begin
      return Create_Primitive (Ctx, Field_Name, Result);
   end Access_Node_Field;

   ------------------------
   -- Eval_Node_Property --
   ------------------------

   function Eval_Node_Property (Ctx : Eval_Context;
                                Receiver : LAL.Ada_Node;
                                Property_Name : L.Identifier;
                                Args          : L.Arg_List) return Primitive
   is
      Data_Ref : constant Any_Node_Data_Reference :=
         Data_Reference_For_Name (Receiver, Property_Name.Text);
   begin
      if not (Data_Ref in Property_Reference) then
         Raise_No_Such_Property (Ctx, Receiver, Property_Name);
      end if;

      return Eval_Node_Property (Ctx, Receiver, Data_Ref, Property_Name, Args);
   end Eval_Node_Property;

   ------------------------
   -- Eval_Node_Property --
   ------------------------

   function Eval_Node_Property (Ctx        : Eval_Context;
                                Receiver   : LAL.Ada_Node;
                                Data_Ref   : Property_Reference;
                                Identifier : L.Identifier;
                                Args       : L.Arg_List) return Primitive
   is
      Data_Arguments : constant Value_Array :=
        Value_Array_From_Args (Ctx, Data_Ref, Args);
      Result         : constant Value_Type :=
        Eval_Node_Data (Receiver, Data_Ref, Data_Arguments);
   begin
      return Create_Primitive (Ctx, Identifier.As_LKQL_Node, Result);
   end Eval_Node_Property;

   function Nth_Arg (Ctx            : Eval_Context;
                     Property_Ref   : Property_Reference;
                     Args           : L.Arg_List;
                     Arguments_Type : Value_Constraint_Array;
                     N              : Positive)
                     return Value_Type
      with Pre => N <= Arguments_Type'Length;
   --  Return the value of the nth argument in a call to the property
   --  designated by 'Property_Ref'.

   --------------------------
   -- Value_Array_From_Arg --
   --------------------------

   function Value_Array_From_Args (Ctx          : Eval_Context;
                                   Data_Ref     : Node_Data_Reference;
                                   Args         : L.Arg_List)
                                   return Value_Array
   is
      Arguments_Type     : constant Value_Constraint_Array :=
        Property_Argument_Types (Data_Ref);
      Result : Value_Array (1 .. Arguments_Type'Length);
   begin
      if Arguments_Type'Length = 0 then
         return Result;
      end if;

      if Args.Children_Count > Arguments_Type'Length then
         Raise_Invalid_Arity (Ctx, Arguments_Type'Length, Args);
      end if;

      for I in Arguments_Type'Range loop
         Result (I) := Nth_Arg (Ctx, Data_Ref, Args, Arguments_Type, I);

         if Result (I) = No_Value then
            Raise_Invalid_Arity (Ctx, Arguments_Type'Length, Args);
         end if;
      end loop;

      return Result;
   end Value_Array_From_Args;

   -------------
   -- Nth_Arg --
   -------------

   function Nth_Arg (Ctx            : Eval_Context;
                     Property_Ref   : Property_Reference;
                     Args           : L.Arg_List;
                     Arguments_Type : Value_Constraint_Array;
                     N              : Positive)
                     return Value_Type
   is
   begin
      if N > Args.Children_Count then
         return Property_Argument_Default_Value (Property_Ref, N);
      else
         declare
            Arg       : constant L.Arg := Args.List_Child (N);
            Arg_Value : constant Primitive := Eval (Ctx, Arg.P_Expr);
            Arg_Type  : constant Value_Kind := Arguments_Type (N).Kind;
         begin
            return To_Value_Type (Ctx, Arg.P_Expr, Arg_Value, Arg_Type);
         end;
      end if;
   end Nth_Arg;

   -----------------------------
   -- Data_Reference_For_Name --
   -----------------------------

   function Data_Reference_For_Name
     (Receiver : LAL.Ada_Node; Name : Text_Type) return Any_Node_Data_Reference
   is
      Receiver_Type_Id : constant Node_Type_Id :=
        Id_For_Kind (Receiver.Kind);
   begin
      return Lookup_Node_Data (Receiver_Type_Id, To_UTF8 (Name));
   end Data_Reference_For_Name;

   ---------------------
   -- Create_Primitve --
   ---------------------

   function Create_Primitive (Ctx    : Eval_Context;
                              Member : L.LKQL_Node'Class;
                              Value  : Value_Type) return Primitive
   is
   begin
      case Kind (Value) is
         when Boolean_Value =>
            return To_Primitive (As_Boolean (Value));
         when Integer_Value =>
            return To_Primitive (As_Integer (Value));
         when Character_Value =>
            return To_Primitive
              (Character_Type'Wide_Wide_Image ((As_Character (Value))));
         when Unbounded_Text_Value =>
            return To_Primitive (As_Unbounded_Text (Value));
         when Node_Value =>
            return To_Primitive (As_Node (Value));
         when Ada_Node_Array_Value =>
            return List_From_Ada_Nodes (As_Ada_Node_Array (Value));
         when Basic_Decl_Array_Value =>
            return List_From_Basic_Decls (As_Basic_Decl_Array (Value));
         when Defining_Name_Array_Value =>
            return List_From_Defining_Names (As_Defining_Name_Array (Value));
         when Base_Formal_Param_Decl_Array_Value =>
            return List_From_Base_Formal_Param_Decls
              (As_Base_Formal_Param_Decl_Array (Value));
         when Generic_Instantiation_Array_Value =>
            return List_From_Generic_Instanciations
              (As_Generic_Instantiation_Array (Value));
         when others =>
            Raise_Unsupported_Value_Type
              (Ctx, Member.As_LKQL_Node, Kind (Value));
      end case;
   end Create_Primitive;

   function Sequence_To_Value_Type (Ctx         : Eval_Context;
                                    Value_Expr  : L.Expr;
                                    Value       : Primitive;
                                    Target_Kind : Value_Kind)
                                    return Value_Type
     with Pre => Kind (Value) in Sequence_Kind;
   --  Convert a list or iterator Primtive to a Value_Type value of kind
   --  'Target_Kind'.
   --  An exception will be raised if the conversion is invalid.

   function String_To_Value_Type (Ctx         : Eval_Context;
                                  Value_Expr  : L.Expr;
                                  Value       : Unbounded_Text_Type;
                                  Target_Kind : Value_Kind)
                                  return Value_Type;
   --  Convert a String Primitive to a Value_Type value of kind 'Target_Kind'.
   --  An exception will be raised if the conversion is invalid.

   -------------------
   -- To_Value_Type --
   -------------------

   function To_Value_Type (Ctx         : Eval_Context;
                           Value_Expr  : L.Expr;
                           Value       : Primitive;
                           Target_Kind : Value_Kind) return Value_Type
   is
   begin
      if Kind (Value) in Sequence_Kind then
         return Sequence_To_Value_Type (Ctx, Value_Expr, Value, Target_Kind);
      elsif Kind (Value) = Kind_Str then
         return String_To_Value_Type
           (Ctx, Value_Expr, Str_Val (Value), Target_Kind);
      elsif Target_Kind = Integer_Value and then Kind (Value) = Kind_Int then
         return Create_Integer (Int_Val (Value));
      elsif Target_Kind = Boolean_Value and then Kind (Value) = Kind_Bool then
         return Create_Boolean (Bool_Val (Value));
      elsif Target_Kind = Node_Value and then Kind (Value) = Kind_Node then
         return Create_Node (Node_Val (Value));
      end if;

      Raise_Invalid_Type_Conversion (Ctx, Value_Expr, Value, Target_Kind);
   end To_Value_Type;

   ----------------------------
   -- Sequence_To_Value_Type --
   ----------------------------

   function Sequence_To_Value_Type (Ctx         : Eval_Context;
                                    Value_Expr  : L.Expr;
                                    Value       : Primitive;
                                    Target_Kind : Value_Kind)
                                    return Value_Type
   is
   begin
      case Target_Kind is
         when Ada_Node_Array_Value =>
            return Create_Ada_Node_Array (Ada_Node_Array_From_List (Value));
         when Base_Formal_Param_Decl_Array_Value =>
            return Create_Base_Formal_Param_Decl_Array
              (Base_Formal_Param_Decl_Array_From_List (Value));
         when Basic_Decl_Array_Value =>
            return Create_Basic_Decl_Array
              (Basic_Decl_Array_From_List (Value));
         when Defining_Name_Array_Value =>
            return Create_Defining_Name_Array
              (Defining_Name_Array_From_List (Value));
         when Generic_Instantiation_Array_Value =>
            return Create_Generic_Instantiation_Array
              (Generic_Instantiation_Array_From_List (Value));
         when Param_Spec_Array_Value =>
            return Create_Param_Spec_Array
              (Param_Spec_Array_From_List (Value));
         when others =>
            Raise_Invalid_Type_Conversion
              (Ctx, Value_Expr, Value, Target_Kind);
      end case;
   end Sequence_To_Value_Type;

   --------------------------
   -- String_To_Value_Type --
   --------------------------

   function String_To_Value_Type (Ctx         : Eval_Context;
                                  Value_Expr  : L.Expr;
                                  Value       : Unbounded_Text_Type;
                                  Target_Kind : Value_Kind)
                                  return Value_Type
   is
   begin
      if Target_Kind = Unbounded_Text_Value then
         return Create_Unbounded_Text (Value);
      elsif Target_Kind = Text_Type_Value then
         return Create_Text_Type (To_Text (Value));
      elsif Target_Kind = Character_Value and then Length (Value) = 1 then
         return Create_Character (Element (Value, 1));
      end if;

      Raise_Invalid_Type_Conversion
              (Ctx, Value_Expr, To_Primitive (Value), Target_Kind);
   end String_To_Value_Type;

   --------------------
   -- Built_In_Field --
   --------------------

   function Built_In_Field
     (Receiver : LAL.Ada_Node; Property_Name : Text_Type) return Primitive
   is
   begin
      if Property_Name = "image" then
         return To_Primitive (To_Unbounded_Text (Receiver.Text_Image));
      elsif Property_Name = "text" then
         return To_Primitive (To_Unbounded_Text (Receiver.Text));
      end if;

      raise Assertion_Error with
        "Invalid built-in property: " & To_UTF8 (Property_Name);
   end Built_In_Field;

   -----------------
   -- Is_Built_In --
   -----------------

   function Is_Built_In (Name : Text_Type) return Boolean is
     (Name = "image" or else Name = "text");

end LKQL.Node_Data;

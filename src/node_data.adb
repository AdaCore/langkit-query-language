with Interpreter.Evaluation;     use Interpreter.Evaluation;
with Interpreter.Error_Handling; use Interpreter.Error_Handling;

with Ada.Assertions; use Ada.Assertions;
with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;

package body Node_Data is

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
         else To_List (Iter_Val (Nodes)));
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

   -----------------
   -- Access_Data --
   -----------------

   function Access_Data (Ctx             : Eval_Context;
                         Receiver        : LAL.Ada_Node;
                         Member          : L.Identifier) return Primitive
   is
      Member_Name : constant String := To_UTF8 (Member.Text);
   begin
      return (if Is_Built_In (Member_Name)
              then Built_In_Property (Receiver, Member_Name)
              else Access_Custom_Data (Ctx, Receiver, Member));
   end Access_Data;

   -------------------
   -- Call_Property --
   -------------------

   function Call_Property (Ctx           : Eval_Context;
                           Receiver      : LAL.Ada_Node;
                           Call          : L.Dot_Call) return Primitive
   is
      Property_Ref       : constant Any_Node_Data_Reference :=
        Data_Reference_For_Name (Receiver, Call.F_Member.Text);
   begin
      if Property_Ref = None or not (Property_Ref in Property_Reference'Range)
      then
         Raise_Invalid_Member (Ctx, Call.F_Member, To_Primitive (Receiver));
      end if;

      return Call_Property (Ctx, Receiver, Property_Ref, Call);
   end Call_Property;

   -------------------
   -- Call_Property --
   -------------------

   function Call_Property (Ctx          : Eval_Context;
                           Receiver     : LAL.Ada_Node;
                           Property_Ref : Property_Reference;
                           Call         : L.Dot_Call) return Primitive
   is
      Arguments          : constant L.Expr_List := Call.F_Arguments;
      Arguments_Type     : constant Value_Constraint_Array :=
        Property_Argument_Types (Property_Ref);
      Property_Arguments : Value_Array (1 .. Arguments_Type'Length);
   begin
      if Arguments_Type'Length /= Arguments.Children_Count then
         Raise_Invalid_Arity (Ctx, Arguments_Type'Length, Arguments);
      end if;

      for I in 1 .. Arguments_Type'Length loop
         declare
            Arg       : constant L.LKQL_Node := Arguments.Children (I);
            Arg_Value : constant Primitive := Eval (Ctx, Arg);
            Arg_Type  : constant Value_Kind := Arguments_Type (I).Kind;
         begin
            Property_Arguments (I) :=
              To_Value_Type (Ctx, Arg.As_Expr, Arg_Value, Arg_Type);
         end;
      end loop;

      declare
         Result : constant Value_Type :=
           Evaluate_Node_Data (Receiver, Property_Ref, Property_Arguments);
      begin
         return Create_Primitive (Ctx, Call.As_LKQL_Node, Result);
      end;
   end Call_Property;

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
                              Member : L.LKQL_Node;
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

   -----------------------
   -- Built_In_Property --
   -----------------------

   function Built_In_Property
     (Receiver : LAL.Ada_Node; Property_Name : String) return Primitive
   is
   begin
      if Property_Name = "image" then
         return To_Primitive (To_Unbounded_Text (To_Text (Receiver.Image)));
      end if;

      raise Assertion_Error with "Invalid built-in property: " & Property_Name;
   end Built_In_Property;

   -----------------
   -- Is_Built_In --
   -----------------

   function Is_Built_In (Name : String) return Boolean is
     (Name = "image");

   ------------------------
   -- Access_Custom_Data --
   ------------------------

   function Access_Custom_Data (Ctx      : Eval_Context;
                                Receiver : LAL.Ada_Node;
                                Member   : L.Identifier) return Primitive
   is
      Data_Ref   : constant Any_Node_Data_Reference :=
        Data_Reference_For_Name (Receiver, Member.Text);
      Empty_Args : constant Value_Array (1 .. 0) := (others => <>);
   begin
      if Data_Ref = None then
         Raise_Invalid_Member (Ctx, Member, To_Primitive (Receiver));
      end if;

      return Create_Primitive
        (Ctx, Member.As_LKQL_Node,
         Evaluate_Node_Data (Receiver, Data_Ref, Empty_Args));
   end Access_Custom_Data;

end Node_Data;

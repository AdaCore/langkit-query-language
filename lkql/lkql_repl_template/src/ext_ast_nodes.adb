with ${lib_name}.Common; use ${lib_name}.Common;
with ${lib_name}.Introspection; use ${lib_name}.Introspection;

with Ada.Unchecked_Conversion;
with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;

package body Ext_AST_Nodes is

   subtype Built_In_LAL_Field is Node_Data_Reference
      range ${root_type}_Parent .. ${root_type}_Is_Ghost;

   Empty_Value_Array : constant Value_Array (1 .. 0) := (others => <>);
   --  Empty Array of Value_Type values

   function Data_Reference_For_Name (Receiver : Ext_AST_Node;
                                     Name     : Text_Type)
                                     return Any_Node_Data_Reference;
   --  Return the node data type corresponding to 'Name' on the receiver
   --  node. Return None if the name is invalid.

   function Is_Built_In (Name : Text_Type) return Boolean;
   --  Return whether the property named 'Name' is built-in

   function Built_In_Field (Receiver      : Ext_AST_Node;
                            Property_Name : Text_Type)
                            return Introspection_Value;
   --  Return the value of the built-in property named 'Property_Name' on
   --  'Receiver'.

   function Make_Introspection_Value
     (Value : Text_Type) return Introspection_Value;
   --  Create an Introspection value from the given Text_Type value

   function Make_Introspection_Value
     (Value : Value_Type) return Introspection_Value;
   --  Create an Introspection_value value from the given Value_type value

   function Get_Property_Ref (Node          : Ext_AST_Node;
                              Property_Name : Text_Type)
                              return Property_Reference;
   --  Return the reference of the property named `Property_Name` on `Node`.
   --  Raise an exception if there is no such property.

   function Make_Value_Type (Value       : Introspection_Value;
                             Target_Kind : Value_Kind)
                             return Value_Type;
   --  Create a Value_Type value from the given Introspection value

   function Array_To_Value_Type (Value       : AST_Node_Array;
                                 Target_Kind : Value_Kind)
                                 return Value_Type;

   function String_To_Value_Type (Value       : Unbounded_Text_Type;
                                  Target_Kind : Value_Kind)
                                  return Value_Type;

   --------------------------------------------------
   -- Node array to introspection value conversion --
   --------------------------------------------------

   generic
      type Array_Elem is new ${root_type} with private;

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
         Result (I) := new Ext_AST_Node'(Node => Nodes (I).As_${root_type});
      end loop;

      return (Kind => Kind_Node_Array, Node_Array_Val => Result);
   end Introspection_Value_From_Array;

   % for type in node_array_types:
   function Make_Introspection_Value is new
      Introspection_Value_From_Array (${type.element_type.api_name}, ${type.api_name});

   % endfor
   -----------------------------------------------
   -- Node array to Value_Type array conversion --
   -----------------------------------------------

   generic

      type Node_Type is new ${root_type} with private;

      type Node_Array is array (Positive range <>) of Node_Type;

      with function Convert (Node : ${root_type}'Class) return Node_Type;

   function Node_Array_From_List
     (Nodes : AST_Node_Array) return Node_Array;

   function Introspection_Node_To_Ada_Node is new Ada.Unchecked_Conversion
     (AST_Node_Access, Ext_AST_Node_Access);

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

   % for type in node_array_types:
   function ${type.api_name}_From_AST_Array is new Node_Array_From_List
      (${type.element_type.api_name},
       ${type.api_name},
       As_${type.element_type.api_name});
   
   % endfor

   -----------------------------
   -- Data_Reference_For_Name --
   -----------------------------

   function Data_Reference_For_Name
     (Receiver : Ext_AST_Node; Name : Text_Type) return Any_Node_Data_Reference
   is
      Receiver_Type_Id : constant Node_Type_Id :=
        Id_For_Kind (Receiver.Node.Kind);
   begin
      return Lookup_Node_Data (Receiver_Type_Id, To_UTF8 (Name));
   end Data_Reference_For_Name;

   -----------------
   -- Is_Built_In --
   -----------------

   function Is_Built_In (Name : Text_Type) return Boolean is
      (Name = "image" or else Name = "text");

   --------------------
   -- Built_In_Field --
   --------------------

   function Built_In_Field (Receiver      : Ext_AST_Node;
                            Property_Name : Text_Type)
                            return Introspection_Value
   is
   begin
      if Property_Name = "image" then
         return Make_Introspection_Value (Receiver.Node.Text_Image);
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
     (Value : Value_Type) return Introspection_Value
   is
   begin
      case Kind (Value) is
         when Boolean_Value =>
            return (Kind => Kind_Bool, Bool_Val => As_Boolean (Value));
         when Integer_Value =>
            return (Kind => Kind_Int, Int_Val => As_Integer (Value));
         when Node_Value =>
            return (Kind     => Kind_Node,
                    Node_Val => new Ext_AST_Node'(Node => As_Node (Value)));
         when Text_Type_Value =>
            return (Kind     => Kind_Text,
                    Text_Val => To_Unbounded_Text (As_Text_Type (Value)));
         when Unbounded_Text_Value =>
            return (Kind => Kind_Text, Text_Val => As_Unbounded_Text (Value));
         % if use_unbounded_text_array:
         when Unbounded_Text_Type_Array_Value =>
            return Make_Introspection_Value
              (As_Unbounded_Text_Type_Array (Value));
         % endif
         % for type in node_array_types:
         when ${type.introspection_kind} =>
            return Make_Introspection_Value (As_${type.api_name} (Value));
         % endfor
         when others =>
            raise Introspection_Error with
              "Unsupported value type from the introspection API: " &
                 Value_Kind'Image (Kind (Value));
      end case;
   end Make_Introspection_Value;

   ----------------------
   -- Get_Property_Ref --
   ----------------------

   function Get_Property_Ref (Node          : Ext_AST_Node;
                              Property_Name : Text_Type)
                              return Property_Reference
   is
      Ref : constant Any_Node_Data_Reference :=
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
                             return Value_Type
   is
   begin
      if Value.Kind = Kind_Node_Array then
         return Array_To_Value_Type (Value.Node_Array_Val.all, Target_Kind);
      elsif Value.Kind = Kind_Text then
         return String_To_Value_Type (Value.Text_Val, Target_Kind);
      % if use_unbounded_text_array:
      elsif Value.Kind = Kind_Text_Array then
         declare
            Result : Unbounded_Text_Type_Array
              (1 .. Value.Text_Array_Val'Length);
         begin
            for I in Result'Range loop
               Result (I) := Value.Text_Array_Val (I);
            end loop;

            return Create_Unbounded_Text_Type_Array (Result);
         end;
      % endif
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
                                 return Value_Type
   is
   begin
      case Target_Kind is
         % for type in node_array_types:
         when ${type.introspection_kind} =>
            return Create_${type.api_name}
               (${type.api_name}_From_AST_Array (Value));
         % endfor
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

      raise Introspection_Error with "Cannot create a value of kind" &
        Value_Kind'Image (Target_Kind) & " from an unbounded String";
   end String_To_Value_Type;

   -----------------------
   -- Matches_Kind_Name --
   -----------------------

   overriding function Matches_Kind_Name
     (Node : Ada_AST_Node; Kind_Name : String) return Boolean
   is
      Expected_Kind : constant Any_Node_Type_Id :=
        Lookup_DSL_Name (Kind_Name);
      Actual_Kind   : constant Any_Node_Type_Id :=
        Id_For_Kind (Node.Node.Kind);
   begin
      if Expected_Kind = None then
         raise Assertion_Error with "Invalid kind name: " & Kind_Name;
      end if;

      return Actual_Kind = Expected_Kind or else
             Is_Derived_From (Actual_Kind, Expected_Kind);
   end Matches_Kind_Name;

   -------------------
   -- Is_Field_Name --
   -------------------

   overriding function Is_Field_Name
     (Node : Ext_AST_Node; Name : Text_Type) return Boolean
   is
      Data_Ref : constant Any_Node_Data_Reference :=
        Data_Reference_For_Name (Node, Name);
   begin
      return (Data_Ref in Field_Reference) or else
             (Data_Ref in Built_In_LAL_Field) or else
              Is_Built_In (Name);
   end Is_Field_Name;

   ----------------------
   -- Is_Property_Name --
   ----------------------

   function Is_Property_Name
     (Node : Ext_AST_Node; Name : Text_Type) return Boolean
   is
     (Data_Reference_For_Name (Node, Name) in Property_Reference);

   ------------------
   -- Access_Field --
   ------------------

   overriding function Access_Field
     (Node : Ext_AST_Node; Field : Text_Type) return Introspection_Value
   is
      Data_Ref : constant Any_Node_Data_Reference :=
         Data_Reference_For_Name (Node, Field);
   begin
      if Is_Built_In (Field) then
         return Built_In_Field (Node, Field);
      end if;

      return Make_Introspection_Value
        (Eval_Node_Data (Node.Node, Data_Ref, Empty_Value_Array));
   end Access_Field;

   --------------------
   -- Property_Arity --
   --------------------

   overriding function Property_Arity
     (Node : Ext_AST_Node; Property_Name : Text_Type) return Natural
   is
     (Property_Argument_Types
        (Get_Property_Ref (Node, Property_Name))'Length);

   ------------------------
   -- Default_Args_Value --
   ------------------------

   overriding function Default_Arg_Value (Node          : Ext_AST_Node;
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
     (Node          : Ext_AST_Node;
      Property_Name : Text_Type;
      Arguments     : Introspection_Value_Array)
      return Introspection_Value
   is
      Property_Args : Value_Array (1 .. Arguments'Length);
      Property_Ref  : constant Property_Reference :=
        Get_Property_Ref (Node, Property_Name);
      Contraints    : constant Value_Constraint_Array :=
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
        (Eval_Node_Data (Node.Node, Property_Ref, Property_Args));

   end Evaluate_Property;

end Ext_AST_Nodes;

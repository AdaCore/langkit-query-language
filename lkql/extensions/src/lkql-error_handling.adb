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
with LKQL.AST_Nodes;

package body LKQL.Error_Handling is

   ----------------------------
   -- Raise_And_Record_Error --
   ----------------------------

   procedure Raise_And_Record_Error
     (Ctx : Eval_Context; Error : Error_Data)
   is
      Error_Message : constant String :=
        To_UTF8 (To_Text (Error.Short_Message));
   begin
      Ctx.Add_Error (Error);

      raise Stop_Evaluation_Error with Error_Message;
   end Raise_And_Record_Error;

   --------------------------
   -- Raise_From_Exception --
   --------------------------

   procedure Raise_From_Exception
     (Ctx : Eval_Context; E : Exception_Occurrence; N : L.LKQL_Node'Class) is
   begin
      Raise_And_Record_Error
        (Ctx,
         Error_Data'
           (Eval_Error,
            N.As_LKQL_Node,
            To_Unbounded_Text (To_Text (Exception_Message (E))),
            Property_Error_Info => null));
   end Raise_From_Exception;

   --------------------------
   -- Raise_Invalid_Member --
   --------------------------

   procedure Raise_Invalid_Member (Ctx      : Eval_Context;
                                   Node     : L.Dot_Access;
                                   Receiver : Primitive)
   is
   begin
      Raise_Invalid_Member (Ctx, Node.F_Member, Receiver);
   end Raise_Invalid_Member;

   --------------------------
   -- Raise_Invalid_Member --
   --------------------------

   procedure Raise_Invalid_Member (Ctx      : Eval_Context;
                                   Node     : L.Identifier;
                                   Receiver : Primitive)
   is
      Message : constant Text_Type :=
        "Cannot get member " & Node.Text & " for " &
        To_Text (Kind_Name (Receiver)) & " value";
   begin
      Raise_And_Record_Error (Ctx, Make_Eval_Error (Node, Message));
   end Raise_Invalid_Member;

   ---------------------
   -- Raise_Null_Root --
   ---------------------

   procedure Raise_Null_Root (Ctx : Eval_Context; Node : L.Query)
   is
      Message : constant Text_Type :=
         "Cannot run a query without a proper AST root";
   begin
      Raise_And_Record_Error (Ctx, Make_Eval_Error (Node, Message));
   end Raise_Null_Root;

   ------------------------
   -- Raise_Invalid_Type --
   ------------------------

   procedure Raise_Invalid_Type (Ctx      : Eval_Context;
                                 Node     : L.LKQL_Node;
                                 Expected : Text_Type;
                                 Value    : Primitive)
   is
      Message : constant Text_Type :=
        "Type error: expected " & Expected &
        " but got " & To_Text (Kind_Name (Value));
   begin
      Raise_And_Record_Error (Ctx, Make_Eval_Error (Node, Message));
   end Raise_Invalid_Type;

   ------------------------
   -- Raise_Invalid_Kind --
   ------------------------

   procedure Raise_Invalid_Kind (Ctx      : Eval_Context;
                                 Node     : L.LKQL_Node;
                                 Expected : Valid_Primitive_Kind;
                                 Value    : Primitive)
   is
      Message : constant Text_Type :=
        "Type error: expected " & To_Text (To_String (Expected)) &
        " but got " & To_Text (Kind_Name (Value));
   begin
      Raise_And_Record_Error (Ctx, Make_Eval_Error (Node, Message));
   end Raise_Invalid_Kind;

   ---------------------------------
   -- Raise_Invalid_Selector_Name --
   ---------------------------------

   procedure Raise_Invalid_Selector_Name (Ctx  : Eval_Context;
                                          Node : L.Identifier)
   is
      Message : constant Text_Type := "Invalid selector name: " & Node.Text;
   begin
      Raise_And_Record_Error (Ctx, Make_Eval_Error (Node, Message));
   end Raise_Invalid_Selector_Name;

   --------------------------
   -- Raise_Unknown_Symbol --
   --------------------------

   procedure Raise_Unknown_Symbol (Ctx : Eval_Context;
                                   Node : L.Identifier)
   is
      Message : constant Text_Type := "Unknown symbol: " & Node.Text;
   begin
      Raise_And_Record_Error (Ctx, Make_Eval_Error (Node, Message));
   end Raise_Unknown_Symbol;

   -----------------------------------
   -- Raise_already_Existing_Symbol --
   -----------------------------------

   procedure Raise_Already_Existing_Symbol (Ctx        : Eval_Context;
                                            Identifier : Symbol_Type;
                                            Node       : L.LKQL_Node)
   is
      Message : constant Text_Type :=
        "Already existing symbol: " & Identifier.all;
   begin
      Raise_And_Record_Error (Ctx, Make_Eval_Error (Node, Message));
   end Raise_Already_Existing_Symbol;

   -------------------------
   -- Raise_Invalid_Arity --
   -------------------------

   procedure Raise_Invalid_Arity (Ctx            : Eval_Context;
                                  Expected_Arity : Natural;
                                  Arguments      : L.Arg_List)
   is
      Expected : constant Text_Type :=
        Integer'Wide_Wide_Image (Expected_Arity);
      Actual_Arity   : constant Text_Type :=
        Integer'Wide_Wide_Image (Arguments.Children_Count);
      Message  : constant Text_Type :=
         "Expected" & Expected & " arguments but got" & Actual_Arity;
   begin
      Raise_And_Record_Error (Ctx, Make_Eval_Error (Arguments, Message));
   end Raise_Invalid_Arity;

   ----------------------------
   -- Raise_Unknown_Argument --
   ----------------------------

   procedure Raise_Unknown_Argument (Ctx        : Eval_Context;
                                     Identifier : L.Identifier)
   is
      Message : constant Text_Type :=
        "Unknown argument name: " & Identifier.Text;
   begin
      Raise_And_Record_Error (Ctx, Make_Eval_Error (Identifier, Message));
   end Raise_Unknown_Argument;

   -----------------------------------
   -- Raise_Positionnal_After_Named --
   -----------------------------------

   procedure Raise_Positionnal_After_Named (Ctx         : Eval_Context;
                                            Positionnal : L.Expr_Arg)
   is
      Message : constant Text_Type :=
        "Positional argument after named argument";
   begin
      Raise_And_Record_Error (Ctx, Make_Eval_Error (Positionnal, Message));
   end Raise_Positionnal_After_Named;

   ----------------------------
   -- Raise_Already_Seen_Arg --
   ----------------------------

   procedure Raise_Already_Seen_Arg (Ctx : Eval_Context; Arg : L.Arg)
   is
      Message : constant Text_Type :=
        "Multiple arguments with the same name";
   begin
      Raise_And_Record_Error (Ctx, Make_Eval_Error (Arg, Message));
   end Raise_Already_Seen_Arg;

   -------------------------------------
   -- Raise_Invalid_Kind_For_Selector --
   -------------------------------------

   procedure Raise_Invalid_Kind_For_Selector (Ctx   : Eval_Context;
                                              Node  : L.LKQL_Node'Class;
                                              Value : Primitive)
   is
      Value_Kind_Name : constant Text_Type := To_Text (Kind_Name (Value));
      Message : constant Text_Type :=
        "Cannot use values of kind " & Value_Kind_Name & " in a selector";
   begin
      Raise_And_Record_Error (Ctx, Make_Eval_Error (Node, Message));
   end Raise_Invalid_Kind_For_Selector;

   -------------------------
   -- Raise_No_Such_Field --
   -------------------------

   procedure Raise_No_Such_Field (Ctx        : Eval_Context;
                                  Node       : H.AST_Node_Holder;
                                  Field_Name : L.Identifier)
   is
   begin
      Raise_No_Such_Datum (Ctx, Node, Field_Name, "field");
   end Raise_No_Such_Field;

   ----------------------------
   -- Raise_No_Such_Property --
   ----------------------------

   procedure Raise_No_Such_Property (Ctx           : Eval_Context;
                                     Node          : H.AST_Node_Holder;
                                     Property_Name : L.Identifier)
   is
   begin
      Raise_No_Such_Datum (Ctx, Node, Property_Name, "property");
   end Raise_No_Such_Property;

   -------------------------
   -- Raise_No_Such_Datum --
   -------------------------

   procedure Raise_No_Such_Datum (Ctx            : Eval_Context;
                                  Node           : H.AST_Node_Holder;
                                  Field_Name     : L.Identifier;
                                  Data_Type_Name : Text_Type)
   is
      Message : constant Text_Type :=
        "No " & Data_Type_Name & " named " & Field_Name.Text & " on nodes of" &
        " kind: " & To_Text (Node.Unchecked_Get.Kind_Name);
   begin
      Raise_And_Record_Error (Ctx, Make_Eval_Error (Field_Name, Message));
   end Raise_No_Such_Datum;

   -----------------------
   -- Raise_Null_Access --
   -----------------------

   procedure Raise_Null_Access (Ctx         : Eval_Context;
                                Node        : Primitive;
                                Member_Name : L.Identifier)
   is
      Message : constant Text_Type :=
        To_Text ("Invalid direct access on node of kind ") &
        To_Text (Kind_Name (Node));
   begin
      Raise_And_Record_Error (Ctx, Make_Eval_Error (Member_Name, Message));
   end Raise_Null_Access;

end LKQL.Error_Handling;

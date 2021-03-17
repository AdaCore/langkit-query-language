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

with Ada_AST_Nodes; use Ada_AST_Nodes;
with LKQL.Unit_Utils; use LKQL.Unit_Utils;
with Exec; use Exec;
with LKQL.Evaluation; use LKQL.Evaluation;
with LKQL.AST_Nodes; use LKQL.AST_Nodes;

with Libadalang.Analysis; use Libadalang.Analysis;
with Libadalang.Common; use Libadalang.Common;

with Liblkqllang.Common;
with Liblkqllang.Iterators; use Liblkqllang.Iterators;
with Ada.Wide_Wide_Characters.Handling; use Ada.Wide_Wide_Characters.Handling;
with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;

package body Rule_Commands is

   package LCO renames Liblkqllang.Common;

   ----------------
   -- Check_Kind --
   ----------------

   procedure Check_Kind
     (Expected_Kind : Valid_Primitive_Kind; Actual_Kind : Valid_Primitive_Kind;
      Context       : String)
   is
   begin
      if Expected_Kind /= Actual_Kind then
         raise Rule_Error
           with Context & ": expected value of kind " &
           To_String (Expected_Kind) & "but got " & To_String (Actual_Kind);
      end if;
   end Check_Kind;

   -------------------------
   -- Create_Rule_Command --
   -------------------------

   function Create_Rule_Command
     (LKQL_File_Path : String;
      Ctx            : L.Analysis_Context) return Rule_Command
   is
      Root    : constant L.LKQL_Node :=
        Make_LKQL_Unit (Ctx, LKQL_File_Path).Root;
      Check_Annotation : constant L.Decl_Annotation :=
        Find_First
          (Root, Kind_Is (LCO.LKQL_Decl_Annotation)).As_Decl_Annotation;
   begin
      if Check_Annotation.Is_Null
        or else Check_Annotation.F_Name.Text not in "check" | "node_check"
      then
         raise Rule_Error
           with "No @check or @node_check annotated function in "
           & LKQL_File_Path;
      end if;

      declare
         Fn   : constant L.Fun_Decl := Check_Annotation.Parent.As_Fun_Decl;
         Msg_Arg  : constant L.Arg :=
           Check_Annotation.P_Arg_With_Name (To_Unbounded_Text ("message"));
         Msg : Unbounded_Text_Type;
         Name     : constant Text_Type := Fn.F_Name.Text;

         use LCO;
      begin
         --  Get the message from the annotation if it exists

         if not Msg_Arg.Is_Null then
            --  Make sure that the message is a string literal
            if Msg_Arg.P_Expr.Kind /= LCO.LKQL_String_Literal then
               raise Rule_Error
                 with "message argument for @check/@node_check must be a " &
                 "string literal";
            end if;

            --  Store the literal, getting rid of the starting and end quotes
            Msg := To_Unbounded_Text (Msg_Arg.P_Expr.As_String_Literal.Text);
            Delete (Msg, Length (Msg), Length (Msg));
            Delete (Msg, 1, 1);
         else
            Msg := To_Unbounded_Text (To_Lower (Name));
         end if;

         return Rule_Command'
           (Name          => To_Unbounded_Text (To_Lower (Name)),
            Message       => Msg,
            LKQL_Root     => Root,
            LKQL_Context  => Ctx,
            Rule_Args     => <>,
            Is_Node_Check => Check_Annotation.F_Name.Text = "node_check",
            Code          => <>);
      end;
   end Create_Rule_Command;

   -------------
   -- Prepare --
   -------------

   procedure Prepare (Self : in out Rule_Command) is
      Code : Unbounded_Text_Type;
   begin
      --  Create the code snippet that will be passed to LKQL_Eval, along with
      --  the optional arguments passed to the rule via the command line.

      Append (Code, To_Text (Self.Name));
      Append (Code, "(");
      for I in Self.Rule_Args.First_Index .. Self.Rule_Args.Last_Index loop
         Append (Code,
                 To_Text (Self.Rule_Args (I).Name)
                 & "="
                 & To_Text (Self.Rule_Args (I).Value));
         if I < Self.Rule_Args.Last_Index then
            Append (Code, ", ");
         end if;
      end loop;
      Append (Code, ")");

      Self.Code :=
        Make_LKQL_Unit_From_Code
          (Self.LKQL_Context,
           Image (To_Text (Code)),
           "[" & Image (To_Text (Self.Name)) & " inline code]").Root;

   end Prepare;

   --------------
   -- Evaluate --
   --------------

   function Evaluate
     (Self : Rule_Command;
      Ctx  : Eval_Context) return Eval_Diagnostic_Vectors.Vector
   is
      Result       : Eval_Diagnostic_Vectors.Vector;
      Command_Name : constant Text_Type := To_Text (Self.Name);
      Nodes, Dummy : Primitive;
      Code         : Unbounded_Text_Type;
   begin

      --  Eval the rule's code (which should contain only definitions)
      Dummy := Eval (Ctx, Self.LKQL_Root);

      --  Eval the call to the check function
      Nodes := LKQL_Eval (Ctx, Image (To_Text (Code)), Self.LKQL_Context);

      Check_Kind (Kind_List,
                  Kind (Nodes), "Result of " & To_UTF8 (Command_Name));

      for N of List_Val (Nodes).Elements loop
         Check_Kind
           (Kind_Node, Kind (N), "Element from the result of "
            & To_UTF8 (Command_Name));

         declare
            Wrapped_Node : constant AST_Node_Rc := Node_Val (N);
            Ada_Wrapped_Node : constant Ada_AST_Node :=
              Ada_AST_Node (Wrapped_Node.Unchecked_Get.all);
            Node         : Ada_Node := Ada_Wrapped_Node.Node;
         begin
            if Node.Kind in Ada_Basic_Decl then
               Node := Node.As_Basic_Decl.P_Defining_Name.As_Ada_Node;
            end if;

            Result.Append
              (Eval_Diagnostic'
                 (Diagnostic'
                      (Node.Sloc_Range,
                       To_Unbounded_Text (Command_Name & " - rule violation")),
                  Node.Unit));
         end;
      end loop;

      return Result;
   end Evaluate;

end Rule_Commands;

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

with Ada.Characters.Conversions; use Ada.Characters.Conversions;
with Ada.Wide_Wide_Characters.Handling; use Ada.Wide_Wide_Characters.Handling;
with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;

with Ada_AST_Nodes; use Ada_AST_Nodes;
with LKQL.Unit_Utils; use LKQL.Unit_Utils;
with Exec; use Exec;
with LKQL.Evaluation; use LKQL.Evaluation;

with Libadalang.Analysis; use Libadalang.Analysis;
with Libadalang.Common; use Libadalang.Common;

with Liblkqllang.Common;
with Liblkqllang.Iterators; use Liblkqllang.Iterators;
with LKQL.Partial_AST_Nodes; use LKQL.Partial_AST_Nodes;
with LKQL.Primitives;    use LKQL.Primitives;

package body Rule_Commands is

   package LCO renames Liblkqllang.Common;

   function Find_Toplevel_Node_Kind_Pattern
     (Node : L.LKQL_Node'Class) return L.Node_Kind_Pattern;

   function Find_Param_Kind
     (Params : L.Parameter_Decl_List) return Rule_Param_Kind;
   --  Return the parameter kind for the given function body Node.

   --------------------------------
   -- Find_Toplevel_Node_Pattern --
   --------------------------------

   function Find_Toplevel_Node_Kind_Pattern
     (Node : L.LKQL_Node'Class) return L.Node_Kind_Pattern is
   begin
      case Node.Kind is
         when LCO.LKQL_Is_Clause =>
            return Find_Toplevel_Node_Kind_Pattern
              (Node.As_Is_Clause.F_Pattern);
         when LCO.LKQL_Node_Kind_Pattern =>
            return Node.As_Node_Kind_Pattern;
         when LCO.LKQL_Extended_Node_Pattern =>
            return Find_Toplevel_Node_Kind_Pattern
              (Node.As_Extended_Node_Pattern.F_Node_Pattern);
         when LCO.LKQL_Filtered_Pattern =>
            return Find_Toplevel_Node_Kind_Pattern
              (Node.As_Filtered_Pattern.F_Pattern);
         when LCO.LKQL_Binding_Pattern =>
            return Find_Toplevel_Node_Kind_Pattern
              (Node.As_Binding_Pattern.F_Value_Pattern);
         when others =>
            return L.No_Node_Kind_Pattern;
      end case;
   end Find_Toplevel_Node_Kind_Pattern;

   ---------------------
   -- Find_Param_Kind --
   ---------------------

   function Find_Param_Kind
     (Params : L.Parameter_Decl_List) return Rule_Param_Kind is
   begin
      if Params.Last_Child_Index = 1 then
         return No_Param;
      elsif Params.Last_Child_Index = 2 then
         case Params.Child (2).As_Parameter_Decl.F_Default_Expr.Kind is
            when LCO.LKQL_Integer_Literal => return One_Integer;
            when LCO.LKQL_Bool_Literal    => return One_Boolean;
            when LCO.LKQL_String_Literal  => return One_String;
            when LCO.LKQL_List_Literal    => return One_Array;
            when others                   => null;
         end case;
      else
         if Params.Last_Child_Index <= 10
           and then Params.Child (2).As_Parameter_Decl.F_Default_Expr.Kind in
                    LCO.LKQL_Integer_Literal | LCO.LKQL_Bool_Literal
         then
            for J in 3 .. Params.Last_Child_Index loop
               if Params.Child (J).As_Parameter_Decl.F_Default_Expr.Kind
                 not in LCO.LKQL_Bool_Literal
               then
                  return Custom;
               end if;
            end loop;

            return One_Integer_Or_Booleans;
         end if;
      end if;

      return Custom;
   end Find_Param_Kind;

   -------------------------
   -- Create_Rule_Command --
   -------------------------

   function Create_Rule_Command
     (LKQL_File_Path : String;
      Ctx            : Eval_Context;
      Rc             : out Rule_Command) return Boolean
   is
      Root    : constant L.LKQL_Node :=
        Make_LKQL_Unit (Get_Context (Ctx.Kernel.all), LKQL_File_Path).Root;
      Check_Annotation : constant L.Decl_Annotation :=
        Find_First
          (Root, Kind_Is (LCO.LKQL_Decl_Annotation)).As_Decl_Annotation;

   begin
      if Check_Annotation.Is_Null
        or else Check_Annotation.F_Name.Text not in "check" | "unit_check"
      then
         return False;
      end if;

      declare
         Fn                    : constant L.Fun_Decl
           := Check_Annotation.Parent.As_Fun_Decl;
         Msg_Arg               : constant L.Arg :=
           Check_Annotation.P_Arg_With_Name (To_Unbounded_Text ("message"));
         Help_Arg              : constant L.Arg :=
           Check_Annotation.P_Arg_With_Name (To_Unbounded_Text ("help"));
         Parametric_Exemption_Arg : constant L.Arg :=
           Check_Annotation.P_Arg_With_Name
             (To_Unbounded_Text ("parametric_exemption"));
         Remediation_Arg       : constant L.Arg :=
           Check_Annotation.P_Arg_With_Name
             (To_Unbounded_Text ("remediation"));
         Msg                   : Unbounded_Text_Type;
         Help                  : Unbounded_Text_Type;
         Remediation_Level     : Remediation_Levels := Medium;
         Parametric_Exemption  : Boolean := False;
         Name                  : constant Text_Type := Fn.F_Name.Text;
         Toplevel_Node_Pattern : L.Node_Kind_Pattern;

         Follow_Instantiations_Arg : constant L.Arg :=
           Check_Annotation.P_Arg_With_Name
             (To_Unbounded_Text ("follow_generic_instantiations"));
         Follow_Instantiations : Boolean := False;
         Param_Kind            : Rule_Param_Kind;

         use LCO;

         procedure Get_Text
           (Arg     : L.Arg;
            Default : Unbounded_Text_Type;
            Text    : out Unbounded_Text_Type);
         --  Get text value from Arg and store result in Text. Defaults to
         --  Default if Arg is null.

         --------------
         -- Get_Text --
         --------------

         procedure Get_Text
           (Arg     : L.Arg;
            Default : Unbounded_Text_Type;
            Text    : out Unbounded_Text_Type) is
         begin
            if Arg.Is_Null then
               Text := Default;
            else
               --  Make sure that the message is a string literal

               if Arg.P_Expr.Kind /= LCO.LKQL_String_Literal then
                  raise Rule_Error with
                    "argument for @" &
                    To_String (Check_Annotation.F_Name.Text) &
                    " must be a string literal";
               end if;

               --  Store the literal, getting rid of the starting & end quotes

               Text := To_Unbounded_Text (Arg.P_Expr.As_String_Literal.Text);
               Delete (Text, Length (Text), Length (Text));
               Delete (Text, 1, 1);
            end if;
         end Get_Text;

      begin
         Toplevel_Node_Pattern :=
           Find_Toplevel_Node_Kind_Pattern (Fn.F_Fun_Expr.F_Body_Expr);
         Param_Kind := Find_Param_Kind (Fn.F_Fun_Expr.F_Parameters);

         --  Get the "follow_generic_instantiations" settings if the user
         --  specified one. By default it is false.

         if not Follow_Instantiations_Arg.Is_Null then
            Follow_Instantiations :=
              Bool_Val
                (Eval (Ctx, Follow_Instantiations_Arg.P_Expr, Kind_Bool));
         end if;

         if not Parametric_Exemption_Arg.Is_Null then
            Parametric_Exemption :=
              Bool_Val
                (Eval (Ctx, Parametric_Exemption_Arg.P_Expr, Kind_Bool));
         end if;

         Get_Text (Msg_Arg, To_Unbounded_Text (Name), Msg);
         Get_Text (Help_Arg, Msg, Help);

         if not Remediation_Arg.Is_Null then
            if Remediation_Arg.P_Expr.Kind /= LCO.LKQL_String_Literal then
               raise Rule_Error with
                 "argument for @" &
                 To_String (Check_Annotation.F_Name.Text) &
                 " must be a string literal";
            end if;

            declare
               Str : constant String :=
                 To_String (Remediation_Arg.P_Expr.As_String_Literal.Text);
            begin
               Remediation_Level := Remediation_Levels'Value
                 (Str (Str'First + 1 .. Str'Last - 1));
            exception
               when others =>
                  raise Rule_Error with
                    "invalid argument for @" &
                    To_String (Check_Annotation.F_Name.Text);
            end;
         end if;

         Rc := Rule_Command'
           (Name                  => To_Unbounded_Text (To_Lower (Name)),
            Message               => Msg,
            Help                  => Help,
            LKQL_Root             => Root,
            Eval_Ctx              => Ctx.Create_New_Frame,
            Rule_Args             => <>,
            Is_Unit_Check         =>
              Check_Annotation.F_Name.Text = "unit_check",
            Code                  => <>,
            Kind_Pattern          => Toplevel_Node_Pattern,
            Follow_Instantiations => Follow_Instantiations,
            Param_Kind            => Param_Kind,
            Parameters            => Fn.F_Fun_Expr.F_Parameters,
            Remediation_Level     => Remediation_Level,
            Parametric_Exemption  => Parametric_Exemption);
         return True;
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
          (Get_Context (Self.Eval_Ctx.Kernel.all),
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
      Nodes := LKQL_Eval (Ctx, Image (To_Text (Code)),
                          Get_Context (Self.Eval_Ctx.Kernel.all));

      Check_Kind (Ctx, Self.LKQL_Root, Kind_List, Nodes);

      for N of List_Val (Nodes).Elements loop
         Check_Kind (Ctx, Self.LKQL_Root, Kind_Node, N);

         declare
            Wrapped_Node : constant H.AST_Node_Holder := Node_Val (N);
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

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Self : in out Rule_Command) is
   begin
      Self.Eval_Ctx.Release_Current_Frame;
   end Destroy;

end Rule_Commands;

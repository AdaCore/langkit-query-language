------------------------------------------------------------------------------
--                                                                          --
--                                   LKQL                                   --
--                                                                          --
--                     Copyright (C) 2019-2023, AdaCore                     --
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

with Ada.Characters.Conversions;      use Ada.Characters.Conversions;
with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;

with LKQL.Unit_Utils; use LKQL.Unit_Utils;
with LKQL.Evaluation; use LKQL.Evaluation;

with Liblkqllang.Common;
with Liblkqllang.Generic_API.Introspection;
use Liblkqllang.Generic_API.Introspection;
with Liblkqllang.Iterators; use Liblkqllang.Iterators;
with LKQL.Primitives;       use LKQL.Primitives;

package body Rule_Commands is

   package LCO renames Liblkqllang.Common;

   function Find_Toplevel_Node_Kind_Pattern
     (Node : L.Lkql_Node'Class) return L.Node_Kind_Pattern;

   function Find_Param_Kind
     (Params : L.Parameter_Decl_List) return Rule_Param_Kind;
   --  Return the parameter kind for the given function body Node.

   --------------------------------
   -- Find_Toplevel_Node_Pattern --
   --------------------------------

   function Find_Toplevel_Node_Kind_Pattern
     (Node : L.Lkql_Node'Class) return L.Node_Kind_Pattern is
   begin
      case Node.Kind is
         when LCO.Lkql_Is_Clause =>
            return Find_Toplevel_Node_Kind_Pattern
              (Node.As_Is_Clause.F_Pattern);
         when LCO.Lkql_Node_Kind_Pattern =>
            return Node.As_Node_Kind_Pattern;
         when LCO.Lkql_Extended_Node_Pattern =>
            return Find_Toplevel_Node_Kind_Pattern
              (Node.As_Extended_Node_Pattern.F_Node_Pattern);
         when LCO.Lkql_Filtered_Pattern =>
            return Find_Toplevel_Node_Kind_Pattern
              (Node.As_Filtered_Pattern.F_Pattern);
         when LCO.Lkql_Binding_Pattern =>
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
            when LCO.Lkql_Integer_Literal => return One_Integer;
            when LCO.Lkql_Bool_Literal    => return One_Boolean;
            when LCO.Lkql_String_Literal  => return One_String;
            when LCO.Lkql_List_Literal    => return One_Array;
            when others                   => null;
         end case;
      else
         if Params.Last_Child_Index <= 10
           and then Params.Child (2).As_Parameter_Decl.F_Default_Expr.Kind in
                    LCO.Lkql_Integer_Literal | LCO.Lkql_Bool_Literal
         then
            for J in 3 .. Params.Last_Child_Index loop
               if Params.Child (J).As_Parameter_Decl.F_Default_Expr.Kind
                 not in LCO.Lkql_Bool_Literal
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
     (Lkql_File_Path : String;
      Ctx            : Eval_Context;
      Rc             : out Rule_Command) return Boolean
   is
      Root    : constant L.Lkql_Node :=
        Make_Lkql_Unit (Ctx, Lkql_File_Path).Root;

      Check_Annotation : constant L.Decl_Annotation :=
        Find_First
          (Root,
           Kind_Is (LCO.Lkql_Decl_Annotation)
           and Child_With
                 (Member_Refs.Decl_Annotation_F_Name,
                  Text_Is ("check") or Text_Is ("unit_check")))
          .As_Decl_Annotation;

   begin
      if Check_Annotation.Is_Null
        or else Check_Annotation.F_Name.Text not in "check" | "unit_check"
      then
         return False;
      end if;

      declare
         Fn                    : constant L.Fun_Decl :=
           Check_Annotation.Parent.As_Fun_Decl;
         Msg_Arg               : constant L.Arg :=
           Check_Annotation.P_Arg_With_Name (To_Unbounded_Text ("message"));
         Help_Arg              : constant L.Arg :=
           Check_Annotation.P_Arg_With_Name (To_Unbounded_Text ("help"));
         Category_Arg          : constant L.Arg :=
           Check_Annotation.P_Arg_With_Name (To_Unbounded_Text ("category"));
         Subcategory_Arg       : constant L.Arg :=
           Check_Annotation.P_Arg_With_Name
             (To_Unbounded_Text ("subcategory"));
         Parametric_Exemption_Arg : constant L.Arg :=
           Check_Annotation.P_Arg_With_Name
             (To_Unbounded_Text ("parametric_exemption"));
         Remediation_Arg       : constant L.Arg :=
           Check_Annotation.P_Arg_With_Name
             (To_Unbounded_Text ("remediation"));
         Impact_Arg            : constant L.Arg :=
           Check_Annotation.P_Arg_With_Name (To_Unbounded_Text ("impact"));
         Target_Arg            : constant L.Arg :=
           Check_Annotation.P_Arg_With_Name (To_Unbounded_Text ("target"));
         Msg                   : Unbounded_Text_Type;
         Help                  : Unbounded_Text_Type;
         Category              : Unbounded_Text_Type;
         Subcategory           : Unbounded_Text_Type;
         Impact                : Regexp_Access;
         Target                : Regexp_Access;
         Remediation_Level     : Remediation_Levels := Medium;
         Parametric_Exemption  : Boolean := False;
         Name                  : constant Text_Type := Fn.F_Name.Text;
         Toplevel_Node_Pattern : L.Node_Kind_Pattern;

         Follow_Instantiations_Arg : constant L.Arg :=
           Check_Annotation.P_Arg_With_Name
             (To_Unbounded_Text ("follow_generic_instantiations"));
         Follow_Instantiations : Boolean := False;
         Param_Kind            : Rule_Param_Kind;

         use LCO, GNAT.Regexp;

         procedure Check_String (Arg : L.Arg);
         --  Check whether the argument is a string literal, raise Rule_Error
         --  if not.

         procedure Get_Text
           (Arg     : L.Arg;
            Default : Unbounded_Text_Type;
            Text    : out Unbounded_Text_Type);
         --  Get text value from Arg and store result in Text. Defaults to
         --  Default if Arg is null.

         ------------------
         -- Check_String --
         ------------------

         procedure Check_String (Arg : L.Arg) is
         begin
            if Arg.P_Expr.Kind /= LCO.Lkql_String_Literal then
               raise Rule_Error with
                 "argument for @" &
                 To_String (Check_Annotation.F_Name.Text) &
                 " must be a string literal";
            end if;
         end Check_String;

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

               Check_String (Arg);

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
         Get_Text (Category_Arg, To_Unbounded_Text ("Misc"), Category);
         Get_Text (Subcategory_Arg, To_Unbounded_Text (""), Subcategory);

         if not Impact_Arg.Is_Null then
            Check_String (Impact_Arg);

            declare
               Str : constant String :=
                 To_String (Impact_Arg.P_Expr.As_String_Literal.Text);
            begin
               Impact :=
                 new Regexp'(Compile ("{" &
                                      Str (Str'First + 1 .. Str'Last - 1) &
                                      "}",
                                      Glob => True, Case_Sensitive => False));

            exception
               when others =>
                  raise Rule_Error with
                    "invalid argument for @" &
                    To_String (Check_Annotation.F_Name.Text);
            end;
         end if;

         if not Target_Arg.Is_Null then
            Check_String (Target_Arg);

            declare
               Str : constant String :=
                 To_String (Target_Arg.P_Expr.As_String_Literal.Text);
            begin
               Target :=
                 new Regexp'(Compile ("{" &
                                      Str (Str'First + 1 .. Str'Last - 1) &
                                      "}",
                                      Glob => True, Case_Sensitive => False));

            exception
               when others =>
                  raise Rule_Error with
                    "invalid argument for @" &
                    To_String (Check_Annotation.F_Name.Text);
            end;
         end if;

         if not Remediation_Arg.Is_Null then
            Check_String (Remediation_Arg);

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
            Category              => Category,
            Subcategory           => Subcategory,
            Lkql_Root             => Root,
            Function_Expr         => Fn.F_Fun_Expr.F_Body_Expr,
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
            Parametric_Exemption  => Parametric_Exemption,
            Impact                => Impact,
            Target                => Target);
         return True;
      end;
   end Create_Rule_Command;

   -------------
   -- Prepare --
   -------------

   procedure Prepare (Self : in out Rule_Command) is
      Code : Unbounded_Text_Type;
   begin
      --  Create the code snippet that will be passed to Lkql_Eval, along with
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
        Make_Lkql_Unit_From_Code
          (Self.Eval_Ctx,
           Image (To_Text (Code)),
           "[" & Image (To_Text (Self.Name)) & " inline code]").Root;

      if not Self.Is_Unit_Check then
         --  For node checks, we optimize away the function call, so we will
         --  add the parameters values to the environment.

         --  First add bindings for formals who have default param values
         for Param of
            Self.Function_Expr.Parent.As_Base_Function.P_Default_Parameters
         loop
            Self.Eval_Ctx.Add_Binding
              (Param.F_Param_Identifier.Text,
               Eval
                 (Self.Eval_Ctx,
                  Param.F_Default_Expr));
         end loop;

         --  Then add bindings for all explicitly passed parameters
         for I in Self.Rule_Args.First_Index + 1 .. Self.Rule_Args.Last_Index
         loop
            Self.Eval_Ctx.Add_Binding
              (To_Text (Self.Rule_Args (I).Name),
               Eval
                 (Self.Eval_Ctx,
                  Make_Lkql_Unit_From_Code
                    (Self.Eval_Ctx,
                     Image (To_Text (Self.Rule_Args (I).Value)),
                     "[" & Image (To_Text (Self.Name)) & " inline code]")
                  .Root.Child (1)));
         end loop;

      end if;

   end Prepare;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Self : in out Rule_Command) is
   begin
      Self.Eval_Ctx.Release_Current_Frame;
   end Destroy;

end Rule_Commands;

--
--  Copyright (C) 2005-2025, AdaCore
--  SPDX-License-Identifier: GPL-3.0-or-later
--

with Ada.Characters.Conversions;      use Ada.Characters.Conversions;
with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;

with Liblkqllang.Common;
with Liblkqllang.Generic_API.Introspection;
use Liblkqllang.Generic_API.Introspection;
with Liblkqllang.Iterators;                 use Liblkqllang.Iterators;

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
         when LCO.Lkql_Is_Clause             =>
            return
              Find_Toplevel_Node_Kind_Pattern (Node.As_Is_Clause.F_Pattern);

         when LCO.Lkql_Node_Kind_Pattern     =>
            return Node.As_Node_Kind_Pattern;

         when LCO.Lkql_Extended_Node_Pattern =>
            return
              Find_Toplevel_Node_Kind_Pattern
                (Node.As_Extended_Node_Pattern.F_Node_Pattern);

         when LCO.Lkql_Filtered_Pattern      =>
            return
              Find_Toplevel_Node_Kind_Pattern
                (Node.As_Filtered_Pattern.F_Pattern);

         when LCO.Lkql_Binding_Pattern       =>
            return
              Find_Toplevel_Node_Kind_Pattern
                (Node.As_Binding_Pattern.F_Value_Pattern);

         when others                         =>
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
            when LCO.Lkql_Integer_Literal =>
               return One_Integer;

            when LCO.Lkql_Bool_Literal    =>
               return One_Boolean;

            when LCO.Lkql_String_Literal  =>
               return One_String;

            when LCO.Lkql_List_Literal    =>
               return One_Array;

            when others                   =>
               null;
         end case;
      else
         if Params.Last_Child_Index <= 10
           and then Params.Child (2).As_Parameter_Decl.F_Default_Expr.Kind
                    in LCO.Lkql_Integer_Literal | LCO.Lkql_Bool_Literal
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
      Ctx            : L.Analysis_Context;
      Impacts        : JSON_Value;
      Rc             : out Rule_Command) return Boolean
   is
      Root : constant L.Lkql_Node := Ctx.Get_From_File (Lkql_File_Path).Root;

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
         Fn                       : constant L.Fun_Decl :=
           Check_Annotation.Parent.As_Fun_Decl;
         Msg_Arg                  : constant L.Arg :=
           Check_Annotation.P_Arg_With_Name (To_Unbounded_Text ("message"));
         Help_Arg                 : constant L.Arg :=
           Check_Annotation.P_Arg_With_Name (To_Unbounded_Text ("help"));
         Category_Arg             : constant L.Arg :=
           Check_Annotation.P_Arg_With_Name (To_Unbounded_Text ("category"));
         Subcategory_Arg          : constant L.Arg :=
           Check_Annotation.P_Arg_With_Name
             (To_Unbounded_Text ("subcategory"));
         Parametric_Exemption_Arg : constant L.Arg :=
           Check_Annotation.P_Arg_With_Name
             (To_Unbounded_Text ("parametric_exemption"));
         Remediation_Arg          : constant L.Arg :=
           Check_Annotation.P_Arg_With_Name
             (To_Unbounded_Text ("remediation"));
         Target_Arg               : constant L.Arg :=
           Check_Annotation.P_Arg_With_Name (To_Unbounded_Text ("target"));
         Msg                      : Unbounded_Text_Type;
         Help                     : Unbounded_Text_Type;
         Category                 : Unbounded_Text_Type;
         Subcategory              : Unbounded_Text_Type;
         Impact                   : Regexp_Access;
         Target                   : Regexp_Access;
         Remediation_Level        : Remediation_Levels := Medium;
         Parametric_Exemption     : Boolean := False;
         Name                     : constant Text_Type := Fn.F_Name.Text;
         Toplevel_Node_Pattern    : L.Node_Kind_Pattern;

         Param_Kind : Rule_Param_Kind;

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
               raise Rule_Error
                 with
                   "argument for @"
                   & To_String (Check_Annotation.F_Name.Text)
                   & " must be a string literal";
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

         if not Parametric_Exemption_Arg.Is_Null then
            Parametric_Exemption :=
              Parametric_Exemption_Arg.P_Expr.Text = "true";
         end if;

         Get_Text (Msg_Arg, To_Unbounded_Text (Name), Msg);
         Get_Text (Help_Arg, Msg, Help);
         Get_Text (Category_Arg, To_Unbounded_Text ("Misc"), Category);
         Get_Text (Subcategory_Arg, To_Unbounded_Text (""), Subcategory);

         if Impacts /= JSON_Null then
            declare
               Impact_Value : constant JSON_Value :=
                 Impacts.Get (To_UTF8 (To_Lower (Name)));
            begin
               if Impact_Value /= JSON_Null then
                  Impact :=
                    new Regexp'
                      (Compile
                         ("{" & Impact_Value.Get & "}",
                          Glob           => True,
                          Case_Sensitive => False));
               end if;
            exception
               when others =>
                  raise Rule_Error
                    with
                      "invalid impact entry for "
                      & To_String (Check_Annotation.F_Name.Text);
            end;
         end if;

         if not Target_Arg.Is_Null then
            Check_String (Target_Arg);

            declare
               Str : constant String :=
                 To_String (Target_Arg.P_Expr.As_String_Literal.Text);
            begin
               Target :=
                 new Regexp'
                   (Compile
                      ("{" & Str (Str'First + 1 .. Str'Last - 1) & "}",
                       Glob           => True,
                       Case_Sensitive => False));

            exception
               when others =>
                  raise Rule_Error
                    with
                      "invalid argument for @"
                      & To_String (Check_Annotation.F_Name.Text);
            end;
         end if;

         if not Remediation_Arg.Is_Null then
            Check_String (Remediation_Arg);

            declare
               Str : constant String :=
                 To_String (Remediation_Arg.P_Expr.As_String_Literal.Text);
            begin
               Remediation_Level :=
                 Remediation_Levels'Value
                   (Str (Str'First + 1 .. Str'Last - 1));
            exception
               when others =>
                  raise Rule_Error
                    with
                      "invalid argument for @"
                      & To_String (Check_Annotation.F_Name.Text);
            end;
         end if;

         Rc :=
           Rule_Command'
             (Name                 => To_Unbounded_Text (To_Lower (Name)),
              Message              => Msg,
              Help                 => Help,
              Category             => Category,
              Subcategory          => Subcategory,
              Lkql_Root            => Root,
              Function_Expr        => Fn.F_Fun_Expr.F_Body_Expr,
              Rule_Args            => <>,
              Is_Unit_Check        =>
                Check_Annotation.F_Name.Text = "unit_check",
              Code                 => <>,
              Kind_Pattern         => Toplevel_Node_Pattern,
              Param_Kind           => Param_Kind,
              Parameters           => Fn.F_Fun_Expr.F_Parameters,
              Remediation_Level    => Remediation_Level,
              Parametric_Exemption => Parametric_Exemption,
              Impact               => Impact,
              Target               => Target);
         return True;
      end;
   end Create_Rule_Command;

end Rule_Commands;

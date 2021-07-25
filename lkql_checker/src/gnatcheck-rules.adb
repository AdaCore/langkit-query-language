------------------------------------------------------------------------------
--                                                                          --
--                          GNATCHECK COMPONENTS                            --
--                                                                          --
--                      G N A T C H E C K . R U L E S                       --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 2004-2021, AdaCore                     --
--                                                                          --
-- GNATCHECK  is  free  software;  you can redistribute it and/or modify it --
-- under terms of the  GNU  General Public License as published by the Free --
-- Software Foundation;  either version 3, or ( at your option)  any  later --
-- version.  GNATCHECK  is  distributed in the hope that it will be useful, --
-- but  WITHOUT  ANY  WARRANTY;   without  even  the  implied  warranty  of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General --
-- Public License for more details.  You should have received a copy of the --
-- GNU General Public License distributed with GNAT; see file  COPYING3. If --
-- not,  go  to  http://www.gnu.org/licenses  for  a  complete  copy of the --
-- license.                                                                 --
--                                                                          --
-- GNATCHECK is maintained by AdaCore (http://www.adacore.com).             --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Characters.Conversions; use Ada.Characters.Conversions;
with Ada.Characters.Handling;    use Ada.Characters.Handling;
with Ada.Strings.Fixed;          use Ada.Strings.Fixed;
with Ada.Strings.Unbounded;

with Gnatcheck.Options;          use Gnatcheck.Options;
with Gnatcheck.Output;           use Gnatcheck.Output;
with Gnatcheck.String_Utilities; use Gnatcheck.String_Utilities;

with Langkit_Support.Text;       use Langkit_Support.Text;

package body Gnatcheck.Rules is

   -------------------
   -- Annotate_Rule --
   -------------------

   function Annotate_Rule (Rule : Rule_Template) return String is
   begin
      if Gnatcheck.Options.Mapping_Mode then
         if Has_Synonym (Rule) then
            return " [" & Rule_Synonym (Rule) & "]";
         else
            return " [" & Rule_Name (Rule) & "]";
         end if;
      else
         return "";
      end if;
   end Annotate_Rule;

   -----------------
   -- Has_Synonym --
   -----------------

   function Has_Synonym (Rule : Rule_Template) return Boolean is
   begin
      return Rule.User_Synonym /= null;
   end Has_Synonym;

   -------------
   -- Has_Tip --
   -------------

   function Has_Tip (Rule : Rule_Template) return Boolean is
      pragma Unreferenced (Rule);
   begin
      return False;
   end Has_Tip;

   ---------------
   -- Init_Rule --
   ---------------

   procedure Init_Rule (Rule : in out Rule_Template) is
   begin
      Rule.Rule_State        := Disabled;
      Rule.Remediation_Level := Medium;
   end Init_Rule;

   procedure Init_Rule (Rule : in out One_Boolean_Parameter_Rule) is
   begin
      Init_Rule (Rule_Template (Rule));
      Rule.Param := Unset;
   end Init_Rule;

   procedure Init_Rule (Rule : in out One_Integer_Parameter_Rule) is
   begin
      Init_Rule (Rule_Template (Rule));
      Rule.Param := Integer'First;
   end Init_Rule;

   procedure Init_Rule
     (Rule : in out One_Integer_Or_Booleans_Parameter_Rule) is
   begin
      Init_Rule (Rule_Template (Rule));
      Rule.Integer_Param := Integer'First;
      Rule.Boolean_Params := (others => Unset);
   end Init_Rule;

   ----------------
   -- Is_Enabled --
   ----------------

   function Is_Enabled (Rule : Rule_Template) return Boolean is
   begin
      return Rule.Rule_State = Enabled;
   end Is_Enabled;

   ----------------
   -- Print_Rule --
   ----------------

   procedure Print_Rule
     (Rule         : Rule_Template;
      Indent_Level : Natural := 0) is
   begin
      Report_No_EOL (Rule_Name (Rule), Indent_Level);
   end Print_Rule;

   overriding procedure Print_Rule
     (Rule         : One_Integer_Parameter_Rule;
      Indent_Level : Natural := 0) is
   begin
      Print_Rule (Rule_Template (Rule), Indent_Level);

      if Rule.Param /= Integer'First then
         Report_No_EOL (": " & Image (Rule.Param));
      end if;
   end Print_Rule;

   overriding procedure Print_Rule
     (Rule         : One_Boolean_Parameter_Rule;
      Indent_Level : Natural := 0) is
   begin
      Print_Rule (Rule_Template (Rule), Indent_Level);

      if Rule.Param = On then
         Report_No_EOL (": " & To_String (Rule.Parameters.Child (2).
                                          As_Parameter_Decl.F_Param_Identifier.
                                          Text));
      end if;
   end Print_Rule;

   overriding procedure Print_Rule
     (Rule         : One_String_Parameter_Rule;
      Indent_Level : Natural := 0) is
   begin
      Print_Rule (Rule_Template (Rule), Indent_Level);

      if Length (Rule.Param) /= 0 then
         Report_No_EOL (": " & To_String (To_Wide_Wide_String (Rule.Param)));
      end if;
   end Print_Rule;

   overriding procedure Print_Rule
     (Rule         : One_Integer_Or_Booleans_Parameter_Rule;
      Indent_Level : Natural := 0)
   is
      Has_Param : Boolean := False;
   begin
      Print_Rule (Rule_Template (Rule), Indent_Level);

      if Rule.Integer_Param /= Integer'First then
         Report_No_EOL (": " & Image (Rule.Integer_Param));
         Has_Param := True;
      end if;

      for J in Rule.Boolean_Params'Range loop
         if Rule.Boolean_Params (J) = On then
            Report_No_EOL (if Has_Param then ", " else ": ");
            Report_No_EOL (To_String (Rule.Parameters.Child (J).
                                      As_Parameter_Decl.F_Param_Identifier.
                                      Text));
            Has_Param := True;
         end if;
      end loop;
   end Print_Rule;

   ------------------------
   -- Print_Rule_To_File --
   ------------------------

   procedure Print_Rule_To_File
     (Rule         : Rule_Template;
      Rule_File    : File_Type;
      Indent_Level : Natural := 0) is
   begin
      for J in 1 .. Indent_Level loop
         Put (Rule_File, Get_Indent_String);
      end loop;

      Put (Rule_File, "+R" & Rule_Name (Rule));
   end Print_Rule_To_File;

   overriding procedure Print_Rule_To_File
     (Rule         : One_Integer_Parameter_Rule;
      Rule_File    : File_Type;
      Indent_Level : Natural := 0) is
   begin
      Print_Rule_To_File (Rule_Template (Rule), Rule_File, Indent_Level);

      if Rule.Param /= Integer'First then
         Put (Rule_File, ": " & Image (Rule.Param));
      end if;
   end Print_Rule_To_File;

   overriding procedure Print_Rule_To_File
     (Rule         : One_Boolean_Parameter_Rule;
      Rule_File    : File_Type;
      Indent_Level : Natural := 0) is
   begin
      Print_Rule_To_File (Rule_Template (Rule), Rule_File, Indent_Level);

      if Rule.Param = On then
         Put (Rule_File,
              ": " &
              To_String (Rule.Parameters.Child (2).As_Parameter_Decl.
                         F_Param_Identifier.Text));
      end if;
   end Print_Rule_To_File;

   overriding procedure Print_Rule_To_File
     (Rule         : One_String_Parameter_Rule;
      Rule_File    : File_Type;
      Indent_Level : Natural := 0) is
   begin
      Print_Rule_To_File (Rule_Template (Rule), Rule_File, Indent_Level);

      if Length (Rule.Param) /= 0 then
         Put (Rule_File, ": " & To_String (To_Wide_Wide_String (Rule.Param)));
      end if;
   end Print_Rule_To_File;

   overriding procedure Print_Rule_To_File
     (Rule         : One_Integer_Or_Booleans_Parameter_Rule;
      Rule_File    : File_Type;
      Indent_Level : Natural := 0)
   is
      Has_Param : Boolean := False;
   begin
      Print_Rule_To_File (Rule_Template (Rule), Rule_File, Indent_Level);

      if Rule.Integer_Param /= Integer'First then
         Put (Rule_File, ": " & Image (Rule.Integer_Param));
         Has_Param := True;
      end if;

      for J in Rule.Boolean_Params'Range loop
         if Rule.Boolean_Params (J) = On then
            Put (Rule_File, (if Has_Param then ", " else ": "));
            Put (Rule_File, To_String (Rule.Parameters.Child (J).
                                       As_Parameter_Decl.F_Param_Identifier.
                                       Text));
            Has_Param := True;
         end if;
      end loop;
   end Print_Rule_To_File;

   ---------------------
   -- Print_Rule_Help --
   ---------------------

   procedure Print_Rule_Help (Rule : Rule_Template) is
   begin
      Info
        (Message  => " " & Rule.Name.all  & " - " & Rule.Help_Info.all &
                     " - " & Rule.Remediation_Level'Img,
         Line_Len => 0,
         Spacing  => 0);
   end Print_Rule_Help;

   ----------------------------
   -- Process_Rule_Parameter --
   ----------------------------

   function Defined_Str (Defined_At : String) return String is
     (if Defined_At = "" then "command line" else Defined_At);
   --  Helper function to return Defined_At if not null, or "command line"

   procedure Process_Rule_Parameter
     (Rule       : in out Rule_Template;
      Param      : String;
      Enable     : Boolean;
      Defined_At : String)
   is
      pragma Unreferenced (Defined_At);
   begin
      if Param /= "" then
         Error ("no parameter can be set for rule " & Rule.Name.all & ", " &
                Param & " ignored");
      else
         if Enable then
            Rule.Rule_State := Enabled;
         else
            Rule.Rule_State := Disabled;
         end if;
      end if;
   end Process_Rule_Parameter;

   overriding procedure Process_Rule_Parameter
     (Rule       : in out One_Integer_Parameter_Rule;
      Param      : String;
      Enable     : Boolean;
      Defined_At : String) is
   begin
      if Param = "" then
         if Enable then
            Error ("(" & Rule.Name.all & ") parameter is required for +R");
         else
            Rule.Rule_State := Disabled;
         end if;
      else
         if Enable then
            if Gnatcheck.Options.Check_Param_Redefinition
              and then Rule.Rule_State = Enabled
            then
               Error
                ("redefining at " & Defined_Str (Defined_At) &
                 " parameter for rule " & Rule.Name.all &
                 " defined at " & Defined_Str (Rule.Defined_At.all));
            end if;

            begin
               Rule.Param := Integer'Value (Param);

               if Rule.Param >= 0 then
                  Rule.Rule_State := Enabled;
                  Rule.Defined_At := new String'(Defined_At);
               else
                  Error ("(" & Rule.Name.all & ") wrong parameter: " & Param);
                  Rule.Rule_State := Disabled;
               end if;

            exception
               when Constraint_Error =>
                  Error ("(" & Rule.Name.all & ") wrong parameter: " & Param);
                  Rule.Rule_State := Disabled;
            end;

         else
            Error ("(" & Rule.Name.all & ") no parameter allowed for -R");
         end if;
      end if;
   end Process_Rule_Parameter;

   overriding procedure Process_Rule_Parameter
     (Rule       : in out One_Boolean_Parameter_Rule;
      Param      : String;
      Enable     : Boolean;
      Defined_At : String) is
   begin
      if Param = "" then
         if Enable then
            Rule.Rule_State := Enabled;
            Rule.Defined_At := new String'(Defined_At);
         else
            Rule.Param := Unset;
            Rule.Rule_State := Disabled;
         end if;
      elsif To_String (Rule.Parameters.Child (2).As_Parameter_Decl.
                       F_Param_Identifier.Text) /= To_Lower (Param)
      then
         Error ("(" & Rule.Name.all & ") wrong parameter: " & Param);
         Rule.Rule_State := Disabled;
         Rule.Param := Unset;

      elsif Gnatcheck.Options.Check_Param_Redefinition
        and then Rule.Rule_State = Enabled
      then
         Error
          ("redefining at " & Defined_Str (Defined_At) &
           " parameter " & Param & " for rule " & Rule.Name.all &
           " defined at " & Defined_Str (Rule.Defined_At.all));

      elsif Enable then
         Rule.Param := On;
         Rule.Rule_State := Enabled;
         Rule.Defined_At := new String'(Defined_At);

      else
         Rule.Param := Off;
         Rule.Rule_State := Enabled;
         Rule.Defined_At := new String'(Defined_At);
      end if;
   end Process_Rule_Parameter;

   overriding procedure Process_Rule_Parameter
     (Rule       : in out One_String_Parameter_Rule;
      Param      : String;
      Enable     : Boolean;
      Defined_At : String)
   is
      Str  : String_Access;
      Last : Natural;
   begin
      if Param = "" then
         if Enable then
            Error ("(" & Rule.Name.all & ") parameter is required for +R");
         else
            Rule.Rule_State := Disabled;
         end if;
      elsif Gnatcheck.Options.Check_Param_Redefinition
        and then Rule.Rule_State = Enabled
      then
         Error
          ("redefining at " & Defined_Str (Defined_At) &
           " parameter " & Param & " for rule " & Rule.Name.all &
           " defined at " & Defined_Str (Rule.Defined_At.all));

      elsif Enable then
         --  '@' designates a response file

         if Param (Param'First) = '@' then
            Str  := Read_File (Param (Param'First + 1 .. Param'Last));
            Last := Str'Last;

            --  Strip trailing end of line

            if Str (Str'Last) = ASCII.LF then
               Last := Last - 1;
            end if;

            Append (Rule.Param, To_Wide_Wide_String (Str (1 .. Last)));
            Free (Str);

         else
            Append (Rule.Param, To_Wide_Wide_String (Param));
         end if;

         Rule.Rule_State := Enabled;
         Rule.Defined_At := new String'(Defined_At);

      else
         Rule.Rule_State := Disabled;
         Rule.Defined_At := new String'(Defined_At);
      end if;
   end Process_Rule_Parameter;

   overriding procedure Process_Rule_Parameter
     (Rule       : in out One_Integer_Or_Booleans_Parameter_Rule;
      Param      : String;
      Enable     : Boolean;
      Defined_At : String) is
   begin
      if Param = "" then
         if Enable then
            Rule.Rule_State := Enabled;
            Rule.Defined_At := new String'(Defined_At);
         else
            Rule.Integer_Param := Integer'First;
            Rule.Boolean_Params := (others => Unset);
            Rule.Rule_State := Disabled;
         end if;
      else
         if Enable then
            --  First try to extra an integer if not already set

            if Rule.Integer_Param = Integer'First then
               begin
                  Rule.Integer_Param := Integer'Value (Param);

                  if Rule.Integer_Param >= 0 then
                     Rule.Rule_State := Enabled;
                     Rule.Defined_At := new String'(Defined_At);
                  else
                     Error ("(" & Rule.Name.all & ") wrong parameter: " &
                            Param);
                     Rule.Integer_Param := Integer'First;
                     Rule.Boolean_Params := (others => Unset);
                     Rule.Rule_State := Disabled;
                  end if;

                  return;
               exception
                  when Constraint_Error =>
                     null;
               end;
            end if;

            --  Then find the relevant boolean parameter

            for J in 2 .. Rule.Parameters.Last_Child_Index loop
               if To_String (Rule.Parameters.Child (J).As_Parameter_Decl.
                             F_Param_Identifier.Text) = To_Lower (Param)
               then
                  Rule.Boolean_Params (J) := On;
                  Rule.Rule_State := Enabled;
                  Rule.Defined_At := new String'(Defined_At);
                  return;
               end if;
            end loop;

            --  If we get there, it means we have no found any parameter

            Error ("(" & Rule.Name.all & ") wrong parameter: " & Param);
            Rule.Integer_Param := Integer'First;
            Rule.Boolean_Params := (others => Unset);
            Rule.Rule_State := Disabled;

         else
            Error ("(" & Rule.Name.all & ") no parameter allowed for -R");
         end if;
      end if;
   end Process_Rule_Parameter;

   overriding procedure Process_Rule_Parameter
     (Rule       : in out One_Array_Parameter_Rule;
      Param      : String;
      Enable     : Boolean;
      Defined_At : String) is
   begin
      if Param = "" then
         if Enable then
            Rule.Rule_State := Enabled;
         else
            Rule.Rule_State := Disabled;
         end if;
      elsif Enable then
         if Length (Rule.Param) /= 0 then
            Append (Rule.Param, ",");
         end if;

         Append (Rule.Param, To_Wide_Wide_String (Param));
         Rule.Rule_State := Enabled;
         Rule.Defined_At := new String'(Defined_At);

      else
         Set_Unbounded_Wide_Wide_String (Rule.Param, "");
         Rule.Rule_State := Disabled;
         Rule.Defined_At := new String'(Defined_At);
      end if;
   end Process_Rule_Parameter;

   --------------------
   -- Map_Parameters --
   --------------------

   overriding procedure Map_Parameters
     (Rule : One_Integer_Parameter_Rule;
      Args : in out Rule_Argument_Vectors.Vector) is
   begin
      if Rule.Param /= Integer'First then
         Args.Append
           (Rule_Argument'(Name  => To_Unbounded_Text
                                      (Rule.Parameters.Child (2).
                                       As_Parameter_Decl.F_Param_Identifier.
                                       Text),
                           Value => To_Unbounded_Text
                                      (Rule.Param'Wide_Wide_Image)));
      end if;
   end Map_Parameters;

   overriding procedure Map_Parameters
     (Rule : One_Boolean_Parameter_Rule;
      Args : in out Rule_Argument_Vectors.Vector) is
   begin
      if Rule.Param /= Unset then
         Args.Append
           (Rule_Argument'
             (Name  => To_Unbounded_Text
                         (Rule.Parameters.Child (2).
                          As_Parameter_Decl.F_Param_Identifier.Text),
              Value => To_Unbounded_Text
                         (if Rule.Param = On then "true" else "false")));
      end if;
   end Map_Parameters;

   overriding procedure Map_Parameters
     (Rule : One_String_Parameter_Rule;
      Args : in out Rule_Argument_Vectors.Vector) is
   begin
      if Length (Rule.Param) /= 0 then
         Args.Append
           (Rule_Argument'
             (Name  => To_Unbounded_Text
                         (Rule.Parameters.Child (2).
                          As_Parameter_Decl.F_Param_Identifier.Text),
              Value => To_Unbounded_Text ('"' &
                                          To_Wide_Wide_String (Rule.Param) &
                                          '"')));
      end if;
   end Map_Parameters;

   overriding procedure Map_Parameters
     (Rule : One_Integer_Or_Booleans_Parameter_Rule;
      Args : in out Rule_Argument_Vectors.Vector)
   is
   begin
      if Rule.Integer_Param /= Integer'First then
         Args.Append
           (Rule_Argument'(Name  => To_Unbounded_Text
                                      (Rule.Parameters.Child (2).
                                       As_Parameter_Decl.F_Param_Identifier.
                                       Text),
                           Value => To_Unbounded_Text
                                      (Rule.Integer_Param'Wide_Wide_Image)));
      end if;

      for J in 2 .. Rule.Parameters.Last_Child_Index loop
         if Rule.Boolean_Params (J) /= Unset then
            Args.Append
              (Rule_Argument'
                (Name  => To_Unbounded_Text
                            (Rule.Parameters.Child (J).
                             As_Parameter_Decl.F_Param_Identifier.Text),
                 Value => To_Unbounded_Text
                            (if Rule.Boolean_Params (J) = On
                             then "true" else "false")));
         end if;
      end loop;
   end Map_Parameters;

   overriding procedure Map_Parameters
     (Rule : One_Array_Parameter_Rule;
      Args : in out Rule_Argument_Vectors.Vector)
   is
      Param : Unbounded_Wide_Wide_String;
      C     : Wide_Wide_Character;
   begin
      if Length (Rule.Param) /= 0 then
         Append (Param, "[""");

         for J in 1 .. Length (Rule.Param) loop
            C := Element (Rule.Param, J);

            if C = ',' then
               Append (Param, """,""");
            else
               Append (Param, C);
            end if;
         end loop;

         Append (Param, """]");

         Args.Append
           (Rule_Argument'
             (Name  => To_Unbounded_Text
                         (Rule.Parameters.Child (2).
                          As_Parameter_Decl.F_Param_Identifier.Text),
              Value => To_Unbounded_Text (To_Wide_Wide_String (Param))));
      end if;
   end Map_Parameters;

   ------------------
   -- Rule_Comment --
   ------------------

   function Rule_Comment (Rule : Rule_Template) return String is
   begin
      return Rule.Help_Info.all;
   end Rule_Comment;

   ---------------
   -- Rule_Name --
   ---------------

   function Rule_Name (Rule : Rule_Template) return String is
   begin
      return Rule.Name.all;
   end Rule_Name;

   -----------------
   -- Rule_Option --
   -----------------

   function Rule_Option
     (Rule    : Rule_Template;
      Enabled : Boolean) return String is
   begin
      if Enabled then
         return "+R " & Rule_Name (Rule);
      else
         return "-R " & Rule_Name (Rule);
      end if;
   end Rule_Option;

   function Rule_Option
     (Rule    : One_Integer_Parameter_Rule;
      Enabled : Boolean) return String is
   begin
      if Enabled then
         return Rule_Option (Rule_Template (Rule), Enabled) & " :" &
                Rule.Param'Image;
      else
         return Rule_Option (Rule_Template (Rule), Enabled);
      end if;
   end Rule_Option;

   function Rule_Option
     (Rule    : One_Boolean_Parameter_Rule;
      Enabled : Boolean) return String is
   begin
      if Enabled and then Rule.Param = On then
         return Rule_Option (Rule_Template (Rule), Enabled) & " : " &
                To_String
                  (Rule.Parameters.Child (2).As_Parameter_Decl.
                   F_Param_Identifier.Text);
      else
         return Rule_Option (Rule_Template (Rule), Enabled);
      end if;
   end Rule_Option;

   function Rule_Option
     (Rule    : One_String_Parameter_Rule;
      Enabled : Boolean) return String is
   begin
      if Enabled and then Length (Rule.Param) /= 0 then
         return Rule_Option (Rule_Template (Rule), Enabled) & " : " &
                To_String (To_Wide_Wide_String (Rule.Param));
      else
         return Rule_Option (Rule_Template (Rule), Enabled);
      end if;
   end Rule_Option;

   function Rule_Option
     (Rule    : One_Integer_Or_Booleans_Parameter_Rule;
      Enabled : Boolean) return String
   is
      use Ada.Strings.Unbounded;

      Result    : Unbounded_String :=
        To_Unbounded_String (Rule_Option (Rule_Template (Rule), Enabled));
      Has_Param : Boolean := False;

   begin
      if Enabled then
         if Rule.Integer_Param /= Integer'First then
            Append (Result, ": " & Image (Rule.Integer_Param));
            Has_Param := True;
         end if;

         for J in Rule.Boolean_Params'Range loop
            if Rule.Boolean_Params (J) = On then
               Append (Result, (if Has_Param then ", " else ": "));
               Append (Result, To_String (Rule.Parameters.Child (J).
                                          As_Parameter_Decl.F_Param_Identifier.
                                          Text));
               Has_Param := True;
            end if;
         end loop;
      end if;

      return To_String (Result);
   end Rule_Option;

   --------------------
   -- Rule_Parameter --
   --------------------

   function Rule_Parameter
     (Rule : Rule_Template;
      Diag : String) return String
   is
      pragma Unreferenced (Diag);
   begin
      return "";
   end Rule_Parameter;

   ------------------
   -- Rule_Synonym --
   ------------------

   function Rule_Synonym (Rule : Rule_Template) return String is
   begin
      if Has_Synonym (Rule) then
         return Rule.User_Synonym.all;
      else
         return "";
      end if;
   end Rule_Synonym;

   ------------------
   -- Sample_Image --
   ------------------

   procedure Sample_Image
     (Rule         : Rule_Template'Class;
      Enabled      : Boolean;
      Sample_File  : File_Type;
      Comment_From : Positive) is
   begin
      Put (Sample_File, Rule_Option (Rule, Enabled));
      Set_Col (Sample_File, Positive_Count (Comment_From));
      Put_Line (Sample_File, "-- " & Rule_Comment (Rule));
   end Sample_Image;

   --------------------
   -- XML_Print_Rule --
   --------------------

   procedure XML_Print_Rule
     (Rule         : Rule_Template;
      Indent_Level : Natural := 0) is
   begin
      XML_Report
        ("<rule id=""" & Rule_Name (Rule) & """></rule>",
         Indent_Level);
   end XML_Print_Rule;

   overriding procedure XML_Print_Rule
     (Rule         : One_Integer_Parameter_Rule;
      Indent_Level : Natural := 0) is
   begin
      XML_Report
        ("<rule id=""" & Rule_Name (Rule) & """>",
         Indent_Level);

      if Rule.Param /= Integer'First then
         XML_Report
           ("<parameter>" & Image (Rule.Param) & "</parameter>",
            Indent_Level + 1);
      end if;

      XML_Report ("</rule>", Indent_Level);
   end XML_Print_Rule;

   overriding procedure XML_Print_Rule
     (Rule         : One_Boolean_Parameter_Rule;
      Indent_Level : Natural := 0) is
   begin
      XML_Report
        ("<rule id=""" & Rule_Name (Rule) & """>",
         Indent_Level);

      if Rule.Param = On then
         XML_Report
           ("<parameter>" &
            To_String (Rule.Parameters.Child (2).As_Parameter_Decl.
                       F_Param_Identifier.Text) &
            "</parameter>",
            Indent_Level + 1);
      end if;

      XML_Report ("</rule>", Indent_Level);
   end XML_Print_Rule;

   overriding procedure XML_Print_Rule
     (Rule         : One_String_Parameter_Rule;
      Indent_Level : Natural := 0) is
   begin
      XML_Report
        ("<rule id=""" & Rule_Name (Rule) & """>",
         Indent_Level);

      if Length (Rule.Param) /= 0 then
         XML_Report
           ("<parameter>" &
            To_String (To_Wide_Wide_String (Rule.Param)) &
            "</parameter>",
            Indent_Level + 1);
      end if;

      XML_Report ("</rule>", Indent_Level);
   end XML_Print_Rule;

   overriding procedure XML_Print_Rule
     (Rule         : One_Integer_Or_Booleans_Parameter_Rule;
      Indent_Level : Natural := 0) is
   begin
      XML_Report
        ("<rule id=""" & Rule_Name (Rule) & """>",
         Indent_Level);

      if Rule.Integer_Param /= Integer'First then
         XML_Report
           ("<parameter>" & Image (Rule.Integer_Param) & "</parameter>",
            Indent_Level + 1);
      end if;

      for J in Rule.Boolean_Params'Range loop
         if Rule.Boolean_Params (J) = On then
            XML_Report
              ("<parameter>" &
               To_String (Rule.Parameters.Child (J).
                          As_Parameter_Decl.F_Param_Identifier.Text),
               Indent_Level + 1);
         end if;
      end loop;

      XML_Report ("</rule>", Indent_Level);
   end XML_Print_Rule;

   -------------------
   -- XML_Rule_Help --
   -------------------

   procedure XML_Rule_Help (Rule : Rule_Template; Level : Natural) is
   begin
      Info_No_EOL (Level * Indent_String &
                   "<check switch=""+R"  &
                   Rule.Name.all         &
                   """ label="""         &
                   Rule.Help_Info.all    &
                   """");

      if Has_Tip (Rule_Template'Class (Rule)) then
         Info (">");
         XML_Rule_Help_Tip (Rule_Template'Class (Rule), Level + 1);
         Info (Level * Indent_String & "</check>");
      else
         Info ("/>");
      end if;
   end XML_Rule_Help;

   overriding procedure XML_Rule_Help
     (Rule  : One_Integer_Parameter_Rule;
      Level : Natural) is
   begin
      Info (Level * Indent_String            &
            "<spin switch=""+R"              &
            Rule.Name.all                    &
            """ label="""                    &
            Rule.Help_Info.all               &
            """ min=""0"""                   &
            " max=""99999"""                 &
            " default=""-1"""                &
            """ separator="":"""             &
            "/>");
   end XML_Rule_Help;

   overriding procedure XML_Rule_Help
     (Rule  : One_Boolean_Parameter_Rule;
      Level : Natural) is
   begin
      Info (Level * Indent_String            &
            "<field switch=""+R"             &
            Rule.Name.all                    &
            """ separator="":"""             &
            " label="""                      &
            Rule.Help_Info.all               &
            """/>");
      Info (Level * Indent_String            &
            "<check switch=""+R"             &
            Rule.Name.all & ":"              &
            To_String (Rule.Parameters.Child (2).As_Parameter_Decl.
                       F_Param_Identifier.Text) &
            """ label="""                    &
            Rule.Help_Info.all               &
            """/>");
   end XML_Rule_Help;

   overriding procedure XML_Rule_Help
     (Rule  : One_String_Parameter_Rule;
      Level : Natural) is
   begin
      Info (Level * Indent_String            &
            "<field switch=""+R"             &
            Rule.Name.all                    &
            """ separator="":"""             &
            " label="""                      &
            Rule.Help_Info.all               &
            """/>");
   end XML_Rule_Help;

   overriding procedure XML_Rule_Help
     (Rule  : One_Integer_Or_Booleans_Parameter_Rule;
      Level : Natural) is
   begin
      --  Should we do more here???
      XML_Rule_Help (Rule_Template (Rule), Level);
   end XML_Rule_Help;

   -----------------------
   -- XML_Rule_Help_Tip --
   -----------------------

   procedure XML_Rule_Help_Tip (Rule : Rule_Template; Level : Natural) is
   begin
      null;
   end XML_Rule_Help_Tip;

end Gnatcheck.Rules;

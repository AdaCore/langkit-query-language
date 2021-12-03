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
with Ada.Strings;                use Ada.Strings;
with Ada.Strings.Fixed;          use Ada.Strings.Fixed;
with GNAT.Directory_Operations;  use GNAT.Directory_Operations;

with Gnatcheck.Options;          use Gnatcheck.Options;
with Gnatcheck.Output;           use Gnatcheck.Output;
with Gnatcheck.String_Utilities; use Gnatcheck.String_Utilities;
with Gnatcheck.Rules.Rule_Table; use Gnatcheck.Rules.Rule_Table;

with Langkit_Support.Text;       use Langkit_Support.Text;

package body Gnatcheck.Rules is

   procedure Append_Param
     (Args  : in out Rule_Argument_Vectors.Vector;
      Name  : Wide_Wide_String;
      Value : Unbounded_Wide_Wide_String);
   --  Append to Args a parameter named Name with value Value if not empty,
   --  otherwise do nothing.

   procedure Append_Array_Param
     (Args  : in out Rule_Argument_Vectors.Vector;
      Name  : Wide_Wide_String;
      Value : Unbounded_Wide_Wide_String);
   --  Like Append_Param, for an array of strings represented by a comma
   --  separated list in Value.

   function Find_File (Name : String) return String;
   --  Return the pathname corresponding to Name, relative to either the
   --  current directory or the rule file if any. Return "" if no file found.

   procedure Load_Dictionary
     (File_Name : String;
      Rule      : in out Rule_Template'Class;
      Param     : in out Unbounded_Wide_Wide_String);
   --  Load dictionary file File_Name for rule Rule and append the result in
   --  Param as a comma separated list.

   function To_String (S : Unbounded_Wide_Wide_String) return String
   is (To_String (To_Wide_Wide_String (S)));
   --  Convert an Unbounded_Wide_Wide_String to a String

   ------------------------
   -- Append_Array_Param --
   ------------------------

   procedure Append_Array_Param
     (Args  : in out Rule_Argument_Vectors.Vector;
      Name  : Wide_Wide_String;
      Value : Unbounded_Wide_Wide_String)
   is
      Param : Unbounded_Wide_Wide_String;
      C     : Wide_Wide_Character;
   begin
      if Length (Value) /= 0 then
         Append (Param, "[""");

         for J in 1 .. Length (Value) loop
            C := Element (Value, J);

            if C = ',' then
               Append (Param, """,""");
            else
               Append (Param, C);
            end if;
         end loop;

         Append (Param, """]");
         Args.Append
           (Rule_Argument'
             (Name  => To_Unbounded_Text (Name),
              Value => To_Unbounded_Text (To_Wide_Wide_String (Param))));
      end if;
   end Append_Array_Param;

   ------------------
   -- Append_Param --
   ------------------

   procedure Append_Param
     (Args  : in out Rule_Argument_Vectors.Vector;
      Name  : Wide_Wide_String;
      Value : Unbounded_Wide_Wide_String) is
   begin
      if Length (Value) /= 0 then
         Args.Append
           (Rule_Argument'
             (Name  => To_Unbounded_Text (Name),
              Value => To_Unbounded_Text
                         ('"' & To_Wide_Wide_String (Value) & '"')));
      end if;
   end Append_Param;

   -------------------
   -- Annotate_Rule --
   -------------------

   function Annotate_Rule (Rule : Rule_Template) return String is
   begin
      if Subprocess_Mode then
         return " [" & Rule_Name (Rule) & "]";
      elsif not Mapping_Mode then
         return "";
      elsif Has_Synonym (Rule) then
         return " [" & Rule_Synonym (Rule) & "]";
      else
         return " [" & Rule_Name (Rule) & "]";
      end if;
   end Annotate_Rule;

   ---------------
   -- Find_File --
   ---------------

   function Find_File (Name : String) return String is
      Rule_File_Dir : constant String :=
        Dir_Name
          (Gnatcheck.Rules.Rule_Table.Processed_Rule_File_Name);

   begin
      if Is_Regular_File (Rule_File_Dir & Name) then
         return Rule_File_Dir & Name;
      elsif Is_Regular_File (Name) then
         return Name;
      else
         return "";
      end if;
   end Find_File;

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

   ----------------
   -- Is_Enabled --
   ----------------

   function Is_Enabled (Rule : Rule_Template) return Boolean is
   begin
      return Rule.Rule_State = Enabled;
   end Is_Enabled;

   ---------------------
   -- Load_Dictionary --
   ---------------------

   procedure Load_Dictionary
     (File_Name : String;
      Rule      : in out Rule_Template'Class;
      Param     : in out Unbounded_Wide_Wide_String)
   is
      Name : constant String := Find_File (File_Name);
      File : File_Type;
      Line : String (1 .. 1024);
      Len  : Natural;

   begin
      if Name /= "" then
         Open (File, In_File, Name);

         while not End_Of_File (File) loop
            Get_Line (File, Line, Len);

            --  Skip empty and comment lines

            declare
               S : constant String := Remove_Spaces (Line (1 .. Len));
            begin
               if S'Length > 0
                 and then (S'Length < 2
                           or else S (S'First .. S'First + 1) /= "--")
               then
                  if Length (Param) /= 0 then
                     Append (Param, ",");
                  end if;

                  Append (Param, To_Wide_Wide_String (S));
               end if;
            end;
         end loop;

         Close (File);
         Rule.Rule_State := Enabled;

      else
         Error ("(" & Rule.Name.all & "): cannot load file " & File_Name);
         Rule.Rule_State := Disabled;
      end if;

   exception
      when others =>
         Error ("(" & Rule.Name.all & "): cannot load file " & File_Name);
         Rule.Rule_State := Disabled;
   end Load_Dictionary;

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
         Put (Rule_File, ":" & Image (Rule.Param));
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
              ":" &
              To_String (Rule.Parameters.Child (2).As_Parameter_Decl.
                         F_Param_Identifier.Text));
      end if;
   end Print_Rule_To_File;

   overriding procedure Print_Rule_To_File
     (Rule         : One_String_Parameter_Rule;
      Rule_File    : File_Type;
      Indent_Level : Natural := 0)
   is
      use Ada.Strings.Unbounded;
   begin
      Print_Rule_To_File (Rule_Template (Rule), Rule_File, Indent_Level);

      if Length (Rule.File) /= 0 then
         Put (Rule_File, ":" & To_String (Rule.File));

      elsif Length (Rule.Param) /= 0 then
         Put (Rule_File, ":" & To_String (Rule.Param));
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
         Put (Rule_File, ":" & Image (Rule.Integer_Param));
         Has_Param := True;
      end if;

      for J in Rule.Boolean_Params'Range loop
         if Rule.Boolean_Params (J) = On then
            Put (Rule_File, (if Has_Param then "," else ":"));
            Put (Rule_File, To_String (Rule.Parameters.Child (J).
                                       As_Parameter_Decl.F_Param_Identifier.
                                       Text));
            Has_Param := True;
         end if;
      end loop;
   end Print_Rule_To_File;

   overriding procedure Print_Rule_To_File
     (Rule         : Identifier_Prefixes_Rule;
      Rule_File    : File_Type;
      Indent_Level : Natural := 0)
   is
      First_Param       : Boolean         := True;
      Rule_Name_Padding : constant String :=
        [1 .. Rule.Name'Length + 3 => ' '];

      procedure Print
        (Param  : String;
         Prefix : Unbounded_Wide_Wide_String;
         Force  : Boolean := False);
      --  Print value Prefix of parameter Param if not null or if Force is set

      -----------
      -- Print --
      -----------

      procedure Print
        (Param  : String;
         Prefix : Unbounded_Wide_Wide_String;
         Force  : Boolean := False) is
      begin
         if Force or else Length (Prefix) /= 0 then
            if First_Param then
               Put (Rule_File, ":" & Param & "=" & To_String (Prefix));
               First_Param := False;
            else
               Put_Line (Rule_File, ",");

               for J in 1 .. Indent_Level loop
                  Put (Rule_File, Get_Indent_String);
               end loop;

               Put (Rule_File,
                    Rule_Name_Padding & Param & "=" & To_String (Prefix));
            end if;
         end if;
      end Print;

   begin
      Print_Rule_To_File (Rule_Template (Rule), Rule_File, Indent_Level);

      Print ("Type", Rule.Type_Prefix);
      Print ("Concurrent", Rule.Concurrent_Prefix);
      Print ("Access", Rule.Access_Prefix);
      Print ("Class_Access", Rule.Class_Access_Prefix);
      Print ("Subprogram_Access", Rule.Subprogram_Access_Prefix);
      Print ("Constant", Rule.Constant_Prefix);
      Print ("Exception", Rule.Exception_Prefix);
      Print ("Enum", Rule.Enum_Prefix);

      if Length (Rule.Derived_Prefix) /= 0 then
         Print ("Derived", Null_Unbounded_Wide_Wide_String, Force => True);

         for J in 1 .. Length (Rule.Derived_Prefix) loop
            declare
               C : constant Character :=
                 To_Character (Element (Rule.Derived_Prefix, J));
            begin
               if C /= ',' then
                  Put (Rule_File, C);
               else
                  Print ("Derived",
                         Null_Unbounded_Wide_Wide_String,
                         Force => True);
               end if;
            end;
         end loop;
      end if;

      --  We have to print out Exclusive parameter, but this would make sense
      --  only if at least one prefix is specified

      if not First_Param and then Rule.Exclusive /= Off then
         Put_Line (Rule_File, ",");
         Put (Rule_File, Rule_Name_Padding & "Exclusive");
      end if;
   end Print_Rule_To_File;

   overriding procedure Print_Rule_To_File
     (Rule         : Identifier_Suffixes_Rule;
      Rule_File    : File_Type;
      Indent_Level : Natural := 0)
   is
      First_Param       : Boolean         := True;
      Rule_Name_Padding : constant String :=
        [1 .. Rule.Name'Length + 3 => ' '];

      procedure Print (Param : String; Suffix : Unbounded_Wide_Wide_String);
      --  Print value Suffix of parameter Param if not null

      -----------
      -- Print --
      -----------

      procedure Print (Param : String; Suffix : Unbounded_Wide_Wide_String) is
      begin
         if Length (Suffix) /= 0 then
            if First_Param then
               Put (Rule_File, ":" & Param & "=" & To_String (Suffix));
               First_Param := False;
            else
               Put_Line (Rule_File, ",");

               for J in 1 .. Indent_Level loop
                  Put (Rule_File, Get_Indent_String);
               end loop;

               Put (Rule_File,
                    Rule_Name_Padding & Param & "=" & To_String (Suffix));
            end if;
         end if;
      end Print;

   begin
      Print_Rule_To_File (Rule_Template (Rule), Rule_File, Indent_Level);

      Print ("Type_Suffix", Rule.Type_Suffix);
      Print ("Access_Suffix", Rule.Access_Suffix);

      if Length (Rule.Access_Access_Suffix) /= 0 then
         Put (Rule_File, "(" & To_String (Rule.Access_Access_Suffix) & ")");
      end if;

      Print ("Class_Subtype_Suffix", Rule.Class_Subtype_Suffix);
      Print ("Class_Access_Suffix", Rule.Class_Access_Suffix);
      Print ("Constant_Suffix", Rule.Constant_Suffix);
      Print ("Renaming_Suffix", Rule.Renaming_Suffix);
      Print ("Access_Obj_Suffix", Rule.Access_Obj_Suffix);
      Print ("Interrupt_Suffix", Rule.Interrupt_Suffix);
   end Print_Rule_To_File;

   overriding procedure Print_Rule_To_File
     (Rule         : Identifier_Casing_Rule;
      Rule_File    : File_Type;
      Indent_Level : Natural := 0)
   is
      First_Param       : Boolean         := True;
      Rule_Name_Padding : constant String :=
        [1 .. Rule.Name'Length + 3 => ' '];

      procedure Print
        (Param  : String;
         Casing : Unbounded_Wide_Wide_String;
         Force  : Boolean := False);
      --  Print value Casing of parameter Param if not null or if Force is set

      -----------
      -- Print --
      -----------

      procedure Print
        (Param  : String;
         Casing : Unbounded_Wide_Wide_String;
         Force  : Boolean := False) is
      begin
         if Force or else Length (Casing) /= 0 then
            if First_Param then
               Put (Rule_File, ":" & Param & "=" & To_String (Casing));
               First_Param := False;
            else
               Put_Line (Rule_File, ",");

               for J in 1 .. Indent_Level loop
                  Put (Rule_File, Get_Indent_String);
               end loop;

               Put (Rule_File,
                    Rule_Name_Padding & Param & "=" & To_String (Casing));
            end if;
         end if;
      end Print;

      C : Character;

   begin
      Print_Rule_To_File (Rule_Template (Rule), Rule_File, Indent_Level);

      Print ("Type", Rule.Type_Casing);
      Print ("Enum", Rule.Enum_Casing);
      Print ("Constant", Rule.Constant_Casing);
      Print ("Exception", Rule.Exception_Casing);
      Print ("Others", Rule.Others_Casing);

      if Length (Rule.Exclude) /= 0 then
         Print ("Exclude", Null_Unbounded_Wide_Wide_String, Force => True);

         for J in 1 .. Length (Rule.Exclude) loop
            C := To_Character (Element (Rule.Exclude, J));

            if C /= ',' then
               Put (Rule_File, C);
            else
               Print ("Exclude",
                      Null_Unbounded_Wide_Wide_String,
                      Force => True);
            end if;
         end loop;
      end if;
   end Print_Rule_To_File;

   overriding procedure Print_Rule_To_File
     (Rule         : Forbidden_Rule;
      Rule_File    : File_Type;
      Indent_Level : Natural := 0)
   is
      First_Param : Boolean := True;

      procedure Print (Items : Unbounded_Wide_Wide_String);
      --  Print Items as parameters if not empty

      -----------
      -- Print --
      -----------

      procedure Print (Items : Unbounded_Wide_Wide_String) is
      begin
         if Length (Items) = 0 then
            return;
         end if;

         if First_Param then
            Put (Rule_File, ":");
            First_Param := False;
         else
            Put (Rule_File, ",");
         end if;

         Put (Rule_File, To_String (To_Wide_Wide_String (Items)));
      end Print;

   begin
      Print_Rule_To_File (Rule_Template (Rule), Rule_File, Indent_Level);

      if Rule.All_Flag = On then
         Report_No_EOL (":ALL");
         First_Param := False;
      else
         Print (Rule.Forbidden);
      end if;

      if Length (Rule.Allowed) /= 0 then
         New_Line (Rule_File);
         First_Param := True;

         for J in 1 .. Indent_Level loop
            Put (Rule_File, Get_Indent_String);
         end loop;

         Put (Rule_File, "-R" & Rule_Name (Rule));

         Print (Rule.Allowed);
      end if;
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
            if Check_Param_Redefinition and then Rule.Rule_State = Enabled then
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

      elsif Enable then
         if Check_Param_Redefinition and then Rule.Rule_State = Enabled then
            Error
             ("redefining at " & Defined_Str (Defined_At) &
              " parameter " & Param & " for rule " & Rule.Name.all &
              " defined at " & Defined_Str (Rule.Defined_At.all));
         end if;

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
      Defined_At : String) is
   begin
      if Param = "" then
         if Enable then
            Error ("(" & Rule.Name.all & ") parameter is required for +R");
         else
            Rule.Rule_State := Disabled;
         end if;
      elsif Enable then
         if Check_Param_Redefinition and then Rule.Rule_State = Enabled then
            Error
             ("redefining at " & Defined_Str (Defined_At) &
              " parameter " & Param & " for rule " & Rule.Name.all &
              " defined at " & Defined_Str (Rule.Defined_At.all));
         end if;

         --  Headers rule takes a file name as parameter, containing the
         --  header contents.

         if Rule.Name.all = "headers" then
            declare
               Name : constant String := Find_File (Param);
               Str  : String_Access;
               Last : Natural;

            begin
               if Name /= "" then
                  Str := Read_File (Name);
                  Ada.Strings.Unbounded.Set_Unbounded_String (Rule.File, Name);
               else
                  Error ("(" & Rule.Name.all & "): cannot load file " & Param);
                  Rule.Rule_State := Disabled;
                  return;
               end if;

               Last := Str'Last;

               --  Strip trailing end of line

               if Str (Str'Last) = ASCII.LF then
                  Last := Last - 1;
               end if;

               Append (Rule.Param, To_Wide_Wide_String (Str (1 .. Last)));
               Free (Str);
            end;
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
            Rule.Boolean_Params := [others => Unset];
            Rule.Rule_State := Disabled;
         end if;
      else
         if Enable then
            if Check_Param_Redefinition and then Rule.Rule_State = Enabled then
               Error
                ("redefining at " & Defined_Str (Defined_At) &
                 " parameter " & Param & " for rule " & Rule.Name.all &
                 " defined at " & Defined_Str (Rule.Defined_At.all));
            end if;

            --  First try to extract an integer

            begin
               Rule.Integer_Param := Integer'Value (Param);

               if Rule.Integer_Param >= 0 then
                  Rule.Rule_State := Enabled;
                  Rule.Defined_At := new String'(Defined_At);
               else
                  Error ("(" & Rule.Name.all & ") wrong parameter: " &
                         Param);
                  Rule.Integer_Param := Integer'First;
                  Rule.Boolean_Params := [others => Unset];
                  Rule.Rule_State := Disabled;
               end if;

               return;
            exception
               when Constraint_Error =>
                  null;
            end;

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
            Rule.Boolean_Params := [others => Unset];
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
         if Check_Param_Redefinition and then Rule.Rule_State = Enabled then
            Error
             ("redefining at " & Defined_Str (Defined_At) &
              " parameter " & Param & " for rule " & Rule.Name.all &
              " defined at " & Defined_Str (Rule.Defined_At.all));
         end if;

         if Rule.Name.all = "name_clashes" then
            Ada.Strings.Unbounded.Set_Unbounded_String (Rule.File, Param);
            Load_Dictionary (Param, Rule, Rule.Param);
            Rule.Defined_At := new String'(Defined_At);
         else
            if Length (Rule.Param) /= 0 then
               Append (Rule.Param, ",");
            end if;

            Append (Rule.Param, To_Wide_Wide_String (Param));
            Rule.Rule_State := Enabled;
            Rule.Defined_At := new String'(Defined_At);
         end if;
      else
         Set_Unbounded_Wide_Wide_String (Rule.Param, "");
         Error ("(" & Rule.Name.all & ") no parameter allowed for -R");
         Rule.Rule_State := Disabled;
         Rule.Defined_At := new String'(Defined_At);
      end if;
   end Process_Rule_Parameter;

   overriding procedure Process_Rule_Parameter
     (Rule       : in out Identifier_Suffixes_Rule;
      Param      : String;
      Enable     : Boolean;
      Defined_At : String)
   is
      Paren_Index : Natural;
      Norm_Param  : constant String := Remove_Spaces (Param);
      Lower_Param : constant String := To_Lower (Param);

   begin
      if Norm_Param = "" then
         if Enable then
            Rule.Rule_State := Enabled;
         else
            Rule.Rule_State := Disabled;
         end if;
      elsif Enable then
         Rule.Rule_State := Enabled;
         Rule.Defined_At := new String'(Defined_At);

         if Lower_Param = "default" then
            Set_Unbounded_Wide_Wide_String (Rule.Type_Suffix, "_T");
            Set_Unbounded_Wide_Wide_String (Rule.Access_Suffix, "_A");
            Set_Unbounded_Wide_Wide_String (Rule.Access_Access_Suffix, "");
            Set_Unbounded_Wide_Wide_String (Rule.Class_Access_Suffix, "");
            Set_Unbounded_Wide_Wide_String (Rule.Class_Subtype_Suffix, "");
            Set_Unbounded_Wide_Wide_String (Rule.Constant_Suffix, "_C");
            Set_Unbounded_Wide_Wide_String (Rule.Renaming_Suffix, "_R");
            Set_Unbounded_Wide_Wide_String (Rule.Access_Obj_Suffix, "");
            Set_Unbounded_Wide_Wide_String (Rule.Interrupt_Suffix, "");

         elsif Has_Prefix (Norm_Param, "type_suffix=") then
            Set_Unbounded_Wide_Wide_String
              (Rule.Type_Suffix,
               To_Wide_Wide_String
                 (Norm_Param (Norm_Param'First + 12 .. Norm_Param'Last)));

         elsif Has_Prefix (Norm_Param, "access_suffix=") then
            if Norm_Param (Norm_Param'Last) = ')' then
               Paren_Index :=
                 Index
                   (Norm_Param (Norm_Param'First + 14 .. Norm_Param'Last),
                    "(");
               Set_Unbounded_Wide_Wide_String
                 (Rule.Access_Suffix,
                  To_Wide_Wide_String
                    (Norm_Param (Norm_Param'First + 14 .. Paren_Index - 1)));
               Set_Unbounded_Wide_Wide_String
                 (Rule.Access_Access_Suffix,
                  To_Wide_Wide_String
                    (Norm_Param (Paren_Index + 1 .. Norm_Param'Last - 1)));

            else
               Set_Unbounded_Wide_Wide_String
                 (Rule.Access_Suffix,
                  To_Wide_Wide_String
                    (Norm_Param (Norm_Param'First + 14 .. Norm_Param'Last)));
               Set_Unbounded_Wide_Wide_String
                 (Rule.Access_Access_Suffix, "");
            end if;

         elsif Has_Prefix (Norm_Param, "class_access_suffix=") then
            Set_Unbounded_Wide_Wide_String
              (Rule.Class_Access_Suffix,
               To_Wide_Wide_String
                 (Norm_Param (Norm_Param'First + 20 .. Norm_Param'Last)));

         elsif Has_Prefix (Norm_Param, "class_subtype_suffix=") then
            Set_Unbounded_Wide_Wide_String
              (Rule.Class_Subtype_Suffix,
               To_Wide_Wide_String
                 (Norm_Param (Norm_Param'First + 21 .. Norm_Param'Last)));

         elsif Has_Prefix (Norm_Param, "constant_suffix=") then
            Set_Unbounded_Wide_Wide_String
              (Rule.Constant_Suffix,
               To_Wide_Wide_String
                 (Norm_Param (Norm_Param'First + 16 .. Norm_Param'Last)));

         elsif Has_Prefix (Norm_Param, "renaming_suffix=") then
            Set_Unbounded_Wide_Wide_String
              (Rule.Renaming_Suffix,
               To_Wide_Wide_String
                 (Norm_Param (Norm_Param'First + 16 .. Norm_Param'Last)));

         elsif Has_Prefix (Norm_Param, "access_obj_suffix=") then
            Set_Unbounded_Wide_Wide_String
              (Rule.Access_Obj_Suffix,
               To_Wide_Wide_String
                 (Norm_Param (Norm_Param'First + 18 .. Norm_Param'Last)));

         elsif Has_Prefix (Norm_Param, "interrupt_suffix=") then
            Set_Unbounded_Wide_Wide_String
              (Rule.Interrupt_Suffix,
               To_Wide_Wide_String
                 (Norm_Param (Norm_Param'First + 17 .. Norm_Param'Last)));
         else
            Error ("(" & Rule.Name.all & ") wrong parameter: " & Param);
            Rule.Rule_State := Disabled;
         end if;
      else
         Rule.Rule_State := Disabled;
         Rule.Defined_At := new String'(Defined_At);

         if Lower_Param = "all_suffixes" then
            Set_Unbounded_Wide_Wide_String (Rule.Type_Suffix, "");
            Set_Unbounded_Wide_Wide_String (Rule.Access_Suffix, "");
            Set_Unbounded_Wide_Wide_String (Rule.Access_Access_Suffix, "");
            Set_Unbounded_Wide_Wide_String (Rule.Class_Access_Suffix, "");
            Set_Unbounded_Wide_Wide_String (Rule.Class_Subtype_Suffix, "");
            Set_Unbounded_Wide_Wide_String (Rule.Constant_Suffix, "");
            Set_Unbounded_Wide_Wide_String (Rule.Renaming_Suffix, "");
            Set_Unbounded_Wide_Wide_String (Rule.Access_Obj_Suffix, "");
            Set_Unbounded_Wide_Wide_String (Rule.Interrupt_Suffix, "");

         elsif Lower_Param = "type_suffix" then
            Set_Unbounded_Wide_Wide_String (Rule.Type_Suffix, "");
         elsif Lower_Param = "access_suffix" then
            Set_Unbounded_Wide_Wide_String (Rule.Access_Suffix, "");
            Set_Unbounded_Wide_Wide_String (Rule.Access_Access_Suffix, "");
         elsif Lower_Param = "class_access_suffix" then
            Set_Unbounded_Wide_Wide_String (Rule.Class_Access_Suffix, "");
         elsif Lower_Param = "class_subtype_suffix" then
            Set_Unbounded_Wide_Wide_String (Rule.Class_Subtype_Suffix, "");
         elsif Lower_Param = "constant_suffix" then
            Set_Unbounded_Wide_Wide_String (Rule.Constant_Suffix, "");
         elsif Lower_Param = "renaming_suffix" then
            Set_Unbounded_Wide_Wide_String (Rule.Renaming_Suffix, "");
         elsif Lower_Param = "access_obj_suffix" then
            Set_Unbounded_Wide_Wide_String (Rule.Access_Obj_Suffix, "");
         elsif Lower_Param = "interrupt_suffix" then
            Set_Unbounded_Wide_Wide_String (Rule.Interrupt_Suffix, "");
         else
            Error ("(" & Rule.Name.all & ") wrong parameter: " & Param);
         end if;
      end if;
   end Process_Rule_Parameter;

   overriding procedure Process_Rule_Parameter
     (Rule       : in out Identifier_Prefixes_Rule;
      Param      : String;
      Enable     : Boolean;
      Defined_At : String)
   is
      Col_Index   : Natural;
      Norm_Param  : constant String := Remove_Spaces (Param);
      Lower_Param : constant String := To_Lower (Param);

   begin
      if Norm_Param = "" then
         if Enable then
            Rule.Rule_State := Enabled;
         else
            Rule.Rule_State := Disabled;
         end if;
      elsif Enable then
         Rule.Rule_State := Enabled;
         Rule.Defined_At := new String'(Defined_At);

         if Lower_Param = "exclusive" then
            Rule.Exclusive := On;

         elsif Has_Prefix (Norm_Param, "type=") then
            Set_Unbounded_Wide_Wide_String
              (Rule.Type_Prefix,
               To_Wide_Wide_String
                 (Norm_Param (Norm_Param'First + 5 .. Norm_Param'Last)));

         elsif Has_Prefix (Norm_Param, "concurrent=") then
            Set_Unbounded_Wide_Wide_String
              (Rule.Concurrent_Prefix,
               To_Wide_Wide_String
                 (Norm_Param (Norm_Param'First + 11 .. Norm_Param'Last)));

         elsif Has_Prefix (Norm_Param, "access=") then
            Set_Unbounded_Wide_Wide_String
              (Rule.Access_Prefix,
               To_Wide_Wide_String
                 (Norm_Param (Norm_Param'First + 7 .. Norm_Param'Last)));

         elsif Has_Prefix (Norm_Param, "class_access=") then
            Set_Unbounded_Wide_Wide_String
              (Rule.Class_Access_Prefix,
               To_Wide_Wide_String
                 (Norm_Param (Norm_Param'First + 13 .. Norm_Param'Last)));

         elsif Has_Prefix (Norm_Param, "subprogram_access=") then
            Set_Unbounded_Wide_Wide_String
              (Rule.Subprogram_Access_Prefix,
               To_Wide_Wide_String
                 (Norm_Param (Norm_Param'First + 18 .. Norm_Param'Last)));

         elsif Has_Prefix (Norm_Param, "derived=") then
            Col_Index :=
              Index
                (Norm_Param (Norm_Param'First + 8 .. Norm_Param'Last), ":");

            if Col_Index /= 0 then
               if Length (Rule.Derived_Prefix) /= 0 then
                  Append (Rule.Derived_Prefix, ",");
               end if;

               Append
                 (Rule.Derived_Prefix,
                  To_Wide_Wide_String
                    (To_Lower
                      (Norm_Param (Norm_Param'First + 8 .. Col_Index - 1))));
               Append
                 (Rule.Derived_Prefix,
                  To_Wide_Wide_String
                    (Norm_Param (Col_Index .. Norm_Param'Last)));

            else
               Error ("(" & Rule.Name.all & ") wrong parameter: " & Param);
               Rule.Rule_State := Disabled;
            end if;

         elsif Has_Prefix (Norm_Param, "constant=") then
            Set_Unbounded_Wide_Wide_String
              (Rule.Constant_Prefix,
               To_Wide_Wide_String
                 (Norm_Param (Norm_Param'First + 9 .. Norm_Param'Last)));

         elsif Has_Prefix (Norm_Param, "exception=") then
            Set_Unbounded_Wide_Wide_String
              (Rule.Exception_Prefix,
               To_Wide_Wide_String
                 (Norm_Param (Norm_Param'First + 10 .. Norm_Param'Last)));

         elsif Has_Prefix (Norm_Param, "enum=") then
            Set_Unbounded_Wide_Wide_String
              (Rule.Enum_Prefix,
               To_Wide_Wide_String
                 (Norm_Param (Norm_Param'First + 5 .. Norm_Param'Last)));

         else
            Error ("(" & Rule.Name.all & ") wrong parameter: " & Param);
            Rule.Rule_State := Disabled;
         end if;
      else
         Rule.Rule_State := Disabled;
         Rule.Defined_At := new String'(Defined_At);

         if Lower_Param = "exclusive" then
            Rule.Exclusive := Off;

         elsif Lower_Param = "all_prefixes" then
            Set_Unbounded_Wide_Wide_String (Rule.Type_Prefix, "");
            Set_Unbounded_Wide_Wide_String (Rule.Concurrent_Prefix, "");
            Set_Unbounded_Wide_Wide_String (Rule.Access_Prefix, "");
            Set_Unbounded_Wide_Wide_String (Rule.Class_Access_Prefix, "");
            Set_Unbounded_Wide_Wide_String (Rule.Subprogram_Access_Prefix, "");
            Set_Unbounded_Wide_Wide_String (Rule.Derived_Prefix, "");
            Set_Unbounded_Wide_Wide_String (Rule.Constant_Prefix, "");
            Set_Unbounded_Wide_Wide_String (Rule.Exception_Prefix, "");
            Set_Unbounded_Wide_Wide_String (Rule.Enum_Prefix, "");

         elsif Lower_Param = "type" then
            Set_Unbounded_Wide_Wide_String (Rule.Type_Prefix, "");
         elsif Lower_Param = "concurrent" then
            Set_Unbounded_Wide_Wide_String (Rule.Concurrent_Prefix, "");
         elsif Lower_Param = "access" then
            Set_Unbounded_Wide_Wide_String (Rule.Access_Prefix, "");
         elsif Lower_Param = "class_access" then
            Set_Unbounded_Wide_Wide_String (Rule.Class_Access_Prefix, "");
         elsif Lower_Param = "subprogram_access" then
            Set_Unbounded_Wide_Wide_String (Rule.Subprogram_Access_Prefix, "");
         elsif Lower_Param = "derived" then
            Set_Unbounded_Wide_Wide_String (Rule.Derived_Prefix, "");
         elsif Lower_Param = "constant" then
            Set_Unbounded_Wide_Wide_String (Rule.Constant_Prefix, "");
         elsif Lower_Param = "exception" then
            Set_Unbounded_Wide_Wide_String (Rule.Exception_Prefix, "");
         elsif Lower_Param = "enum" then
            Set_Unbounded_Wide_Wide_String (Rule.Enum_Prefix, "");
         else
            Error ("(" & Rule.Name.all & ") wrong parameter: " & Param);
         end if;
      end if;
   end Process_Rule_Parameter;

   overriding procedure Process_Rule_Parameter
     (Rule       : in out Identifier_Casing_Rule;
      Param      : String;
      Enable     : Boolean;
      Defined_At : String)
   is
      procedure Check_And_Set
        (S     : in out Unbounded_Wide_Wide_String;
         Val   : String;
         Label : String);

      procedure Check_And_Set
        (S     : in out Unbounded_Wide_Wide_String;
         Val   : String;
         Label : String) is
      begin
         if Check_Param_Redefinition and then Length (S) /= 0 then
            Error
             ("redefining at " & Defined_Str (Defined_At) & " " &
              Label & " casing for rule " & Rule.Name.all);
         end if;

         Set_Unbounded_Wide_Wide_String (S, To_Wide_Wide_String (Val));
      end Check_And_Set;

      Norm_Param : constant String := To_Lower (Remove_Spaces (Param));
   begin
      if Norm_Param = "" then
         if Enable then
            Rule.Rule_State := Enabled;
         else
            Rule.Rule_State := Disabled;
         end if;
      elsif Enable then
         Rule.Rule_State := Enabled;
         Rule.Defined_At := new String'(Defined_At);

         if Has_Prefix (Norm_Param, "type=") then
            Check_And_Set
              (Rule.Type_Casing,
               Norm_Param (Norm_Param'First + 5 .. Norm_Param'Last),
               "type");

         elsif Has_Prefix (Norm_Param, "enum=") then
            Check_And_Set
              (Rule.Enum_Casing,
               Norm_Param (Norm_Param'First + 5 .. Norm_Param'Last),
               "enumeration literal");

         elsif Has_Prefix (Norm_Param, "constant=") then
            Check_And_Set
              (Rule.Constant_Casing,
               Norm_Param (Norm_Param'First + 9 .. Norm_Param'Last),
               "constant");

         elsif Has_Prefix (Norm_Param, "exception=") then
            Check_And_Set
              (Rule.Exception_Casing,
               Norm_Param (Norm_Param'First + 10 .. Norm_Param'Last),
               "exception");

         elsif Has_Prefix (Norm_Param, "others=") then
            Check_And_Set
              (Rule.Others_Casing,
               Norm_Param (Norm_Param'First + 7 .. Norm_Param'Last),
               "others");

         elsif Has_Prefix (Norm_Param, "exclude=") then
            Load_Dictionary
              (Norm_Param (Norm_Param'First + 8 .. Norm_Param'Last),
               Rule, Rule.Exclude);

         else
            Error ("(" & Rule.Name.all & ") wrong parameter: " & Param);
            Rule.Rule_State := Disabled;
         end if;
      else
         Error ("(" & Rule.Name.all & ") no parameter allowed for -R");
         Rule.Rule_State := Disabled;
         Rule.Defined_At := new String'(Defined_At);
      end if;
   end Process_Rule_Parameter;

   GNAT_Attributes : constant Wide_Wide_String :=
     "abort_signal,address_size,asm_input,asm_output," &
     "atomic_always_lock_free,bit,bit_position,code_address," &
     "compiler_version,constant_indexing,default_bit_order," &
     "default_scalar_storage_order,default_iterator,deref," &
     "elaborated,enabled,fast_math,finalization_size,fixed_value," &
     "has_access_values,has_discriminants,has_tagged_values," &
     "implicit_dereference,initialized,integer_value,invalid_value," &
     "iterator_element,iterable,library_level,lock_free,loop_entry," &
     "machine_size,max_integer_size,maximum_alignment,mechanism_code," &
     "null_parameter,object_size,overlaps_storage,passed_by_reference," &
     "pool_address,range_length,ref,restriction_set,result," &
     "scalar_storage_order,small_denominator,small_numerator,storage_unit," &
     "system_allocator_alignment,target_name,to_address,type_class," &
     "type_key,unconstrained_array,universal_literal_string," &
     "unrestricted_access,update,vads_size,valid_scalars,value_size," &
     "variable_indexing,wchar_t_size,word_size,from_any,img,to_any," &
     "typecode,valid_value,elab_body,elab_spec,elab_subp_body," &
     "simple_storage_pool,stub_type,secondary_stack_size";
   --  List of GNAT implementation defined attributes

   GNAT_Pragmas : constant Wide_Wide_String :=
     "ada_83,ada_95,ada_05,ada_2005,ada_12,ada_2012,ada_2022," &
     "aggregate_individually_assign,allow_integer_address,annotate," &
     "assume_no_invalid_values,c_pass_by_copy,check_float_overflow," &
     "check_name,check_policy,compile_time_error,compile_time_warning," &
     "compiler_unit,compiler_unit_warning,component_alignment," &
     "convention_identifier,debug_policy,default_scalar_storage_order," &
     "disable_atomic_synchronization,elaboration_checks,eliminate," &
     "enable_atomic_synchronization,extend_system,extensions_allowed," &
     "external_name_casing,fast_math,favor_top_level,gnat_annotate," &
     "ignore_pragma implicit_packing,initialize_scalars,interrupt_state," &
     "license,no_component_reordering,no_heap_finalization,no_run_time," &
     "no_strict_aliasing,optimize_alignment,overflow_mode," &
     "overriding_renamings,persistent_bss,prefix_exception_messages," &
     "profile_warnings,propagate_exceptions,rational,ravenscar," &
     "rename_pragma,restricted_run_time,restriction_warnings," &
     "short_circuit_and_or,short_descriptors,source_file_name," &
     "source_file_name_project,spark_mode,style_checks," &
     "suppress_exception_locations,unevaluated_use_of_old,use_vads_size," &
     "validity_checks,warning_as_error,warnings,wide_character_encoding," &
     "abort_defer,abstract_state,assert_and_cut,assume,async_readers," &
     "async_writers,attribute_definition,check,comment,common_object," &
     "complete_representation,complex_representation," &
     "constant_after_elaboration,contract_cases,cpp_class,cpp_constructor," &
     "cpp_virtual,cpp_vtable,cuda_execute,cuda_global,deadline_floor," &
     "debug,default_initial_condition,depends,effective_reads," &
     "effective_writes,export_function,export_object,export_procedure," &
     "export_valued_procedure,extensions_visible,external," &
     "finalize_storage_only,ghost,global,ident,implementation_defined," &
     "import_function,import_object,import_procedure," &
     "import_valued_procedure,initial_condition,initializes,inline_always," &
     "inline_generic,interface,interface_name,interrupt_priority," &
     "invariant,keep_names,link_with,linker_alias,linker_constructor," &
     "linker_destructor,linker_section,lock_free,loop_invariant," &
     "loop_optimize,loop_variant,machine_attribute,main,main_storage," &
     "max_entry_queue_depth,max_queue_length,no_body,no_caching," &
     "no_elaboration_code_all,no_inline,no_tagged_streams,obsolescent," &
     "ordered,part_of,passive,post,postcondition,post_class,pre," &
     "precondition,predicate,pre_class,provide_shift_operators," &
     "psect_object,pure_function,refined_depends,refined_global," &
     "refined_post,refined_state,remote_access_type,secondary_stack_size," &
     "share_generic,simple_storage_pool_type,source_reference," &
     "static_elaboration_desired,stream_convert,subprogram_variant," &
     "subtitle,suppress_all,suppress_debug_info,suppress_initialization," &
     "task_info,task_name,task_storage,test_case,thread_local_storage," &
     "time_slice,title,type_invariant,type_invariant_class," &
     "unimplemented_unit,universal_aliasing,unmodified,unreferenced," &
     "unreferenced_objects,unreserve_all_interrupts,unused," &
     "volatile_full_access,volatile_function,weak_external";
   --  List of GNAT implementation defined pragmas

   overriding procedure Process_Rule_Parameter
     (Rule       : in out Forbidden_Rule;
      Param      : String;
      Enable     : Boolean;
      Defined_At : String)
   is
      Lower_Param : constant String := To_Lower (Param);
   begin
      if Param = "" then
         if Enable then
            Rule.Rule_State := Enabled;
         else
            Rule.Rule_State := Disabled;
         end if;
      elsif Enable then
         if Lower_Param = "all" then
            Rule.All_Flag := On;
         else
            if Length (Rule.Forbidden) /= 0 then
               Append (Rule.Forbidden, ",");
            end if;

            if Lower_Param = "gnat" then
               if Rule.Name.all = "forbidden_attributes" then
                  Append (Rule.Forbidden, GNAT_Attributes);
               elsif Rule.Name.all = "forbidden_pragmas" then
                  Append (Rule.Forbidden, GNAT_Pragmas);
               else
                  Append (Rule.Forbidden, To_Wide_Wide_String (Lower_Param));
               end if;
            else
               Append (Rule.Forbidden, To_Wide_Wide_String (Lower_Param));
            end if;
         end if;

         Rule.Rule_State := Enabled;
         Rule.Defined_At := new String'(Defined_At);

      else
         if Lower_Param = "all" then
            Rule.All_Flag   := Off;
            Rule.Rule_State := Disabled;
            Rule.Forbidden  := Null_Unbounded_Wide_Wide_String;
            Rule.Allowed    := Null_Unbounded_Wide_Wide_String;

         else
            if Length (Rule.Allowed) /= 0 then
               Append (Rule.Allowed, ",");
            end if;

            if Lower_Param = "gnat" then
               if Rule.Name.all = "forbidden_attributes" then
                  Append (Rule.Allowed, GNAT_Attributes);
               elsif Rule.Name.all = "forbidden_pragmas" then
                  Append (Rule.Allowed, GNAT_Pragmas);
               else
                  Append (Rule.Allowed, To_Wide_Wide_String (Lower_Param));
               end if;
            else
               Append (Rule.Allowed, To_Wide_Wide_String (Lower_Param));
            end if;
         end if;

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
      Append_Param (Args,
                    Rule.Parameters.Child (2).
                    As_Parameter_Decl.F_Param_Identifier.Text,
                    Rule.Param);
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
      Args : in out Rule_Argument_Vectors.Vector) is
   begin
      Append_Array_Param
        (Args,
         Rule.Parameters.Child (2).As_Parameter_Decl.F_Param_Identifier.Text,
         Rule.Param);
   end Map_Parameters;

   overriding procedure Map_Parameters
     (Rule : Identifier_Suffixes_Rule;
      Args : in out Rule_Argument_Vectors.Vector) is
   begin
      Append_Param (Args, "type_suffix", Rule.Type_Suffix);
      Append_Param (Args, "access_suffix", Rule.Access_Suffix);
      Append_Param (Args, "access_access_suffix", Rule.Access_Access_Suffix);
      Append_Param (Args, "class_access_suffix", Rule.Class_Access_Suffix);
      Append_Param (Args, "class_subtype_suffix", Rule.Class_Subtype_Suffix);
      Append_Param (Args, "constant_suffix", Rule.Constant_Suffix);
      Append_Param (Args, "renaming_suffix", Rule.Renaming_Suffix);
      Append_Param (Args, "access_obj_suffix", Rule.Access_Obj_Suffix);
      Append_Param (Args, "interrupt_suffix", Rule.Interrupt_Suffix);
   end Map_Parameters;

   overriding procedure Map_Parameters
     (Rule : Identifier_Prefixes_Rule;
      Args : in out Rule_Argument_Vectors.Vector) is
   begin
      Append_Param (Args, "type", Rule.Type_Prefix);
      Append_Param (Args, "concurrent", Rule.Concurrent_Prefix);
      Append_Param (Args, "access", Rule.Access_Prefix);
      Append_Param (Args, "class_access", Rule.Class_Access_Prefix);
      Append_Param (Args, "subprogram_access", Rule.Subprogram_Access_Prefix);
      Append_Param (Args, "constant", Rule.Constant_Prefix);
      Append_Param (Args, "exception", Rule.Exception_Prefix);
      Append_Param (Args, "enum", Rule.Enum_Prefix);
      Append_Array_Param (Args, "derived", Rule.Derived_Prefix);

      if Rule.Exclusive = Off then
         Args.Append
           (Rule_Argument'
             (Name  => To_Unbounded_Text ("exclusive"),
              Value => To_Unbounded_Text ("false")));
      end if;
   end Map_Parameters;

   overriding procedure Map_Parameters
     (Rule : Identifier_Casing_Rule;
      Args : in out Rule_Argument_Vectors.Vector) is
   begin
      Append_Param (Args, "type", Rule.Type_Casing);
      Append_Param (Args, "enum", Rule.Enum_Casing);
      Append_Param (Args, "constant", Rule.Constant_Casing);
      Append_Param (Args, "exception", Rule.Exception_Casing);
      Append_Param (Args, "others", Rule.Others_Casing);
      Append_Array_Param (Args, "exclude", Rule.Exclude);
   end Map_Parameters;

   overriding procedure Map_Parameters
     (Rule : Forbidden_Rule;
      Args : in out Rule_Argument_Vectors.Vector) is
   begin
      if Rule.All_Flag = On then
         Args.Append
           (Rule_Argument'
             (Name  => To_Unbounded_Text ("all"),
              Value => To_Unbounded_Text ("true")));
      end if;

      Append_Array_Param (Args, "forbidden", Rule.Forbidden);
      Append_Array_Param (Args, "allowed", Rule.Allowed);
   end Map_Parameters;

   ---------------
   -- Rule_Name --
   ---------------

   function Rule_Name (Rule : Rule_Template) return String is
   begin
      return Rule.Name.all;
   end Rule_Name;

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

   overriding function Rule_Parameter
     (Rule : Forbidden_Rule;
      Diag : String) return String
   is
      pragma Unreferenced (Rule);
      First_Idx : constant Natural := Index (Diag, " ", Going => Backward) + 1;
   begin
      return To_Lower (Diag (First_Idx .. Diag'Last));
   end Rule_Parameter;

   overriding function Rule_Parameter
     (Rule : Identifier_Casing_Rule;
      Diag : String) return String
   is
      pragma Unreferenced (Rule);
      First_Idx : Natural := Index (Diag, " for ");
   begin
      if First_Idx > 0 then
         First_Idx := First_Idx + 5;

         case Diag (First_Idx) is
            when 'c' =>
               return "constant";
            when 'e' =>
               if Diag (First_Idx + 1) = 'n' then
                  return "enum";
               else
                  return "exception";
               end if;

            when 's' =>
               return "type";
            when others =>
               raise Constraint_Error with
                 "identifier_Casing: bug in exemption parameter processing";
         end case;
      end if;

      First_Idx := Index (Diag, " in ");

      if First_Idx > 0 then
         return "exclude";
      else
         return "others";
      end if;
   end Rule_Parameter;

   overriding function Rule_Parameter
     (Rule : Identifier_Suffixes_Rule;
      Diag : String) return String
   is
      pragma Unreferenced (Rule);
   begin
      if Index (Diag, "access-to-class") /= 0 then
         return "class_access";
      elsif Index (Diag, "access object") /= 0 then
         return "access_obj";
      elsif Index (Diag, "access") /= 0 then
         return "access";
      elsif Index (Diag, "class-wide") /= 0 then
         return "class_subtype";
      elsif Index (Diag, "constant") /= 0 then
         return "constant";
      elsif Index (Diag, "type") /= 0 then
         return "type";
      elsif Index (Diag, "renaming") /= 0 then
         return "renaming";
      elsif Index (Diag, "interrupt") /= 0 then
         return "interrupt";
      else
         return "";
      end if;
   end Rule_Parameter;

   overriding function Rule_Parameter
     (Rule : Identifier_Prefixes_Rule;
      Diag : String) return String
   is
      pragma Unreferenced (Rule);
   begin
      if Index (Diag, "task") /= 0
        or else
         Index (Diag, "protected") /= 0
      then
         return "concurrent";
      elsif Index (Diag, "access-to-class") /= 0 then
         return "class_access";
      elsif Index (Diag, "access-to-subprogram") /= 0 then
         return "subprogram_access";
      elsif Index (Diag, "derived") /= 0 then
         return "derived";
      elsif Index (Diag, "constant") /= 0 then
         return "constant";
      elsif Index (Diag, "enumeration") /= 0 then
         return "enum";
      elsif Index (Diag, "exception") /= 0 then
         return "exception";
      elsif Index (Diag, "access") /= 0 then
         return "access";
      elsif Index (Diag, "subtype") /= 0 then
         return "type";
      elsif Index (Diag, "exclusive") /= 0 then
         return "exclusive";
      else
         return "";
      end if;
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
           ("<parameter>" & To_String (Rule.Param) & "</parameter>",
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

   overriding procedure XML_Print_Rule
     (Rule         : Identifier_Prefixes_Rule;
      Indent_Level : Natural := 0)
   is
      Prefix_Specified : Boolean := False;

      procedure Print (Param : String; Prefix : Unbounded_Wide_Wide_String);
      --  Print in XML format the value Prefix for parameter Param if not empty

      -----------
      -- Print --
      -----------

      procedure Print (Param : String; Prefix : Unbounded_Wide_Wide_String) is
      begin
         if Length (Prefix) /= 0 then
            XML_Report
              ("<parameter>" & Param & "=" & To_String (Prefix) &
               "</parameter>",
               Indent_Level + 1);
            Prefix_Specified := True;
         end if;
      end Print;

   begin
      XML_Report
        ("<rule id=""" & Rule_Name (Rule) & """>",
         Indent_Level);

      Print ("Type", Rule.Type_Prefix);
      Print ("Concurrent", Rule.Concurrent_Prefix);
      Print ("Access", Rule.Access_Prefix);
      Print ("Class_Access", Rule.Class_Access_Prefix);
      Print ("Subprogram_Access", Rule.Subprogram_Access_Prefix);
      Print ("Constant", Rule.Constant_Prefix);
      Print ("Exception", Rule.Exception_Prefix);
      Print ("Enum", Rule.Enum_Prefix);

      if Length (Rule.Derived_Prefix) /= 0 then
         XML_Report
           ("<parameter>Derived=" & To_String (Rule.Derived_Prefix) &
            "</parameter>",
            Indent_Level + 1);
         Prefix_Specified := True;
      end if;

      --  We have to print out Exclusive parameter, but this would make sense
      --  only if at least one prefix is specified

      if Prefix_Specified and then Rule.Exclusive /= Off then
         XML_Report ("<parameter>Exclusive</parameter>", Indent_Level + 1);
      end if;

      XML_Report ("</rule>", Indent_Level);
   end XML_Print_Rule;

   overriding procedure XML_Print_Rule
     (Rule         : Identifier_Suffixes_Rule;
      Indent_Level : Natural := 0)
   is
      procedure Print (Param : String; Suffix : Unbounded_Wide_Wide_String);
      --  Print in XML format the value Prefix for parameter Param if not empty

      -----------
      -- Print --
      -----------

      procedure Print (Param : String; Suffix : Unbounded_Wide_Wide_String) is
      begin
         if Length (Suffix) /= 0 then
            XML_Report
              ("<parameter>" & Param & "=" & To_String (Suffix) &
               "</parameter>",
               Indent_Level + 1);
         end if;
      end Print;

   begin
      XML_Report
        ("<rule id=""" & Rule_Name (Rule) & """>",
         Indent_Level);

      Print ("Type_Suffix", Rule.Type_Suffix);

      if Length (Rule.Access_Access_Suffix) /= 0 then
         XML_Report_No_EOL
           ("<parameter>Access_Suffix=" & To_String (Rule.Access_Suffix),
            Indent_Level + 1);
         XML_Report_No_EOL ("(" & To_String (Rule.Access_Access_Suffix) & ")");
      end if;

      Print ("Class_Subtype_Suffix", Rule.Class_Subtype_Suffix);
      Print ("Class_Access_Suffix", Rule.Class_Access_Suffix);
      Print ("Constant_Suffix", Rule.Constant_Suffix);
      Print ("Renaming_Suffix", Rule.Renaming_Suffix);
      Print ("Access_Obj_Suffix", Rule.Access_Obj_Suffix);
      Print ("Interrupt_Suffix", Rule.Interrupt_Suffix);
      XML_Report ("</rule>", Indent_Level);
   end XML_Print_Rule;

   overriding procedure XML_Print_Rule
     (Rule         : Identifier_Casing_Rule;
      Indent_Level : Natural := 0)
   is
      procedure Print (Param : String; Casing : Unbounded_Wide_Wide_String);
      --  Print in XML format the value Casing for parameter Param if not empty

      -----------
      -- Print --
      -----------

      procedure Print (Param : String; Casing : Unbounded_Wide_Wide_String) is
      begin
         if Length (Casing) /= 0 then
            XML_Report
              ("<parameter>" & Param & "=" & To_String (Casing) &
               "</parameter>",
               Indent_Level + 1);
         end if;
      end Print;

      C : Character;

   begin
      XML_Report ("<rule id=""" & Rule_Name (Rule) & """>", Indent_Level);

      Print ("Type", Rule.Type_Casing);
      Print ("Enum", Rule.Enum_Casing);
      Print ("Constant", Rule.Constant_Casing);
      Print ("Exception", Rule.Exception_Casing);
      Print ("Others", Rule.Others_Casing);

      if Length (Rule.Exclude) /= 0 then
         XML_Report_No_EOL ("<parameter>Exclude=", Indent_Level + 1);

         for J in 1 .. Length (Rule.Exclude) loop
            C := To_Character (Element (Rule.Exclude, J));

            if C /= ',' then
               XML_Report_No_EOL ([C]);
            else
               XML_Report ("</parameter>", Indent_Level + 1);
               XML_Report_No_EOL ("<parameter>Exclude=", Indent_Level + 1);
            end if;
         end loop;
      end if;

      XML_Report ("</rule>", Indent_Level);
   end XML_Print_Rule;

   overriding procedure XML_Print_Rule
     (Rule         : Forbidden_Rule;
      Indent_Level : Natural := 0)
   is
      procedure XML_Print
        (Items  : Unbounded_Wide_Wide_String;
         Enable : Boolean);
      --  Print Items if not empty. If Enable is True, these items are printed
      --  as is, otherwise they are marked as disabled (prefixed with a '-').

      ---------------
      -- XML_Print --
      ---------------

      procedure XML_Print
        (Items  : Unbounded_Wide_Wide_String;
         Enable : Boolean)
      is
         C : Character;
      begin
         if Length (Items) = 0 then
            return;
         end if;

         if Enable then
            XML_Report_No_EOL ("<parameter>", Indent_Level + 1);
         else
            XML_Report_No_EOL ("<parameter>-", Indent_Level + 1);
         end if;

         for J in 1 .. Length (Items) loop
            C := To_Character (Element (Items, J));

            if C = ',' then
               XML_Report ("</parameter>", Indent_Level + 1);

               if Enable then
                  XML_Report_No_EOL ("<parameter>", Indent_Level + 1);
               else
                  XML_Report_No_EOL ("<parameter>-", Indent_Level + 1);
               end if;
            else
               XML_Report_No_EOL ([C]);
            end if;
         end loop;
      end XML_Print;

   begin
      XML_Report
        ("<rule id=""" & Rule_Name (Rule) & """>",
         Indent_Level);

      if Rule.All_Flag = On then
         XML_Report ("<parameter>ALL</parameter>", Indent_Level + 1);
      else
         XML_Print (Rule.Forbidden, True);
      end if;

      XML_Print (Rule.Allowed, False);
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

   overriding procedure XML_Rule_Help
     (Rule  : Identifier_Prefixes_Rule;
      Level : Natural)
   is
      procedure Print (Param : String; Help : String);
      --  Print XML help for parameter Param

      -----------
      -- Print --
      -----------

      procedure Print (Param : String; Help : String) is
      begin
         Info (Level * Indent_String              &
               "<field switch=""+R"               &
               Rule.Name.all                      &
               ":" & Param & '"'                  &
               " label="""                        &
               "prefix for " & Help               &
               " (empty string disables check)""" &
               " separator=""="""                 &
               " switch-off=""-R"                 &
               Rule.Name.all                      &
               ":" & Param & '"'                  &
               "/>");
      end Print;

   begin
      Print ("Type", "type names");
      Print ("Concurrent", "task and protected type names");
      Print ("Access", "access type names");
      Print ("Class_Access", "class access type names");
      Print ("Subprogram_Access", "access-to-subprogram type names");
      Print ("Derived", "derived type names");
      Print ("Constant", "constant names");
      Print ("Exception", "exception names");
      Print ("Enum", "enumeration literals");
      Info (Level * Indent_String &
            "<check switch=""+R"  &
            Rule.Name.all         &
            ":Exclusive"""        &
            " label=""strong check mode""/>");
   end XML_Rule_Help;

   overriding procedure XML_Rule_Help
     (Rule  : Identifier_Suffixes_Rule;
      Level : Natural)
   is
      procedure Print (Param : String; Help : String);
      --  Print XML help for parameter Param

      -----------
      -- Print --
      -----------

      procedure Print (Param : String; Help : String) is
      begin
         Info (Level * Indent_String              &
               "<field switch=""+R"               &
               Rule.Name.all                      &
               ":" & Param & '"'                  &
               " label="""                        &
               "suffix for " & Help & " names"    &
               " (empty string disables check)""" &
               " separator=""="""                 &
               " switch-off=""-R"                 &
               Rule.Name.all                      &
               ":" & Param & '"'                  &
               "/>");
      end Print;

   begin
      Info (Level * Indent_String                 &
            "<check switch=""+R"                  &
            Rule.Name.all                         &
            ":Default"""                          &
            " label="""                           &
            "identifiers use standard suffixes""" &
            "/>");

      Print ("Type_Suffix", "type");
      Print ("Access_Suffix", "access type");
      Print ("Constant_Suffix", "constant");
      Print ("Renaming_Suffix", "package renaming");
      Print ("Access_Obj_Suffix", "access object");
      Print ("Interrupt_Suffix", "interrupt handler");

      --  Specifying the dependencies between the default suffixes and the
      --  content of the fields for specific suffixes

      Info (Level * Indent_String                         &
           "<default-value-dependency master-switch=""+R" &
            Rule.Name.all                                 &
            ":Default"""                                  &
            " slave-switch=""+R"                          &
            Rule.Name.all                                 &
            ":Type_Suffix=_T""/>");
      Info (Level * Indent_String                         &
           "<default-value-dependency master-switch=""+R" &
            Rule.Name.all                                 &
            ":Default"""                                  &
            " slave-switch=""+R"                          &
            Rule.Name.all                                 &
            ":Access_Suffix=_A""/>");
      Info (Level * Indent_String                         &
           "<default-value-dependency master-switch=""+R" &
            Rule.Name.all                                 &
            ":Default"""                                  &
            " slave-switch=""+R"                          &
            Rule.Name.all                                 &
            ":Constant_Suffix=_C""/>");
      Info (Level * Indent_String                         &
           "<default-value-dependency master-switch=""+R" &
            Rule.Name.all                                 &
            ":Default"""                                  &
            " slave-switch=""+R"                          &
            Rule.Name.all                                 &
            ":Renaming_Suffix=_R""/>");
   end XML_Rule_Help;

   overriding procedure XML_Rule_Help
     (Rule  : Identifier_Casing_Rule;
      Level : Natural)
   is
      procedure Print_Combo (Par : String; Descr : String);
      --  Print one combo definition

      -----------------
      -- Print_Combo --
      -----------------

      procedure Print_Combo (Par : String; Descr : String) is
      begin
         Info (Level * Indent_String & "<combo switch=""+RIdentifier_Casing:" &
               Par & '"'                                                      &
               " label=""" & Descr & " casing"" separator=""="">");
         Info ((Level + 1) * Indent_String &
              "<combo-entry value=""upper"" />");
         Info ((Level + 1) * Indent_String &
              "<combo-entry value=""lower"" />");
         Info ((Level + 1) * Indent_String &
              "<combo-entry value=""mixed"" />");
         Info (Level * Indent_String & "</combo>");
      end Print_Combo;

   begin
      Print_Combo ("Type", "type name");
      Print_Combo ("Enum", "enumeration literal");
      Print_Combo ("Constant", "constant name");
      Print_Combo ("Exception", "exception name");
      Print_Combo ("Others", "other name");

      Info (Level * Indent_String               &
            "<field switch=""+R"                &
            Rule.Name.all                       &
            ":Exclude"""                        &
            " label="""                         &
            "dictionary of casing exceptions""" &
            " separator=""="""                  &
            "/>");
   end XML_Rule_Help;

   procedure XML_Rule_Help
     (Rule  : Forbidden_Rule;
      Level : Natural)
   is
      Str : constant String :=
        (if Rule.Name.all = "forbidden_pragmas"
         then "pragmas" else "attributes");

   begin
      Info (Level * Indent_String &
            "<check switch=""+R" &
            Rule.Name.all         &
            ":ALL"""              &
            " label="""           &
            "detect all " & Str   &
            " except explicitly disabled""/>");

      Info (Level * Indent_String &
            "<check switch=""+R" &
            Rule.Name.all         &
            ":GNAT"""             &
            " label="""           &
            "detect all GNAT " & Str & " except explicitly disabled""/>");

      Info (Level * Indent_String &
            "<field switch=""+R"  &
            Rule.Name.all         &
            """ label="""         &
            "detect specified "   &
            Str & " (use ',' as separator)""" &
            " separator="":""/>");

      Info (Level * Indent_String &
            "<field switch=""-R"  &
            Rule.Name.all         &
            """ label="""         &
            "do not detect specified " &
            Str & " (use ',' as separator)""" &
            " separator="":""/>");
   end XML_Rule_Help;

   -----------------------
   -- XML_Rule_Help_Tip --
   -----------------------

   procedure XML_Rule_Help_Tip (Rule : Rule_Template; Level : Natural) is
   begin
      null;
   end XML_Rule_Help_Tip;

end Gnatcheck.Rules;

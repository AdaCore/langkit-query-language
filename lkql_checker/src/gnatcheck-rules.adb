--
--  Copyright (C) 2005-2024, AdaCore
--  SPDX-License-Identifier: GPL-3.0-or-later
--

with Ada.Characters.Conversions; use Ada.Characters.Conversions;
with Ada.Strings;                use Ada.Strings;
with Ada.Strings.Fixed;          use Ada.Strings.Fixed;
with Ada.Strings.Maps;
with GNAT.Directory_Operations;  use GNAT.Directory_Operations;
with GNAT.String_Split;          use GNAT.String_Split;
with GNAT.OS_Lib;

with Gnatcheck.JSON_Utilities;   use Gnatcheck.JSON_Utilities;
with Gnatcheck.Options;          use Gnatcheck.Options;
with Gnatcheck.Output;           use Gnatcheck.Output;
with Gnatcheck.Rules.Rule_Table; use Gnatcheck.Rules.Rule_Table;
with Gnatcheck.String_Utilities; use Gnatcheck.String_Utilities;

with Langkit_Support.Text;       use Langkit_Support.Text;

package body Gnatcheck.Rules is

   procedure Append_Int_Param
     (Args  : in out Rule_Argument_Vectors.Vector;
      Name : Text_Type;
      Value :        Integer);
   --  Add the integer actual parameter to the given argument vector

   procedure Append_Bool_Param
     (Args  : in out Rule_Argument_Vectors.Vector;
      Name : Text_Type;
      Value : Tri_State);
   --  Add the boolean actual parameter to the given argument vector

   procedure Append_String_Param
     (Args  : in out Rule_Argument_Vectors.Vector;
      Name  : Wide_Wide_String;
      Value : Unbounded_Wide_Wide_String);
   --  Append to Args a parameter named Name with value Value if not empty,
   --  otherwise do nothing.

   procedure Append_Array_Param
     (Args  : in out Rule_Argument_Vectors.Vector;
      Name  : Wide_Wide_String;
      Value : Unbounded_Wide_Wide_String);
   --  Like Append_String_Param, for an array of strings represented by a comma
   --  separated list in Value.

   procedure Process_String_Arg
     (Params_Object : JSON_Value;
      Param_Name : String;
      Rule_Field : in out Unbounded_Wide_Wide_String;
      Normalize : Boolean := False);
   --  If the given `Param_Name` is present as a field in the `Params_Object`,
   --  then parse it as a string literal and set `Rule_Field` to the result
   --  value.
   --  If `Params_Object` contains `Param_Name` then unset the field.
   --  If `Narmalize` is true, then remove spaces and lower the extracted
   --  string.

   procedure Handle_Array_Param
     (Args        : in out Rule_Argument_Vectors.Vector;
      Rule        : in out One_Array_Parameter_Rule;
      Param_Value : Unbounded_Wide_Wide_String);
   --  Common procedure to handle array parameter rules and aliases by adding
   --  their parameter actual value in the provided rule arguments vector.

   function Expand_Env_Variables (Name : String) return String;
   --  Assuming that Name is a name of a dictionary file (used as rule
   --  parameter) and that it may contain environment variables, tries
   --  to locate environment variables and to replace them with their values.
   --  Any substring that starts with '$', and is either limited by
   --  a Directory_Separator or by another '$' character (which is considered
   --  as the beginning of another environment variable) or by the end of Name
   --  is treated as the name of an environment variable. If the corresponding
   --  environment variable does not exist or does not have a value, generates
   --  the diagnostic message and does not do any replacement.

   function Find_File (Name : String) return String;
   --  Return the pathname corresponding to Name, relative to either the
   --  current directory or the rule file if any. Return "" if no file found.

   procedure Load_Dictionary
     (File_Name : String;
      Rule_Name : String;
      State     : in out Rule_States;
      Param     : in out Unbounded_Wide_Wide_String);
   --  Load dictionary file File_Name for rule Rule and append the result in
   --  Param as a comma separated list.

   function To_String (S : Unbounded_Wide_Wide_String) return String
   is (To_String (To_Wide_Wide_String (S)));
   --  Convert an Unbounded_Wide_Wide_String to a String

   function From_Boolean (B : Boolean) return Tri_State is
     (if B then On else Off);
   --  Get the `Tri_State` value corresponding to the given boolean

   function Param_Name
     (Rule : Rule_Template'Class; Index : Positive) return Text_Type is
     (Rule.Parameters.Child (Index).As_Parameter_Decl.F_Param_Identifier
        .Text);
   --  Get the name of the rule parameter at the given index

   ----------------------
   -- Append_Int_Param --
   ----------------------

   procedure Append_Int_Param
     (Args  : in out Rule_Argument_Vectors.Vector;
      Name  : Text_Type;
      Value : Integer) is
   begin
      if Value /= Integer'First then
         Args.Append
           (Rule_Argument'
              (Name  => To_Unbounded_Text (Name),
               Value => To_Unbounded_Text (Value'Wide_Wide_Image)));
      end if;
   end Append_Int_Param;

   -----------------------
   -- Append_Bool_Param --
   -----------------------

   procedure Append_Bool_Param
     (Args  : in out Rule_Argument_Vectors.Vector;
      Name  : Text_Type;
      Value : Tri_State)
   is
   begin
      if Value /= Unset then
         Args.Append
           (Rule_Argument'
              (Name  => To_Unbounded_Text (Name),
               Value =>
                 To_Unbounded_Text
                   (if Value = On then "true" else "false")));
      end if;
   end Append_Bool_Param;

   -------------------------
   -- Append_String_Param --
   -------------------------

   procedure Append_String_Param
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
   end Append_String_Param;

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

   ------------------------
   -- Handle_Array_Param --
   ------------------------

   procedure Handle_Array_Param
     (Args        : in out Rule_Argument_Vectors.Vector;
      Rule        : in out One_Array_Parameter_Rule;
      Param_Value : Unbounded_Wide_Wide_String)
   is
      Last : constant Natural := Length (Param_Value);

      procedure Error;
      --  Emit an error message when an invalid parameter is detected.

      function Find_Char
        (Str      : Wide_Wide_String;
         C        : Wide_Wide_Character;
         Backward : Boolean := False) return Natural;
      --  Return the first occurrence of C in Str, 0 if none
      --  If Backward is True, search from the end of the string.

      -----------
      -- Error --
      -----------

      procedure Error is
      begin
         Error ("(" & To_String (Rule.Name) & ") wrong parameter: " &
                To_String (Param_Value));
         Rule.Rule_State   := Disabled;
         Bad_Rule_Detected := True;
      end Error;

      -----------
      -- Index --
      -----------

      function Find_Char
        (Str      : Wide_Wide_String;
         C        : Wide_Wide_Character;
         Backward : Boolean := False) return Natural is
      begin
         if Backward then
            for J in reverse Str'Range loop
               if Str (J) = C then
                  return J;
               end if;
            end loop;
         else
            for J in Str'Range loop
               if Str (J) = C then
                  return J;
               end if;
            end loop;
         end if;

         return 0;
      end Find_Char;

   begin

      --  Check whether we have 5 arguments for parameters_out_of_order
      if Rule.Name = "parameters_out_of_order"
        and then Last /= 0
        and then Slice_Count (Create (To_String (Param_Value), ",")) /= 5
      then
         Error
           ("(" & To_String (Rule.Name) &
            ") requires 5 parameters, got: " &
            To_String (Param_Value));
         Bad_Rule_Detected := True;
         Rule.Rule_State := Disabled;
         return;
      end if;

      if Rule.Name = "actual_parameters" and then Last /= 0 then
         declare
            Param        : Unbounded_Wide_Wide_String;
            C            : Wide_Wide_Character;
            Num_Elements : Positive := 1;
            Lower        : Boolean  := True;

         begin
            Append (Param, "[(""");

            for J in 1 .. Last loop
               C := Element (Param_Value, J);

               case C is
                  when '"'    =>
                     if Num_Elements = 3 then
                        if Element (Param_Value, J - 1) = ':' then
                           Append (Param, '|');
                           Lower := False;

                        elsif J /= Last
                          and then Element (Param_Value, J + 1) /= ','
                        then
                           Error;
                           return;
                        end if;
                     else
                        Append (Param, "\""");
                     end if;

                  when ':'    =>
                     Append (Param, """,""");
                     Num_Elements := @ + 1;
                     Lower := True;

                  when ','    =>
                     Append (Param, """),(""");
                     Num_Elements := 1;
                     Lower := True;

                  when others =>
                     Append (Param, (if Lower then To_Lower (C) else C));
               end case;
            end loop;

            Append (Param, """)]");
            Args.Append
              (Rule_Argument'
                (Name  => To_Unbounded_Text (Param_Name (Rule, 2)),
                 Value => To_Unbounded_Text (To_Wide_Wide_String (Param))));
         end;
      elsif Last /= 0
        and then Rule.Name = "exception_propagation_from_callbacks"
      then
         declare
            Param      : Unbounded_Wide_Wide_String;
            Str        : constant Wide_Wide_String :=
              To_Wide_Wide_String (Param_Value);
            Current    : Natural := Str'First;
            Next_Comma : Natural;
            Dot        : Natural;

         begin
            Append (Param, "[(""");

            loop
               Next_Comma := Find_Char (Str (Current .. Str'Last), ',');

               if Next_Comma = 0 then
                  Next_Comma := Str'Last + 1;
               end if;

               Dot :=
                 Find_Char
                   (Str (Current .. Next_Comma - 1), '.', Backward => True);

               if Dot = 0 then
                  Error;
                  return;
               end if;

               Append (Param, To_Lower (Str (Current .. Dot - 1)));
               Append (Param, """,""");
               Append (Param, To_Lower (Str (Dot + 1 .. Next_Comma - 1)));
               Current := Next_Comma + 1;

               exit when Current > Str'Last;

               Append (Param, """),(""");
            end loop;

            Append (Param, """)]");
            Args.Append
              (Rule_Argument'
                (Name  => To_Unbounded_Text (Param_Name (Rule, 2)),
                 Value => To_Unbounded_Text (To_Wide_Wide_String (Param))));
         end;
      else
         Append_Array_Param (Args, Param_Name (Rule, 2), Param_Value);
      end if;
   end Handle_Array_Param;

   ------------------------
   -- Process_String_Arg --
   ------------------------

   procedure Process_String_Arg
     (Params_Object : JSON_Value;
      Param_Name : String;
      Rule_Field : in out Unbounded_Wide_Wide_String;
      Normalize : Boolean := False) is
   begin
      if Params_Object.Has_Field (Param_Name) then
         declare
            Field_Val : constant String :=
              (if Normalize
               then Remove_Spaces
                 (To_Lower (Expect_Literal (Params_Object, Param_Name)))
               else Expect_Literal (Params_Object, Param_Name));
         begin
            Set_Unbounded_Wide_Wide_String
              (Rule_Field, To_Wide_Wide_String (Field_Val));
            Params_Object.Unset_Field (Param_Name);
         end;
      end if;
   end Process_String_Arg;

   -------------------
   -- Annotate_Rule --
   -------------------

   function Annotate_Rule
     (Rule : Rule_Template;
      Alias : String) return String is
   begin
      if not Mapping_Mode then
         return "";
      else
         return " [" &
                (if Alias /= "" then Alias & "|" else "") &
                Rule_Name (Rule) & "]";
      end if;
   end Annotate_Rule;

   ---------------------------
   -- Create_Alias_For_Rule --
   ---------------------------

   function Create_Alias_For_Rule (Id : Rule_Id) return Alias_Access is
      Rule   : constant Rule_Access := All_Rules (Id);
      Result : Alias_Access;
   begin
      Result      := Create_Alias (Rule.all);
      Result.Rule := Id;
      return Result;
   end Create_Alias_For_Rule;

   --------------
   -- Get_Rule --
   --------------

   function Get_Rule
     (Alias : Alias_Template'Class) return Rule_Template'Class is
   begin
      return All_Rules (Alias.Rule).all;
   end Get_Rule;

   --------------------------
   -- Expand_Env_Variables --
   --------------------------

   function Expand_Env_Variables (Name : String) return String is
      Text_Start : Natural := Name'First;
      EV_Start   : Natural := Index (Name, "$");
      EV_End     : Positive;

      Next_Env_Start : Natural;
      Next_Dir_Sep   : Natural;

      function Expand_Env_Var (EV_Name : String) return String;
      --  Assuming that EV_Name is the name of an environment variable, returns
      --  its value. If there is no such variable or if the variable is not
      --  defined, returns the argument

      --------------------
      -- Expand_Env_Var --
      --------------------

      function Expand_Env_Var (EV_Name : String) return String is
         use GNAT.OS_Lib;
         Val : GNAT.OS_Lib.String_Access := Getenv (EV_Name);
      begin
         if Val = null or else Val.all = "" then
            Error ("environment variable " & EV_Name & " undefined");
            Free (Val);
            return EV_Name;
         else
            return Result : constant String := Val.all do
               Free (Val);
            end return;
         end if;
      end Expand_Env_Var;

      use Ada.Strings.Maps;

      Result : Unbounded_String;

   begin
      if EV_Start = 0 then
         return Name;
      end if;

      while EV_Start /= 0 loop
         if Text_Start < EV_Start then
            Append (Result, Name (Text_Start .. EV_Start - 1));
         end if;

         EV_End := Name'Last;
         Next_Env_Start := Index (Name (EV_Start + 2 .. Name'Last), "$");

         if Next_Env_Start /= 0 then
            EV_End := Next_Env_Start - 1;
         end if;

         Next_Dir_Sep :=
           Index (Name (EV_Start + 2 .. EV_End),
                  Set => To_Set (Character_Ranges'
                           (('/', '/'),
                            (GNAT.OS_Lib.Directory_Separator,
                             GNAT.OS_Lib.Directory_Separator))));

         if Next_Dir_Sep /= 0 then
            EV_End := Next_Dir_Sep - 1;
         end if;

         Append (Result, Expand_Env_Var (Name (EV_Start ..  EV_End)));

         EV_Start   := Next_Env_Start;
         Text_Start := EV_End + 1;
      end loop;

      return To_String (Result);
   end Expand_Env_Variables;

   ---------------
   -- Find_File --
   ---------------

   function Find_File (Name : String) return String is
      Rule_File_Dir : constant String :=
        Dir_Name
          (Gnatcheck.Rules.Rule_Table.Processed_Rule_File_Name);

   begin
      if GNAT.OS_Lib.Is_Regular_File (Rule_File_Dir & Name) then
         return Rule_File_Dir & Name;
      elsif GNAT.OS_Lib.Is_Regular_File (Name) then
         return Name;
      else
         return "";
      end if;
   end Find_File;

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
      Rule.Source_Mode       := General;
      Rule.Remediation_Level := Medium;
   end Init_Rule;

   ----------------
   -- Is_Enabled --
   ----------------

   function Is_Enabled (Rule : Rule_Template) return Boolean is
   begin
      return Rule.Rule_State = Enabled;
   end Is_Enabled;

   function Is_Enabled (Alias : Alias_Template'Class) return Boolean is
   begin
      return Alias.Alias_State = Enabled;
   end Is_Enabled;

   function Is_Enabled (Alias : Alias_Access) return Boolean is
   begin
      return Alias /= null and then Alias.Alias_State = Enabled;
   end Is_Enabled;

   ---------------------
   -- Load_Dictionary --
   ---------------------

   procedure Load_Dictionary
     (File_Name : String;
      Rule_Name : String;
      State     : in out Rule_States;
      Param     : in out Unbounded_Wide_Wide_String)
   is
      Name        : constant String := Find_File (File_Name);
      File        : File_Type;
      Line        : String (1 .. 1024);
      Len         : Natural;
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
         State := Enabled;

      else
         Error ("(" & Rule_Name & "): cannot load file " & File_Name);
         State             := Disabled;
         Bad_Rule_Detected := True;
      end if;

   exception
      when others =>
         Error ("(" & Rule_Name & "): cannot load file " & File_Name);
         State             := Disabled;
         Bad_Rule_Detected := True;
   end Load_Dictionary;

   -----------------------------
   -- Print_Rule_To_LKQL_File --
   -----------------------------

   procedure Print_Rule_To_LKQL_File
     (Rule      : in out Rule_Template'Class;
      Rule_File : File_Type)
   is
      Args  : Rule_Argument_Vectors.Vector;
      First : Boolean := True;

      procedure Print_Args_To_LKQL_File
        (Args  : Rule_Argument_Vectors.Vector;
         First : Boolean := True);
      --  Procedure to print an arguments vector to the current `Rule_File`.

      procedure Print_Args_To_LKQL_File
        (Args  : Rule_Argument_Vectors.Vector;
         First : Boolean := True)
      is
         F : Boolean := First;
      begin
         for Param of Args loop
            if F then
               F := False;
            else
               Put (Rule_File, ", ");
            end if;
            Put (Rule_File,
                 To_String (To_Wide_Wide_String (Param.Name)) & ": " &
                   To_String (To_Wide_Wide_String (Param.Value)));
         end loop;
      end Print_Args_To_LKQL_File;
   begin
      Map_Parameters (Rule, Args);
      Put (Rule_File, Rule_Name (Rule));

      if not Args.Is_Empty
        or else not Rule.Aliases.Is_Empty
      then
         Put (Rule_File, ": [");

         --  Print the rule arguments if there are to configure the default
         --  instance.
         if not Args.Is_Empty then
            Put (Rule_File, "{");
            Print_Args_To_LKQL_File (Args);
            Put (Rule_File, "}");
            First := False;
         end if;

         --  Iterate on all aliases to add all other instances
         for Alias of Rule.Aliases loop
            if First then
               First := False;
            else
               Put (Rule_File, ", ");
            end if;
            Put (Rule_File,
                 "{alias_name: """ & To_String (Alias.Name) & """");
            Args.Clear;
            Map_Parameters (Alias.all, Args);
            Print_Args_To_LKQL_File (Args, First => False);
            Put (Rule_File, "}");
         end loop;
         Put (Rule_File, "]");
      end if;
   end Print_Rule_To_LKQL_File;

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
      Indent_Level : Natural := 0) is
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
        [1 .. Length (Rule.Name) + 3 => ' '];

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
        [1 .. Length (Rule.Name) + 3 => ' '];

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
        [1 .. Length (Rule.Name) + 3 => ' '];

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
         Put (Rule_File, ":ALL");
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

   overriding procedure Print_Rule_To_File
     (Rule         : Silent_Exception_Handlers_Rule;
      Rule_File    : File_Type;
      Indent_Level : Natural := 0)
   is
      First_Param : Boolean := True;

      procedure Print
        (Items : Unbounded_Wide_Wide_String; Quote : Boolean := False);
      --  Print Items as parameters if not empty.
      --  If Quote is True, put each parameter within quotes ("").

      -----------
      -- Print --
      -----------

      procedure Print
        (Items : Unbounded_Wide_Wide_String; Quote : Boolean := False) is
      begin
         if Length (Items) = 0 then
            return;
         end if;

         if First_Param then
            Put (Rule_File, ":");
            First_Param := False;
         else
            Put_Line (Rule_File, ",");
            Put (Rule_File, Indent_Level * Indent_String);
         end if;

         if Quote then
            Put (Rule_File, '"');
            for C of To_String (To_Wide_Wide_String (Items)) loop
               if C = ',' then
                  Put (Rule_File, """,""");
               else
                  Put (Rule_File, C);
               end if;
            end loop;
            Put (Rule_File, '"');

         else
            Put (Rule_File, To_String (To_Wide_Wide_String (Items)));
         end if;
      end Print;

   begin
      Print_Rule_To_File (Rule_Template (Rule), Rule_File, Indent_Level);
      Print (Rule.Subprograms);
      Print (Rule.Subprogram_Regexps, Quote => True);
   end Print_Rule_To_File;

   overriding procedure Print_Rule_To_File
     (Rule         : Custom_Rule;
      Rule_File    : File_Type;
      Indent_Level : Natural := 0)
   is
      First_Param : Boolean := True;
   begin
      Print_Rule_To_File (Rule_Template (Rule), Rule_File, Indent_Level);

      for Param of Rule.Arguments loop
         if First_Param then
            Put (Rule_File, ":");
            First_Param := False;
         else
            Put_Line (Rule_File, ",");
            Put (Rule_File, Indent_Level * Indent_String);
         end if;

         Put (Rule_File,
              To_String (To_Wide_Wide_String (Param.Name)) & "=" &
              To_String (To_Wide_Wide_String (Param.Value)));
      end loop;
   end Print_Rule_To_File;

   ---------------------
   -- Print_Rule_Help --
   ---------------------

   procedure Print_Rule_Help (Rule : Rule_Template) is
   begin
      Info
        (Message  =>
           " " & To_String (Rule.Name) & " - " &
           To_String (Rule.Help_Info) & " - " &
           Rule.Remediation_Level'Img,
         Line_Len => 0, Spacing => 0);
   end Print_Rule_Help;

   ----------------------------
   -- Process_Rule_Parameter --
   ----------------------------

   function Defined_Str (Defined_At : String) return String is
     (if Defined_At = "" then "command line" else Defined_At);
   --  Helper function to return Defined_At if not null, or "command line"

   procedure Process_Rule_Parameter
     (Rule       : in out Rule_Template;
      Alias      : Alias_Access;
      Param      : String;
      Enable     : Boolean;
      Defined_At : String)
   is
      pragma Unreferenced (Defined_At);
      State_Value : Rule_States;
   begin
      --  Initialize rule and alias common values
      if Alias = null then
         State_Value := Rule.Rule_State;
      else
         State_Value := Alias.Alias_State;
      end if;

      --  Process the actual parameter
      if Param /= "" then
         Error
           ("no parameter can be set for rule " & To_String (Rule.Name) &
            ", " & Param & " ignored");
         Bad_Rule_Detected := True;
      else
         State_Value := (if Enable then Enabled else Disabled);
      end if;

      --  Update rule or alias values with the computed ones
      if Alias = null then
         Rule.Rule_State := State_Value;
      else
         Alias.Alias_State := State_Value;
      end if;
   end Process_Rule_Parameter;

   overriding procedure Process_Rule_Parameter
     (Rule       : in out One_Integer_Parameter_Rule;
      Alias      : Alias_Access;
      Param      : String;
      Enable     : Boolean;
      Defined_At : String)
   is
      State_Value : Rule_States;
      Param_Value : Integer;
   begin
      --  Initialize rule and alias common values
      if Alias = null then
         State_Value := Rule.Rule_State;
         Param_Value := Rule.Param;
      else
         State_Value := Alias.Alias_State;
         Param_Value := One_Integer_Parameter_Alias (Alias.all).Param;
      end if;

      --  Process the actual parameter
      if Param = "" then
         if Enable then
            Error
              ("(" & To_String (Rule.Name) &
               ") parameter is required for +R");
            Bad_Rule_Detected := True;
         else
            State_Value := Disabled;
         end if;
      else
         if Enable then
            if Check_Param_Redefinition
              and then Rule.Rule_State = Enabled and then Alias = null
            then
               Error
                 ("redefining at " & Defined_Str (Defined_At) &
                  " parameter for rule " & To_String (Rule.Name) &
                  " defined at " &
                  Defined_Str (To_String (Rule.Defined_At)));
               Rule_Option_Problem_Detected := True;
            end if;

            begin
               Param_Value := Integer'Value (Param);

               if Param_Value >= -1 then
                  State_Value     := Enabled;
                  Rule.Defined_At := To_Unbounded_String (Defined_At);
               else
                  Error
                    ("(" & To_String (Rule.Name) &
                     ") wrong parameter: " & Param);
                  State_Value       := Disabled;
                  Bad_Rule_Detected := True;
               end if;
            exception
               when Constraint_Error =>
                  Error
                    ("(" & To_String (Rule.Name) &
                     ") wrong parameter: " & Param);
                  State_Value       := Disabled;
                  Bad_Rule_Detected := True;
            end;

         else
            Error
              ("(" & To_String (Rule.Name) &
               ") no parameter allowed for -R");
            Bad_Rule_Detected := True;
         end if;
      end if;

      --  Update rule or alias values with the computed ones
      if Alias = null then
         Rule.Rule_State := State_Value;
         Rule.Param      := Param_Value;
      else
         Alias.Alias_State := State_Value;
         One_Integer_Parameter_Alias (Alias.all).Param :=
            Param_Value;
      end if;
   end Process_Rule_Parameter;

   overriding procedure Process_Rule_Parameter
     (Rule       : in out One_Boolean_Parameter_Rule;
      Alias      : Alias_Access;
      Param      : String;
      Enable     : Boolean;
      Defined_At : String)
   is
      State_Value : Rule_States;
      Param_Value : Tri_State;
   begin
      --  Initialize rule and alias common values
      if Alias = null then
         State_Value := Rule.Rule_State;
         Param_Value := Rule.Param;
      else
         State_Value := Alias.Alias_State;
         Param_Value := One_Boolean_Parameter_Alias (Alias.all).Param;
      end if;

      --  Process the actual parameter
      if Param = "" then
         if Enable then
            State_Value     := Enabled;
            Rule.Defined_At := To_Unbounded_String (Defined_At);
         else
            Param_Value := Unset;
            State_Value := Disabled;
         end if;
      elsif To_String (Rule.Parameters.Child (2).As_Parameter_Decl.
                       F_Param_Identifier.Text) /= To_Lower (Param)
      then
         Error ("(" & To_String (Rule.Name) & ") wrong parameter: " & Param);
         Bad_Rule_Detected := True;
         State_Value       := Disabled;
         Param_Value       := Unset;

      elsif Enable then
         if Check_Param_Redefinition and then Rule.Rule_State = Enabled
           and then Alias = null
         then
            Error
             ("redefining at " & Defined_Str (Defined_At) &
              " parameter " & Param & " for rule " & To_String (Rule.Name) &
              " defined at " & Defined_Str (To_String (Rule.Defined_At)));
            Rule_Option_Problem_Detected := True;
         end if;

         Param_Value     := On;
         State_Value     := Enabled;
         Rule.Defined_At := To_Unbounded_String (Defined_At);

      else
         Param_Value     := Off;
         State_Value     := Enabled;
         Rule.Defined_At := To_Unbounded_String (Defined_At);
      end if;

      --  Update rule or alias values with the computed ones
      if Alias = null then
         Rule.Rule_State := State_Value;
         Rule.Param      := Param_Value;
      else
         Alias.Alias_State := State_Value;
         One_Boolean_Parameter_Alias (Alias.all).Param :=
           Param_Value;
      end if;
   end Process_Rule_Parameter;

   overriding procedure Process_Rule_Parameter
     (Rule       : in out One_String_Parameter_Rule;
      Alias      : Alias_Access;
      Param      : String;
      Enable     : Boolean;
      Defined_At : String)
   is
      State_Value : Rule_States;
      Param_Value : Unbounded_Wide_Wide_String;
      File_Value  : Unbounded_String;
   begin
      --  Initialize rule and alias common values
      if Alias = null then
         State_Value := Rule.Rule_State;
      else
         State_Value := Alias.Alias_State;
      end if;

      --  Process the actual parameter
      if Param = "" then
         if Enable then
            Error
              ("(" & To_String (Rule.Name) &
               ") parameter is required for +R");
            Bad_Rule_Detected := True;
         else
            State_Value := Disabled;
         end if;
      elsif Enable then
         if Check_Param_Redefinition and then Rule.Rule_State = Enabled
           and then Alias = null
         then
            Error
              ("redefining at " & Defined_Str (Defined_At) &
               " parameter " & Param & " for rule " &
               To_String (Rule.Name) & " defined at " &
               Defined_Str (To_String (Rule.Defined_At)));
            Rule_Option_Problem_Detected := True;
         end if;

         --  Headers rule takes a file name as parameter, containing the
         --  header contents.

         if Rule.Name = "headers" then
            Rule.Load_File
              (To_Load => Param,
               File_Name => File_Value,
               File_Content => Param_Value,
               State => State_Value);
         else
            Append (Param_Value, To_Wide_Wide_String (Param));
         end if;

         State_Value     := Enabled;
         Rule.Defined_At := To_Unbounded_String (Defined_At);

      else
         State_Value     := Disabled;
         Rule.Defined_At := To_Unbounded_String (Defined_At);
      end if;

      --  Update rule or alias values with the computed ones
      if Alias = null then
         Rule.Rule_State := State_Value;
         Append (Rule.Param, Param_Value);
         Ada.Strings.Unbounded.Set_Unbounded_String (Rule.File,
                                                     To_String (File_Value));
      else
         declare
            Rec : One_String_Parameter_Alias renames
              One_String_Parameter_Alias (Alias.all);
         begin
            Rec.Alias_State := State_Value;
            Append (Rec.Param, Param_Value);
            Ada.Strings.Unbounded.Set_Unbounded_String
              (Rec.File, To_String (File_Value));
         end;
      end if;
   end Process_Rule_Parameter;

   overriding procedure Process_Rule_Parameter
     (Rule       : in out One_Integer_Or_Booleans_Parameter_Rule;
      Alias      : Alias_Access;
      Param      : String;
      Enable     : Boolean;
      Defined_At : String)
   is
      State_Value       : Rule_States;
      Int_Param_Value   : Integer;
      Bool_Params_Value : Boolean_Parameters;
      Param_Found       : Boolean := False;
   begin
      --  Intialize rule and alias common values
      if Alias = null then
         State_Value       := Rule.Rule_State;
         Int_Param_Value   := Rule.Integer_Param;
         Bool_Params_Value := Rule.Boolean_Params;
      else
         declare
            Rec : One_Integer_Or_Booleans_Parameter_Alias renames
              One_Integer_Or_Booleans_Parameter_Alias (Alias.all);
         begin
            State_Value       := Rec.Alias_State;
            Int_Param_Value   := Rec.Integer_Param;
            Bool_Params_Value := Rec.Boolean_Params;
         end;
      end if;

      --  Process the actual parameter
      if Param = "" then
         if Enable then
            if Rule.Name = "no_others_in_exception_handlers" then
               Error
                 ("(" & To_String (Rule.Name) &
                  ") parameter is required for +R");
               Bad_Rule_Detected := True;
            else
               State_Value     := Enabled;
               Rule.Defined_At := To_Unbounded_String (Defined_At);
            end if;
         else
            Rule.Integer_Param  := Integer'First;
            Rule.Boolean_Params := [others => Unset];
            State_Value         := Disabled;
         end if;
      else
         if Enable then

            --  First try to extract an integer
            declare
               Old_Integer : constant Integer := Int_Param_Value;
            begin
               Int_Param_Value := Integer'Value (Param);

               if Check_Param_Redefinition
                 and then Old_Integer /= Integer'First
                 and then Alias = null
               then
                  Error
                    ("redefining at " & Defined_Str (Defined_At) &
                     " parameter N for rule " & To_String (Rule.Name));
                  Rule_Option_Problem_Detected := True;
               end if;

               if Int_Param_Value >= 0 then
                  State_Value     := Enabled;
                  Rule.Defined_At := To_Unbounded_String (Defined_At);
               else
                  Error
                    ("(" & To_String (Rule.Name) &
                     ") wrong parameter: " & Param);
                  Bad_Rule_Detected := True;
                  Int_Param_Value   := Integer'First;
                  Bool_Params_Value := [others => Unset];
                  State_Value       := Disabled;
               end if;

               Param_Found := True;
            exception
               when Constraint_Error =>
                  null;
            end;

            --  Then find the relevant boolean if integer has not been parsed
            if not Param_Found then
               for J in 2 .. Rule.Parameters.Last_Child_Index loop
                  if To_String
                      (Rule.Parameters.Child (J).As_Parameter_Decl
                         .F_Param_Identifier
                         .Text) =
                    To_Lower (Param)
                  then
                     if Check_Param_Redefinition
                       and then Rule.Boolean_Params (J) = On
                       and then Alias = null
                     then
                        Error
                          ("redefining at " & Defined_Str (Defined_At) &
                           " parameter " & Param & " for rule " &
                           To_String (Rule.Name));
                        Rule_Option_Problem_Detected := True;
                     end if;

                     Bool_Params_Value (J) := On;
                     State_Value           := Enabled;
                     Rule.Defined_At := To_Unbounded_String (Defined_At);
                     Param_Found           := True;
                  end if;
               end loop;
            end if;

            --  If we get didn't find any valid parameter there is an error
            if not Param_Found then
               Error
                 ("(" & To_String (Rule.Name) & ") wrong parameter: " &
                  Param);
               Bad_Rule_Detected := True;
               Int_Param_Value   := Integer'First;
               Bool_Params_Value := [others => Unset];
               State_Value       := Disabled;
            end if;

         else
            Error
              ("(" & To_String (Rule.Name) &
               ") no parameter allowed for -R");
            Bad_Rule_Detected := True;
         end if;
      end if;

      --  Update rule or alias values with the computed ones
      if Alias = null then
         Rule.Rule_State     := State_Value;
         Rule.Integer_Param  := Int_Param_Value;
         Rule.Boolean_Params := Bool_Params_Value;
      else
         declare
            Rec : One_Integer_Or_Booleans_Parameter_Alias renames
              One_Integer_Or_Booleans_Parameter_Alias (Alias.all);
         begin
            Rec.Alias_State    := State_Value;
            Rec.Integer_Param  := Int_Param_Value;
            Rec.Boolean_Params := Bool_Params_Value;
         end;
      end if;
   end Process_Rule_Parameter;

   overriding procedure Process_Rule_Parameter
     (Rule       : in out One_Array_Parameter_Rule;
      Alias      : Alias_Access;
      Param      : String;
      Enable     : Boolean;
      Defined_At : String)
   is
      State_Value : Rule_States;
      Param_Value : Unbounded_Wide_Wide_String;
      File_Value  : Unbounded_String;
   begin
      --  Intialize rule and alias common values
      if Alias = null then
         State_Value := Rule.Rule_State;
         Param_Value := Rule.Param;
         File_Value  := Rule.File;
      else
         declare
            Rec : One_Array_Parameter_Alias renames
              One_Array_Parameter_Alias (Alias.all);
         begin
            State_Value := Rec.Alias_State;
            Param_Value := Rec.Param;
            File_Value  := Rec.File;
         end;
      end if;

      --  Process the actual parameter
      if Param = "" then
         State_Value := (if Enable then Enabled else Disabled);
      elsif Enable then
         if Check_Param_Redefinition and then Rule.Rule_State = Enabled
           and then Alias = null
         then
            Error
             ("redefining at " & Defined_Str (Defined_At) &
              " parameter " & Param & " for rule " & To_String (Rule.Name) &
              " defined at " & Defined_Str (To_String (Rule.Defined_At)));
            Rule_Option_Problem_Detected := True;
         end if;

         if Rule.Name = "name_clashes" then
            Ada.Strings.Unbounded.Set_Unbounded_String (File_Value, Param);
            Load_Dictionary
              (Expand_Env_Variables (Param),
               To_String (Rule.Name),
               State_Value,
               Param_Value);
            Rule.Defined_At := To_Unbounded_String (Defined_At);
         else
            if (Rule.Name = "parameters_out_of_order"
                and then Param not in
                  "in" | "defaulted_in" | "in_out" | "access" | "out")
              or else (Rule.Name = "actual_parameters"
                       and then Slice_Count (Create (Param, ":")) /= 3)
            then
               Error
                 ("(" & To_String (Rule.Name) &
                  ") wrong parameter: " & Param);
               Bad_Rule_Detected := True;
               State_Value       := Disabled;
               return;
            end if;

            if Length (Param_Value) /= 0 then
               Append (Param_Value, ",");
            end if;

            Append (Param_Value, To_Wide_Wide_String (Param));
            State_Value     := Enabled;
            Rule.Defined_At := To_Unbounded_String (Defined_At);
         end if;
      else
         Set_Unbounded_Wide_Wide_String (Param_Value, "");
         Error ("(" & To_String (Rule.Name) & ") no parameter allowed for -R");
         Bad_Rule_Detected := True;
         State_Value       := Disabled;
         Rule.Defined_At   := To_Unbounded_String (Defined_At);
      end if;

      --  Set the rule or alias parameter values according to the
      --  computed ones.
      if Alias = null then
         Rule.Rule_State := State_Value;
         Rule.Param      := Param_Value;
         Rule.File       := File_Value;
      else
         declare
            Rec : One_Array_Parameter_Alias renames
              One_Array_Parameter_Alias (Alias.all);
         begin
            Rec.Alias_State := State_Value;
            Rec.Param       := Param_Value;
            Rec.File        := File_Value;
         end;
      end if;
   end Process_Rule_Parameter;

   overriding procedure Process_Rule_Parameter
     (Rule       : in out Identifier_Suffixes_Rule;
      Alias      : Alias_Access;
      Param      : String;
      Enable     : Boolean;
      Defined_At : String)
   is
      Paren_Index : Natural;
      Norm_Param  : constant String := Remove_Spaces (Param);
      Lower_Param : constant String := To_Lower (Param);

      State_Value                : Rule_States;
      Type_Suffix_Value          : Unbounded_Wide_Wide_String;
      Access_Suffix_Value        : Unbounded_Wide_Wide_String;
      Access_Access_Suffix_Value : Unbounded_Wide_Wide_String;
      Class_Access_Suffix_Value  : Unbounded_Wide_Wide_String;
      Class_Subtype_Suffix_Value : Unbounded_Wide_Wide_String;
      Constant_Suffix_Value      : Unbounded_Wide_Wide_String;
      Renaming_Suffix_Value      : Unbounded_Wide_Wide_String;
      Access_Obj_Suffix_Value    : Unbounded_Wide_Wide_String;
      Interrupt_Suffix_Value     : Unbounded_Wide_Wide_String;
   begin
      --  Initialize the parameter values
      if Alias = null then
         State_Value                := Rule.Rule_State;
         Type_Suffix_Value          := Rule.Type_Suffix;
         Access_Suffix_Value        := Rule.Access_Suffix;
         Access_Access_Suffix_Value := Rule.Access_Access_Suffix;
         Class_Access_Suffix_Value  := Rule.Class_Access_Suffix;
         Class_Subtype_Suffix_Value := Rule.Class_Subtype_Suffix;
         Constant_Suffix_Value      := Rule.Constant_Suffix;
         Renaming_Suffix_Value      := Rule.Renaming_Suffix;
         Access_Obj_Suffix_Value    := Rule.Access_Obj_Suffix;
         Interrupt_Suffix_Value     := Rule.Interrupt_Suffix;
      else
         declare
            Rec : Identifier_Suffixes_Alias renames
               Identifier_Suffixes_Alias (Alias.all);
         begin
            State_Value                := Rec.Alias_State;
            Type_Suffix_Value          := Rec.Type_Suffix;
            Access_Suffix_Value        := Rec.Access_Suffix;
            Access_Access_Suffix_Value := Rec.Access_Access_Suffix;
            Class_Access_Suffix_Value  := Rec.Class_Access_Suffix;
            Class_Subtype_Suffix_Value := Rec.Class_Subtype_Suffix;
            Constant_Suffix_Value      := Rec.Constant_Suffix;
            Renaming_Suffix_Value      := Rec.Renaming_Suffix;
            Access_Obj_Suffix_Value    := Rec.Access_Obj_Suffix;
            Interrupt_Suffix_Value     := Rec.Interrupt_Suffix;
         end;
      end if;

      --  Process the actual parameter
      if Norm_Param = "" then
         State_Value := (if Enable then Enabled else Disabled);
      elsif Enable then
         State_Value     := Enabled;
         Rule.Defined_At := To_Unbounded_String (Defined_At);

         if Lower_Param = "default" then
            Set_Unbounded_Wide_Wide_String (Type_Suffix_Value, "_T");
            Set_Unbounded_Wide_Wide_String (Access_Suffix_Value, "_A");
            Set_Unbounded_Wide_Wide_String (Access_Access_Suffix_Value, "");
            Set_Unbounded_Wide_Wide_String (Class_Access_Suffix_Value, "");
            Set_Unbounded_Wide_Wide_String (Class_Subtype_Suffix_Value, "");
            Set_Unbounded_Wide_Wide_String (Constant_Suffix_Value, "_C");
            Set_Unbounded_Wide_Wide_String (Renaming_Suffix_Value, "_R");
            Set_Unbounded_Wide_Wide_String (Access_Obj_Suffix_Value, "");
            Set_Unbounded_Wide_Wide_String (Interrupt_Suffix_Value, "");

         elsif Has_Prefix (Norm_Param, "type_suffix=") then
            Set_Unbounded_Wide_Wide_String
              (Type_Suffix_Value,
               To_Wide_Wide_String
                 (Norm_Param (Norm_Param'First + 12 .. Norm_Param'Last)));

         elsif Has_Prefix (Norm_Param, "access_suffix=") then
            if Norm_Param (Norm_Param'Last) = ')' then
               Paren_Index :=
                 Index
                   (Norm_Param (Norm_Param'First + 14 .. Norm_Param'Last),
                    "(");
               Set_Unbounded_Wide_Wide_String
                 (Access_Suffix_Value,
                  To_Wide_Wide_String
                    (Norm_Param (Norm_Param'First + 14 .. Paren_Index - 1)));
               Set_Unbounded_Wide_Wide_String
                 (Access_Access_Suffix_Value,
                  To_Wide_Wide_String
                    (Norm_Param (Paren_Index + 1 .. Norm_Param'Last - 1)));

            else
               Set_Unbounded_Wide_Wide_String
                 (Access_Suffix_Value,
                  To_Wide_Wide_String
                    (Norm_Param (Norm_Param'First + 14 .. Norm_Param'Last)));
               Set_Unbounded_Wide_Wide_String
                 (Access_Access_Suffix_Value, "");
            end if;

         elsif Has_Prefix (Norm_Param, "class_access_suffix=") then
            Set_Unbounded_Wide_Wide_String
              (Class_Access_Suffix_Value,
               To_Wide_Wide_String
                 (Norm_Param (Norm_Param'First + 20 .. Norm_Param'Last)));

         elsif Has_Prefix (Norm_Param, "class_subtype_suffix=") then
            Set_Unbounded_Wide_Wide_String
              (Class_Subtype_Suffix_Value,
               To_Wide_Wide_String
                 (Norm_Param (Norm_Param'First + 21 .. Norm_Param'Last)));

         elsif Has_Prefix (Norm_Param, "constant_suffix=") then
            Set_Unbounded_Wide_Wide_String
              (Constant_Suffix_Value,
               To_Wide_Wide_String
                 (Norm_Param (Norm_Param'First + 16 .. Norm_Param'Last)));

         elsif Has_Prefix (Norm_Param, "renaming_suffix=") then
            Set_Unbounded_Wide_Wide_String
              (Renaming_Suffix_Value,
               To_Wide_Wide_String
                 (Norm_Param (Norm_Param'First + 16 .. Norm_Param'Last)));

         elsif Has_Prefix (Norm_Param, "access_obj_suffix=") then
            Set_Unbounded_Wide_Wide_String
              (Access_Obj_Suffix_Value,
               To_Wide_Wide_String
                 (Norm_Param (Norm_Param'First + 18 .. Norm_Param'Last)));

         elsif Has_Prefix (Norm_Param, "interrupt_suffix=") then
            Set_Unbounded_Wide_Wide_String
              (Interrupt_Suffix_Value,
               To_Wide_Wide_String
                 (Norm_Param (Norm_Param'First + 17 .. Norm_Param'Last)));
         else
            Error
              ("(" & To_String (Rule.Name) &
               ") wrong parameter: " & Param);
            Bad_Rule_Detected := True;
            State_Value       := Disabled;
         end if;
      else
         State_Value     := Disabled;
         Rule.Defined_At := To_Unbounded_String (Defined_At);

         if Lower_Param = "all_suffixes" then
            Set_Unbounded_Wide_Wide_String (Type_Suffix_Value, "");
            Set_Unbounded_Wide_Wide_String (Access_Suffix_Value, "");
            Set_Unbounded_Wide_Wide_String (Access_Access_Suffix_Value, "");
            Set_Unbounded_Wide_Wide_String (Class_Access_Suffix_Value, "");
            Set_Unbounded_Wide_Wide_String (Class_Subtype_Suffix_Value, "");
            Set_Unbounded_Wide_Wide_String (Constant_Suffix_Value, "");
            Set_Unbounded_Wide_Wide_String (Renaming_Suffix_Value, "");
            Set_Unbounded_Wide_Wide_String (Access_Obj_Suffix_Value, "");
            Set_Unbounded_Wide_Wide_String (Interrupt_Suffix_Value, "");

         elsif Lower_Param = "type_suffix" then
            Set_Unbounded_Wide_Wide_String (Type_Suffix_Value, "");
         elsif Lower_Param = "access_suffix" then
            Set_Unbounded_Wide_Wide_String (Access_Suffix_Value, "");
            Set_Unbounded_Wide_Wide_String (Access_Access_Suffix_Value, "");
         elsif Lower_Param = "class_access_suffix" then
            Set_Unbounded_Wide_Wide_String (Class_Access_Suffix_Value, "");
         elsif Lower_Param = "class_subtype_suffix" then
            Set_Unbounded_Wide_Wide_String (Class_Subtype_Suffix_Value, "");
         elsif Lower_Param = "constant_suffix" then
            Set_Unbounded_Wide_Wide_String (Constant_Suffix_Value, "");
         elsif Lower_Param = "renaming_suffix" then
            Set_Unbounded_Wide_Wide_String (Renaming_Suffix_Value, "");
         elsif Lower_Param = "access_obj_suffix" then
            Set_Unbounded_Wide_Wide_String (Access_Obj_Suffix_Value, "");
         elsif Lower_Param = "interrupt_suffix" then
            Set_Unbounded_Wide_Wide_String (Interrupt_Suffix_Value, "");
         else
            Error
              ("(" & To_String (Rule.Name) &
               ") wrong parameter: " & Param);
            Bad_Rule_Detected := True;
         end if;
      end if;

      --  Update rule or alias values with the computed ones
      if Alias = null then
         Rule.Rule_State           := State_Value;
         Rule.Type_Suffix          := Type_Suffix_Value;
         Rule.Access_Suffix        := Access_Suffix_Value;
         Rule.Access_Access_Suffix := Access_Access_Suffix_Value;
         Rule.Class_Access_Suffix  := Class_Access_Suffix_Value;
         Rule.Class_Subtype_Suffix := Class_Subtype_Suffix_Value;
         Rule.Constant_Suffix      := Constant_Suffix_Value;
         Rule.Renaming_Suffix      := Renaming_Suffix_Value;
         Rule.Access_Obj_Suffix    := Access_Obj_Suffix_Value;
         Rule.Interrupt_Suffix     := Interrupt_Suffix_Value;
      else
         declare
            Rec : Identifier_Suffixes_Alias renames
               Identifier_Suffixes_Alias (Alias.all);
         begin
            Rec.Alias_State          := State_Value;
            Rec.Type_Suffix          := Type_Suffix_Value;
            Rec.Access_Suffix        := Access_Suffix_Value;
            Rec.Access_Access_Suffix := Access_Access_Suffix_Value;
            Rec.Class_Access_Suffix  := Class_Access_Suffix_Value;
            Rec.Class_Subtype_Suffix := Class_Subtype_Suffix_Value;
            Rec.Constant_Suffix      := Constant_Suffix_Value;
            Rec.Renaming_Suffix      := Renaming_Suffix_Value;
            Rec.Access_Obj_Suffix    := Access_Obj_Suffix_Value;
            Rec.Interrupt_Suffix     := Interrupt_Suffix_Value;
         end;
      end if;
   end Process_Rule_Parameter;

   overriding procedure Process_Rule_Parameter
     (Rule       : in out Identifier_Prefixes_Rule;
      Alias      : Alias_Access;
      Param      : String;
      Enable     : Boolean;
      Defined_At : String)
   is
      Col_Index   : Natural;
      Norm_Param  : constant String := Remove_Spaces (Param);
      Lower_Param : constant String := To_Lower (Param);

      State_Value                    : Rule_States;
      Type_Prefix_Value              : Unbounded_Wide_Wide_String;
      Concurrent_Prefix_Value        : Unbounded_Wide_Wide_String;
      Access_Prefix_Value            : Unbounded_Wide_Wide_String;
      Class_Access_Prefix_Value      : Unbounded_Wide_Wide_String;
      Subprogram_Access_Prefix_Value : Unbounded_Wide_Wide_String;
      Derived_Prefix_Value           : Unbounded_Wide_Wide_String;
      Constant_Prefix_Value          : Unbounded_Wide_Wide_String;
      Exception_Prefix_Value         : Unbounded_Wide_Wide_String;
      Enum_Prefix_Value              : Unbounded_Wide_Wide_String;
      Exclusive_Value                : Tri_State;
   begin
      --  Initialize rule and alias common values
      if Alias = null then
         State_Value                    := Rule.Rule_State;
         Type_Prefix_Value              := Rule.Type_Prefix;
         Concurrent_Prefix_Value        := Rule.Concurrent_Prefix;
         Access_Prefix_Value            := Rule.Access_Prefix;
         Class_Access_Prefix_Value      := Rule.Class_Access_Prefix;
         Subprogram_Access_Prefix_Value := Rule.Subprogram_Access_Prefix;
         Derived_Prefix_Value           := Rule.Derived_Prefix;
         Constant_Prefix_Value          := Rule.Constant_Prefix;
         Exception_Prefix_Value         := Rule.Exception_Prefix;
         Enum_Prefix_Value              := Rule.Enum_Prefix;
         Exclusive_Value                := Rule.Exclusive;
      else
         declare
            Rec : Identifier_Prefixes_Alias renames
              Identifier_Prefixes_Alias (Alias.all);
         begin
            State_Value                    := Rec.Alias_State;
            Type_Prefix_Value              := Rec.Type_Prefix;
            Concurrent_Prefix_Value        := Rec.Concurrent_Prefix;
            Access_Prefix_Value            := Rec.Access_Prefix;
            Class_Access_Prefix_Value      := Rec.Class_Access_Prefix;
            Subprogram_Access_Prefix_Value := Rec.Subprogram_Access_Prefix;
            Derived_Prefix_Value           := Rec.Derived_Prefix;
            Constant_Prefix_Value          := Rec.Constant_Prefix;
            Exception_Prefix_Value         := Rec.Exception_Prefix;
            Enum_Prefix_Value              := Rec.Enum_Prefix;
            Exclusive_Value                := Rec.Exclusive;
         end;
      end if;

      --  Process the actual parameter
      if Norm_Param = "" then
         State_Value := (if Enable then Enabled else Disabled);
      elsif Enable then
         State_Value     := Enabled;
         Rule.Defined_At := To_Unbounded_String (Defined_At);

         if Lower_Param = "exclusive" then
            Exclusive_Value := On;

         elsif Has_Prefix (Norm_Param, "type=") then
            Set_Unbounded_Wide_Wide_String
              (Type_Prefix_Value,
               To_Wide_Wide_String
                 (Norm_Param (Norm_Param'First + 5 .. Norm_Param'Last)));

         elsif Has_Prefix (Norm_Param, "concurrent=") then
            Set_Unbounded_Wide_Wide_String
              (Concurrent_Prefix_Value,
               To_Wide_Wide_String
                 (Norm_Param
                    (Norm_Param'First + 11 .. Norm_Param'Last)));

         elsif Has_Prefix (Norm_Param, "access=") then
            Set_Unbounded_Wide_Wide_String
              (Access_Prefix_Value,
               To_Wide_Wide_String
                 (Norm_Param (Norm_Param'First + 7 .. Norm_Param'Last)));

         elsif Has_Prefix (Norm_Param, "class_access=") then
            Set_Unbounded_Wide_Wide_String
              (Class_Access_Prefix_Value,
               To_Wide_Wide_String
                 (Norm_Param
                    (Norm_Param'First + 13 .. Norm_Param'Last)));

         elsif Has_Prefix (Norm_Param, "subprogram_access=") then
            Set_Unbounded_Wide_Wide_String
              (Subprogram_Access_Prefix_Value,
               To_Wide_Wide_String
                 (Norm_Param
                    (Norm_Param'First + 18 .. Norm_Param'Last)));

         elsif Has_Prefix (Norm_Param, "derived=") then
            Col_Index :=
              Index
                (Norm_Param (Norm_Param'First + 8 .. Norm_Param'Last),
                 ":");

            if Col_Index /= 0 then
               if Length (Derived_Prefix_Value) /= 0 then
                  Append (Derived_Prefix_Value, ",");
               end if;

               Append
                 (Derived_Prefix_Value,
                  To_Wide_Wide_String
                    (To_Lower
                       (Norm_Param
                          (Norm_Param'First + 8 .. Col_Index - 1))));
               Append
                 (Derived_Prefix_Value,
                  To_Wide_Wide_String
                    (Norm_Param (Col_Index .. Norm_Param'Last)));

            else
               Error
                 ("(" & To_String (Rule.Name) & ") wrong parameter: " &
                  Param);
               Bad_Rule_Detected := True;
               State_Value       := Disabled;
            end if;

         elsif Has_Prefix (Norm_Param, "constant=") then
            Set_Unbounded_Wide_Wide_String
              (Constant_Prefix_Value,
               To_Wide_Wide_String
                 (Norm_Param (Norm_Param'First + 9 .. Norm_Param'Last)));

         elsif Has_Prefix (Norm_Param, "exception=") then
            Set_Unbounded_Wide_Wide_String
              (Exception_Prefix_Value,
               To_Wide_Wide_String
                 (Norm_Param
                    (Norm_Param'First + 10 .. Norm_Param'Last)));

         elsif Has_Prefix (Norm_Param, "enum=") then
            Set_Unbounded_Wide_Wide_String
              (Enum_Prefix_Value,
               To_Wide_Wide_String
                 (Norm_Param (Norm_Param'First + 5 .. Norm_Param'Last)));

         else
            Error
              ("(" & To_String (Rule.Name) & ") wrong parameter: " &
               Param);
            Bad_Rule_Detected := True;
            State_Value       := Disabled;
         end if;
      else
         State_Value     := Disabled;
         Rule.Defined_At := To_Unbounded_String (Defined_At);

         if Lower_Param = "exclusive" then
            Exclusive_Value := Off;

         elsif Lower_Param = "all_prefixes" then
            Set_Unbounded_Wide_Wide_String (Type_Prefix_Value, "");
            Set_Unbounded_Wide_Wide_String (Concurrent_Prefix_Value, "");
            Set_Unbounded_Wide_Wide_String (Access_Prefix_Value, "");
            Set_Unbounded_Wide_Wide_String
              (Class_Access_Prefix_Value, "");
            Set_Unbounded_Wide_Wide_String
              (Subprogram_Access_Prefix_Value, "");
            Set_Unbounded_Wide_Wide_String (Derived_Prefix_Value, "");
            Set_Unbounded_Wide_Wide_String (Constant_Prefix_Value, "");
            Set_Unbounded_Wide_Wide_String (Exception_Prefix_Value, "");
            Set_Unbounded_Wide_Wide_String (Enum_Prefix_Value, "");

         elsif Lower_Param = "type" then
            Set_Unbounded_Wide_Wide_String (Type_Prefix_Value, "");
         elsif Lower_Param = "concurrent" then
            Set_Unbounded_Wide_Wide_String (Concurrent_Prefix_Value, "");
         elsif Lower_Param = "access" then
            Set_Unbounded_Wide_Wide_String (Access_Prefix_Value, "");
         elsif Lower_Param = "class_access" then
            Set_Unbounded_Wide_Wide_String
              (Class_Access_Prefix_Value, "");
         elsif Lower_Param = "subprogram_access" then
            Set_Unbounded_Wide_Wide_String
              (Subprogram_Access_Prefix_Value, "");
         elsif Lower_Param = "derived" then
            Set_Unbounded_Wide_Wide_String (Derived_Prefix_Value, "");
         elsif Lower_Param = "constant" then
            Set_Unbounded_Wide_Wide_String (Constant_Prefix_Value, "");
         elsif Lower_Param = "exception" then
            Set_Unbounded_Wide_Wide_String (Exception_Prefix_Value, "");
         elsif Lower_Param = "enum" then
            Set_Unbounded_Wide_Wide_String (Enum_Prefix_Value, "");
         else
            Error
              ("(" & To_String (Rule.Name) & ") wrong parameter: " &
               Param);
            Bad_Rule_Detected := True;
         end if;
      end if;

      --  Set the parameter values in the rule or alias
      if Alias = null then
         Rule.Rule_State               := State_Value;
         Rule.Type_Prefix              := Type_Prefix_Value;
         Rule.Concurrent_Prefix        := Concurrent_Prefix_Value;
         Rule.Access_Prefix            := Access_Prefix_Value;
         Rule.Class_Access_Prefix      := Class_Access_Prefix_Value;
         Rule.Subprogram_Access_Prefix := Subprogram_Access_Prefix_Value;
         Rule.Derived_Prefix           := Derived_Prefix_Value;
         Rule.Constant_Prefix          := Constant_Prefix_Value;
         Rule.Exception_Prefix         := Exception_Prefix_Value;
         Rule.Enum_Prefix              := Enum_Prefix_Value;
         Rule.Exclusive                := Exclusive_Value;
      else
         declare
            Rec : Identifier_Prefixes_Alias renames
              Identifier_Prefixes_Alias (Alias.all);
         begin
            Rec.Alias_State              := State_Value;
            Rec.Type_Prefix              := Type_Prefix_Value;
            Rec.Concurrent_Prefix        := Concurrent_Prefix_Value;
            Rec.Access_Prefix            := Access_Prefix_Value;
            Rec.Class_Access_Prefix      := Class_Access_Prefix_Value;
            Rec.Subprogram_Access_Prefix := Subprogram_Access_Prefix_Value;
            Rec.Derived_Prefix           := Derived_Prefix_Value;
            Rec.Constant_Prefix          := Constant_Prefix_Value;
            Rec.Exception_Prefix         := Exception_Prefix_Value;
            Rec.Enum_Prefix              := Enum_Prefix_Value;
            Rec.Exclusive                := Exclusive_Value;
         end;
      end if;
   end Process_Rule_Parameter;

   overriding procedure Process_Rule_Parameter
     (Rule       : in out Identifier_Casing_Rule;
      Alias      : Alias_Access;
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
              Label & " casing for rule " & To_String (Rule.Name));
            Rule_Option_Problem_Detected := True;
         end if;

         Set_Unbounded_Wide_Wide_String (S, To_Wide_Wide_String (Val));
      end Check_And_Set;

      Norm_Param : constant String := To_Lower (Remove_Spaces (Param));

      State_Value            : Rule_States;
      Type_Casing_Value      : Unbounded_Wide_Wide_String;
      Enum_Casing_Value      : Unbounded_Wide_Wide_String;
      Constant_Casing_Value  : Unbounded_Wide_Wide_String;
      Exception_Casing_Value : Unbounded_Wide_Wide_String;
      Others_Casing_Value    : Unbounded_Wide_Wide_String;
      Exclude_Value          : Unbounded_Wide_Wide_String;
   begin
      --  Initialize rule and alias common values
      if Alias = null then
         State_Value            := Rule.Rule_State;
         Type_Casing_Value      := Rule.Type_Casing;
         Enum_Casing_Value      := Rule.Enum_Casing;
         Constant_Casing_Value  := Rule.Constant_Casing;
         Exception_Casing_Value := Rule.Exception_Casing;
         Others_Casing_Value    := Rule.Others_Casing;
         Exclude_Value          := Rule.Exclude;
      else
         declare
            Rec : Identifier_Casing_Alias renames
              Identifier_Casing_Alias (Alias.all);
         begin
            State_Value            := Rec.Alias_State;
            Type_Casing_Value      := Rec.Type_Casing;
            Enum_Casing_Value      := Rec.Enum_Casing;
            Constant_Casing_Value  := Rec.Constant_Casing;
            Exception_Casing_Value := Rec.Exception_Casing;
            Others_Casing_Value    := Rec.Others_Casing;
            Exclude_Value          := Rec.Exclude;
         end;
      end if;

      --  Process the actual parameter
      if Norm_Param = "" then
         State_Value := (if Enable then Enabled else Disabled);
      elsif Enable then
         State_Value     := Enabled;
         Rule.Defined_At := To_Unbounded_String (Defined_At);

         if Has_Prefix (Norm_Param, "type=") then
            Check_And_Set
              (Type_Casing_Value,
               Norm_Param (Norm_Param'First + 5 .. Norm_Param'Last),
               "type");

         elsif Has_Prefix (Norm_Param, "enum=") then
            Check_And_Set
              (Enum_Casing_Value,
               Norm_Param (Norm_Param'First + 5 .. Norm_Param'Last),
               "enumeration literal");

         elsif Has_Prefix (Norm_Param, "constant=") then
            Check_And_Set
              (Constant_Casing_Value,
               Norm_Param (Norm_Param'First + 9 .. Norm_Param'Last),
               "constant");

         elsif Has_Prefix (Norm_Param, "exception=") then
            Check_And_Set
              (Exception_Casing_Value,
               Norm_Param (Norm_Param'First + 10 .. Norm_Param'Last),
               "exception");

         elsif Has_Prefix (Norm_Param, "others=") then
            Check_And_Set
              (Others_Casing_Value,
               Norm_Param (Norm_Param'First + 7 .. Norm_Param'Last),
               "others");

         elsif Has_Prefix (Norm_Param, "exclude=") then
            Load_Dictionary
              (Expand_Env_Variables
                 (Norm_Param (Norm_Param'First + 8 .. Norm_Param'Last)),
               To_String (Rule.Name), State_Value, Exclude_Value);

         else
            Error
              ("(" & To_String (Rule.Name) & ") wrong parameter: " & Param);
            Bad_Rule_Detected := True;
            State_Value       := Disabled;
         end if;
      else
         Error ("(" & To_String (Rule.Name) & ") no parameter allowed for -R");
         Bad_Rule_Detected := True;
         State_Value       := Disabled;
         Rule.Defined_At   := To_Unbounded_String (Defined_At);
      end if;

      --  Update rule or alias values with the computed ones
      if Alias = null then
         Rule.Rule_State       := State_Value;
         Rule.Type_Casing      := Type_Casing_Value;
         Rule.Enum_Casing      := Enum_Casing_Value;
         Rule.Constant_Casing  := Constant_Casing_Value;
         Rule.Exception_Casing := Exception_Casing_Value;
         Rule.Others_Casing    := Others_Casing_Value;
         Rule.Exclude          := Exclude_Value;
      else
         declare
            Rec : Identifier_Casing_Alias renames
              Identifier_Casing_Alias (Alias.all);
         begin
            Rec.Alias_State      := State_Value;
            Rec.Type_Casing      := Type_Casing_Value;
            Rec.Enum_Casing      := Enum_Casing_Value;
            Rec.Constant_Casing  := Constant_Casing_Value;
            Rec.Exception_Casing := Exception_Casing_Value;
            Rec.Others_Casing    := Others_Casing_Value;
            Rec.Exclude          := Exclude_Value;
         end;
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
      Alias      : Alias_Access;
      Param      : String;
      Enable     : Boolean;
      Defined_At : String)
   is
      Lower_Param : constant String := To_Lower (Param);

      State_Value                    : Rule_States;
      All_Flag_Value                 : Tri_State;
      Forbidden_Value, Allowed_Value : Unbounded_Wide_Wide_String;
   begin
      --  Initialize rule and alias common values
      if Alias = null then
         State_Value     := Rule.Rule_State;
         All_Flag_Value  := Rule.All_Flag;
         Forbidden_Value := Rule.Forbidden;
         Allowed_Value   := Rule.Allowed;
      else
         declare
            Rec : Forbidden_Alias renames Forbidden_Alias (Alias.all);
         begin
            State_Value     := Rec.Alias_State;
            All_Flag_Value  := Rec.All_Flag;
            Forbidden_Value := Rec.Forbidden;
            Allowed_Value   := Rec.Allowed;
         end;
      end if;

      --  Process the actual parameter
      if Param = "" then
         State_Value := (if Enable then Enabled else Disabled);
      elsif Enable then
         if Lower_Param = "all" then
            All_Flag_Value := On;
         else
            if Length (Forbidden_Value) /= 0 then
               Append (Forbidden_Value, ",");
            end if;

            if Lower_Param = "gnat" then
               if Rule.Name = "forbidden_attributes" then
                  Append (Forbidden_Value, GNAT_Attributes);
               elsif Rule.Name = "forbidden_pragmas" then
                  Append (Forbidden_Value, GNAT_Pragmas);
               else
                  Append (Forbidden_Value, To_Wide_Wide_String (Lower_Param));
               end if;
            else
               Append (Forbidden_Value, To_Wide_Wide_String (Lower_Param));
            end if;
         end if;

         State_Value     := Enabled;
         Rule.Defined_At := To_Unbounded_String (Defined_At);

      else
         if Lower_Param = "all" then
            All_Flag_Value  := Off;
            State_Value     := Disabled;
            Forbidden_Value := Null_Unbounded_Wide_Wide_String;
            Allowed_Value   := Null_Unbounded_Wide_Wide_String;

         else
            if Length (Allowed_Value) /= 0 then
               Append (Allowed_Value, ",");
            end if;

            if Lower_Param = "gnat" then
               if Rule.Name = "forbidden_attributes" then
                  Append (Allowed_Value, GNAT_Attributes);
               elsif Rule.Name = "forbidden_pragmas" then
                  Append (Allowed_Value, GNAT_Pragmas);
               else
                  Append (Allowed_Value, To_Wide_Wide_String (Lower_Param));
               end if;
            else
               Append (Allowed_Value, To_Wide_Wide_String (Lower_Param));
            end if;
         end if;

         Rule.Defined_At := To_Unbounded_String (Defined_At);
      end if;

      --  Update the rule or alias values with the computed ones
      if Alias = null then
         Rule.Rule_State := State_Value;
         Rule.All_Flag   := All_Flag_Value;
         Rule.Forbidden  := Forbidden_Value;
         Rule.Allowed    := Allowed_Value;
      else
         declare
            Rec : Forbidden_Alias renames Forbidden_Alias (Alias.all);
         begin
            Rec.Alias_State := State_Value;
            Rec.All_Flag    := All_Flag_Value;
            Rec.Forbidden   := Forbidden_Value;
            Rec.Allowed     := Allowed_Value;
         end;
      end if;
   end Process_Rule_Parameter;

   overriding procedure Process_Rule_Parameter
     (Rule       : in out Silent_Exception_Handlers_Rule;
      Alias      : Alias_Access;
      Param      : String;
      Enable     : Boolean;
      Defined_At : String)
   is
      procedure Add_To
        (Str   : in out Unbounded_Wide_Wide_String;
         Param : String);
      --  Add parameter Param to Str, separated with ","

      ------------
      -- Add_To --
      ------------

      procedure Add_To
        (Str   : in out Unbounded_Wide_Wide_String;
         Param : String) is
      begin
         if Length (Str) /= 0 then
            Append (Str, ",");
         end if;

         Append (Str, To_Wide_Wide_String (Param));
      end Add_To;

      State_Value : Rule_States;
      Subprograms_Value, Subprogram_Regexps_Value : Unbounded_Wide_Wide_String;
   begin
      --  Initialize rule and alias common values
      if Alias = null then
         State_Value              := Rule.Rule_State;
         Subprograms_Value        := Rule.Subprograms;
         Subprogram_Regexps_Value := Rule.Subprogram_Regexps;
      else
         declare
            Rec : Silent_Exception_Handlers_Alias renames
              Silent_Exception_Handlers_Alias (Alias.all);
         begin
            State_Value              := Rec.Alias_State;
            Subprograms_Value        := Rec.Subprograms;
            Subprogram_Regexps_Value := Rec.Subprogram_Regexps;
         end;
      end if;

      --  Process the acutal parameter
      if Param = "" then
         State_Value := (if Enable then Enabled else Disabled);
      elsif Enable then
         if Param (Param'First) = '"' then
            Add_To (Subprogram_Regexps_Value,
                    Param (Param'First + 1 .. Param'Last - 1));
         else
            Add_To (Subprograms_Value, To_Lower (Param));
         end if;

         State_Value     := Enabled;
         Rule.Defined_At := To_Unbounded_String (Defined_At);

      else
         Error ("(" & To_String (Rule.Name) & ") no parameter allowed for -R");
         Bad_Rule_Detected := True;
         State_Value       := Disabled;
         Rule.Defined_At   := To_Unbounded_String (Defined_At);
      end if;

      --  Update rule or alias values with the computed ones
      if Alias = null then
         Rule.Rule_State         := State_Value;
         Rule.Subprograms        := Subprograms_Value;
         Rule.Subprogram_Regexps := Subprogram_Regexps_Value;
      else
         declare
            Rec : Silent_Exception_Handlers_Alias renames
              Silent_Exception_Handlers_Alias (Alias.all);
         begin
            Rec.Alias_State        := State_Value;
            Rec.Subprograms        := Subprograms_Value;
            Rec.Subprogram_Regexps := Subprogram_Regexps_Value;
         end;
      end if;
   end Process_Rule_Parameter;

   overriding procedure Process_Rule_Parameter
     (Rule       : in out Custom_Rule;
      Alias      : Alias_Access;
      Param      : String;
      Enable     : Boolean;
      Defined_At : String)
   is
      State_Value     : Rule_States;
      Arguments_Value : Rule_Argument_Vectors.Vector;
   begin
      --  Initialize rule and alias common values
      if Alias = null then
         State_Value     := Rule.Rule_State;
         Arguments_Value := Rule.Arguments;
      else
         State_Value     := Alias.Alias_State;
         Arguments_Value := Custom_Alias (Alias.all).Arguments;
      end if;

      --  Process the actual parameter
      if Param = "" then
         State_Value := (if Enable then Enabled else Disabled);
      elsif Enable then
         Rule.Defined_At := To_Unbounded_String (Defined_At);

         declare
            First_Equal : constant Natural := Index (Param, "=");
         begin
            if First_Equal = 0 then
               Error ("(" & To_String (Rule.Name) &
                      ") missing = in parameter argument: " & Param);
               Bad_Rule_Detected := True;
               State_Value       := Disabled;
               return;
            end if;

            declare
               Param_Name : constant String :=
                 To_Lower (Param (Param'First .. First_Equal - 1));
               Found       : Boolean := False;
            begin
               --  Check that the parameter name is valid

               for J in 1 .. Rule.Parameters.Last_Child_Index loop
                  if To_String (Rule.Parameters.Child (J).As_Parameter_Decl.
                                F_Param_Identifier.Text) = Param_Name
                  then
                     Found := True;
                     exit;
                  end if;
               end loop;

               if Found then
                  Arguments_Value.Append (Rule_Argument'
                    (Name  => To_Unbounded_Text
                                (To_Text (Param_Name)),
                     Value => To_Unbounded_Text
                                (To_Text
                                  (Param (First_Equal + 1 .. Param'Last)))));
                  State_Value := Enabled;

               else
                  Error ("(" & To_String (Rule.Name) &
                         ") unknown parameter: " & Param_Name);
                  Bad_Rule_Detected := True;
                  State_Value       := Disabled;
               end if;
            end;
         end;
      else
         Error ("(" & To_String (Rule.Name) & ") no parameter allowed for -R");
         Bad_Rule_Detected := True;
         State_Value       := Disabled;
         Rule.Defined_At   := To_Unbounded_String (Defined_At);
      end if;

      --  Place the computed parameter values in the rule or alias
      if Alias = null then
         Rule.Rule_State := State_Value;
         Rule.Arguments  := Arguments_Value;
      else
         Alias.Alias_State                  := State_Value;
         Custom_Alias (Alias.all).Arguments := Arguments_Value;
      end if;
   end Process_Rule_Parameter;

   --------------------------------
   -- Process_Rule_Params_Object --
   --------------------------------

   overriding procedure Process_Rule_Params_Object
     (Rule          : in out One_Integer_Parameter_Rule;
      Params_Object : in out JSON_Value)
   is
      Param_Name : constant String := Rule.Parameter_Name (2);
   begin
      Rule.Param := Expect_Literal (Params_Object, Param_Name);
      Params_Object.Unset_Field (Param_Name);
   end Process_Rule_Params_Object;

   overriding procedure Process_Rule_Params_Object
     (Rule          : in out One_Boolean_Parameter_Rule;
      Params_Object : in out JSON_Value)
   is
      Param_Name : constant String := Rule.Parameter_Name (2);
   begin
      if Params_Object.Has_Field (Param_Name) then
         Rule.Param :=
           From_Boolean (Expect_Literal (Params_Object, Param_Name));
         Params_Object.Unset_Field (Param_Name);
      end if;
   end Process_Rule_Params_Object;

   overriding procedure Process_Rule_Params_Object
     (Rule          : in out One_String_Parameter_Rule;
      Params_Object : in out JSON_Value)
   is
      Param_Name : constant String := Rule.Parameter_Name (2);
   begin
      --  Handle the "headers" rule in a special way since the argument should
      --  be a file name.
      if Rule.Name = "headers" then
         Rule.Load_File
           (To_Load => Expect_Literal (Params_Object, Param_Name),
            File_Name => Rule.File,
            File_Content => Rule.Param,
            State => Rule.Rule_State);
         Params_Object.Unset_Field (Param_Name);

      --  Else, handle the parameter as a simple string
      else
         Set_Unbounded_Wide_Wide_String
            (Rule.Param,
            To_Wide_Wide_String
               (Expect_Literal (Params_Object, Param_Name)));
         Params_Object.Unset_Field (Param_Name);
      end if;
   end Process_Rule_Params_Object;

   overriding procedure Process_Rule_Params_Object
     (Rule          : in out One_Array_Parameter_Rule;
      Params_Object : in out JSON_Value)
   is
      Param_Name : constant String := Rule.Parameter_Name (2);
   begin
      --  If the rule is "name_clashes", then load the provided file a
      --  dictionary file.
      if Rule.Name = "name_clashes" then
         declare
            Param_Value : constant String :=
            Expect_Literal (Params_Object, "dictionary_file");
         begin
            Set_Unbounded_String (Rule.File, Param_Value);
            Load_Dictionary
              (Expand_Env_Variables (Param_Value),
               Rule_Name (Rule),
               Rule.Rule_State,
               Rule.Param);
            Params_Object.Unset_Field ("dictionary_file");
         end;
      end if;

      --  Else, handle the real array parametrized rules
      if Params_Object.Has_Field (Param_Name) then
         declare
            Param_Value : constant String_Vector :=
               Expect_Literal (Params_Object, Param_Name);
            Res : String_Vector;
         begin

            --  Special case for the "parameters_out_of_order" rule to verify
            --  that the argument value contains valid values.
            if Rule.Name = "parameters_out_of_order" then
               for S of Param_Value loop
                  if To_Lower (S) not in
                    "in" | "defaulted_in" | "in_out" | "access" | "out"
                  then
                     raise Invalid_Value with
                       "'" & Param_Name & "' should contains only 'in', " &
                       "'defaulted_in', 'in_out', 'access' or 'out' strings";
                  end if;
               end loop;

            --  Special case for the "actual_parameters" rule which should
            --  have a list of three-elem tuples of strings.
            elsif Rule.Name = "actual_parameters" then
               begin
                  for S of Param_Value loop
                     Res.Append (Join (Parse_String_Tuple (S), ":"));
                  end loop;
               exception
                  when Invalid_Type =>
                     raise Invalid_Type with
                       "'" & Param_Name & "' should be a list of string " &
                       "tuples";
               end;

            --  Special case for the "exception_propagation_from_callbacks"
            --  rule which should have a list of two-elems tuples of strings.
            elsif Rule.Name = "exception_propagation_from_callbacks"
            then
               begin
                  for S of Param_Value loop
                     Res.Append (Join (Parse_String_Tuple (S), "."));
                  end loop;
               exception
                  when Invalid_Type =>
                     raise Invalid_Type with
                       "'" & Param_Name & "' should be a list of string " &
                       "tuples";
               end;

            --  Else the rule parameter is just a list of strings
            else
               Res := Param_Value;
            end if;
            Set_Unbounded_Wide_Wide_String
              (Rule.Param, To_Wide_Wide_String (Join (Res, ",")));
         end;
         Params_Object.Unset_Field (Param_Name);
      end if;
   end Process_Rule_Params_Object;

   overriding procedure Process_Rule_Params_Object
     (Rule          : in out One_Integer_Or_Booleans_Parameter_Rule;
      Params_Object : in out JSON_Value) is
   begin
      --  Iterate over all rules parameters and try getting it from the
      --  arguments object.
      for I in 2 .. Rule.Parameters.Last_Child_Index loop
         if Params_Object.Has_Field (Rule.Parameter_Name (I)) then
            --  Try getting the parameter as an integer
            begin
               Rule.Integer_Param :=
                 Expect_Literal (Params_Object, Rule.Parameter_Name (I));

            --  If it fails, then the argument should be a boolean
            exception
               when Invalid_Type =>
                  Rule.Boolean_Params (I) :=
                    From_Boolean
                      (Expect_Literal
                         (Params_Object, Rule.Parameter_Name (I)));
            end;
            Params_Object.Unset_Field (Rule.Parameter_Name (I));
         end if;
      end loop;
   end Process_Rule_Params_Object;

   overriding procedure Process_Rule_Params_Object
     (Rule          : in out Identifier_Suffixes_Rule;
      Params_Object : in out JSON_Value) is
   begin
      --  Process the "default" boolean parameter
      if Params_Object.Has_Field ("default") then
         if Expect_Literal (Params_Object, "default") then
            Set_Unbounded_Wide_Wide_String (Rule.Type_Suffix, "_T");
            Set_Unbounded_Wide_Wide_String (Rule.Access_Suffix, "_A");
            Set_Unbounded_Wide_Wide_String (Rule.Access_Access_Suffix, "");
            Set_Unbounded_Wide_Wide_String (Rule.Class_Access_Suffix, "");
            Set_Unbounded_Wide_Wide_String (Rule.Class_Subtype_Suffix, "");
            Set_Unbounded_Wide_Wide_String (Rule.Constant_Suffix, "_C");
            Set_Unbounded_Wide_Wide_String (Rule.Renaming_Suffix, "_R");
            Set_Unbounded_Wide_Wide_String (Rule.Access_Obj_Suffix, "");
            Set_Unbounded_Wide_Wide_String (Rule.Interrupt_Suffix, "");
         end if;
         Params_Object.Unset_Field ("default");
      end if;

      --  Process the "access_suffix" special string argument
      if Params_Object.Has_Field ("access_suffix") then
         declare
            Arg : constant String :=
              Expect_Literal (Params_Object, "access_suffix");
            Paren_Index : Natural;
         begin
            if Has_Suffix (Arg, ")") then
               Paren_Index := Index (Arg, "(");
               Set_Unbounded_Wide_Wide_String
                 (Rule.Access_Suffix,
                  To_Wide_Wide_String (Arg (Arg'First .. Paren_Index - 1)));
               Set_Unbounded_Wide_Wide_String
                 (Rule.Access_Access_Suffix,
                  To_Wide_Wide_String (Arg (Paren_Index + 1 .. Arg'Last - 1)));
            else
               Set_Unbounded_Wide_Wide_String
                 (Rule.Access_Suffix, To_Wide_Wide_String (Arg));
            end if;
         end;
         Params_Object.Unset_Field ("access_suffix");
      end if;

      --  Then process the other arguments
      Process_String_Arg (Params_Object, "type_suffix", Rule.Type_Suffix);
      Process_String_Arg
        (Params_Object, "class_access_suffix", Rule.Class_Access_Suffix);
      Process_String_Arg
        (Params_Object, "class_subtype_suffix", Rule.Class_Subtype_Suffix);
      Process_String_Arg
        (Params_Object, "constant_suffix", Rule.Constant_Suffix);
      Process_String_Arg
        (Params_Object, "renaming_suffix", Rule.Renaming_Suffix);
      Process_String_Arg
        (Params_Object, "access_obj_suffix", Rule.Access_Obj_Suffix);
      Process_String_Arg
        (Params_Object, "interrupt_suffix", Rule.Interrupt_Suffix);
   end Process_Rule_Params_Object;

   overriding procedure Process_Rule_Params_Object
     (Rule          : in out Identifier_Prefixes_Rule;
      Params_Object : in out JSON_Value)
   is
      Derived_Param : String_Vector;
      Col_Index : Natural;
   begin
      --  Process the exclusive boolean argument
      if Params_Object.Has_Field ("exclusive") then
         Rule.Exclusive :=
           From_Boolean (Expect_Literal (Params_Object, "exclusive"));
         Params_Object.Unset_Field ("exclusive");
      end if;

      --  Process the "derived" argument
      if Params_Object.Has_Field ("derived") then
         Derived_Param := Expect_Literal (Params_Object, "derived");
         for S of Derived_Param loop
            if Length (Rule.Derived_Prefix) /= 0 then
               Append (Rule.Derived_Prefix, ",");
            end if;
            Col_Index := Index (S, ":");
            if Col_Index /= 0 then
               Append
                 (Rule.Derived_Prefix,
                  To_Wide_Wide_String
                    (To_Lower (S (S'First .. Col_Index - 1))));
               Append
                 (Rule.Derived_Prefix,
                  To_Wide_Wide_String (S (Col_Index .. S'Last)));
            else
               raise Invalid_Value with
                 "'derived' elements should contain a colon";
            end if;
         end loop;
         Params_Object.Unset_Field ("derived");
      end if;

      --  The process the other arguments
      Process_String_Arg (Params_Object, "type", Rule.Type_Prefix);
      Process_String_Arg (Params_Object, "concurrent", Rule.Concurrent_Prefix);
      Process_String_Arg (Params_Object, "access", Rule.Access_Prefix);
      Process_String_Arg
        (Params_Object, "class_access", Rule.Class_Access_Prefix);
      Process_String_Arg
        (Params_Object, "subprogram_access", Rule.Subprogram_Access_Prefix);
      Process_String_Arg (Params_Object, "constant", Rule.Constant_Prefix);
      Process_String_Arg (Params_Object, "exception", Rule.Exception_Prefix);
      Process_String_Arg (Params_Object, "enum", Rule.Enum_Prefix);
   end Process_Rule_Params_Object;

   overriding procedure Process_Rule_Params_Object
     (Rule          : in out Identifier_Casing_Rule;
      Params_Object : in out JSON_Value) is
   begin
      --  Process the "exclude" argument
      if Params_Object.Has_Field ("exclude") then
         Load_Dictionary
           (Expand_Env_Variables (Expect_Literal (Params_Object, "exclude")),
            Rule_Name (Rule),
            Rule.Rule_State,
            Rule.Exclude);
         Params_Object.Unset_Field ("exclude");
      end if;

      --  Then process the other arguments
      Process_String_Arg
        (Params_Object, "type", Rule.Type_Casing, Normalize => True);
      Process_String_Arg
        (Params_Object, "enum", Rule.Enum_Casing, Normalize => True);
      Process_String_Arg
        (Params_Object, "constant", Rule.Constant_Casing, Normalize => True);
      Process_String_Arg
        (Params_Object, "exception", Rule.Exception_Casing, Normalize => True);
      Process_String_Arg
        (Params_Object, "others", Rule.Others_Casing, Normalize => True);
   end Process_Rule_Params_Object;

   overriding procedure Process_Rule_Params_Object
     (Rule          : in out Forbidden_Rule;
      Params_Object : in out JSON_Value)
   is
      procedure Process_List_Field
        (Field_Name : String;
         Field : in out Unbounded_Wide_Wide_String);
      --  Get the given `Field_Name` in the arguments object, if presents, get
      --  a string vector from it and add its comma separated items in `Field`.

      ------------------------
      -- Process_List_Field --
      ------------------------

      procedure Process_List_Field
        (Field_Name : String;
         Field : in out Unbounded_Wide_Wide_String)
      is
         Val : String_Vector;
      begin
         if Params_Object.Has_Field (Field_Name) then
            Val := Expect_Literal (Params_Object, Field_Name);
            for S of Val loop
               declare
                  Lower_S : constant String := To_Lower (S);
               begin
                  if Length (Field) > 0 then
                     Append (Field, ",");
                  end if;

                  --  Special cases when the current value is "gnat"
                  if Lower_S = "gnat" then
                     if Rule.Name = "forbidden_attributes" then
                        Append (Field, GNAT_Attributes);
                     elsif Rule.Name = "forbidden_pragmas" then
                        Append (Field, GNAT_Pragmas);
                     else
                        Append (Field, To_Wide_Wide_String (Lower_S));
                     end if;
                  else
                     Append (Field, To_Wide_Wide_String (Lower_S));
                  end if;
               end;
            end loop;
            Params_Object.Unset_Field (Field_Name);
         end if;
      end Process_List_Field;
   begin
      --  Process the "all" boolean argument
      if Params_Object.Has_Field ("all") then
         Rule.All_Flag := From_Boolean (Expect_Literal (Params_Object, "all"));
         Params_Object.Unset_Field ("all");
      end if;

      --  Process the "forbidden" and "allowed" list argument
      Process_List_Field ("forbidden", Rule.Forbidden);
      Process_List_Field ("allowed", Rule.Allowed);
   end Process_Rule_Params_Object;

   overriding procedure Process_Rule_Params_Object
     (Rule          : in out Silent_Exception_Handlers_Rule;
      Params_Object : in out JSON_Value)
   is
      Subp_Value : String_Vector;
   begin
      --  Process the "subprograms" list argument
      if Params_Object.Has_Field ("subprograms") then
         Subp_Value := Expect_Literal (Params_Object, "subprograms");
         for S of Subp_Value loop
            if S (S'First) = '|' then
               if Length (Rule.Subprogram_Regexps) /= 0 then
                  Append (Rule.Subprogram_Regexps, ",");
               end if;
               Append
                 (Rule.Subprogram_Regexps,
                  To_Wide_Wide_String (S (S'First + 1 .. S'Last)));
            else
               if Length (Rule.Subprograms) /= 0 then
                  Append (Rule.Subprograms, ",");
               end if;
               Append (Rule.Subprograms, To_Wide_Wide_String (To_Lower (S)));
            end if;
         end loop;
         Params_Object.Unset_Field ("subprograms");
      end if;
   end Process_Rule_Params_Object;

   overriding procedure Process_Rule_Params_Object
     (Rule          : in out Custom_Rule;
      Params_Object : in out JSON_Value) is
   begin
      for I in 2 .. Rule.Parameters.Last_Child_Index loop
         declare
            Param_Name : constant String := Rule.Parameter_Name (I);
         begin
            if Params_Object.Has_Field (Param_Name) then
               Rule.Arguments.Append
                 (Rule_Argument'
                   (Name => To_Unbounded_Text (To_Text (Param_Name)),
                    Value => To_Unbounded_Text
                               (To_Text
                                  (Expect (Params_Object, Param_Name)))));
               Params_Object.Unset_Field (Param_Name);
            end if;
         end;
      end loop;
   end Process_Rule_Params_Object;

   --------------------
   -- Map_Parameters --
   --------------------

   overriding procedure Map_Parameters
     (Rule : in out One_Integer_Parameter_Rule;
      Args : in out Rule_Argument_Vectors.Vector) is
   begin
      Append_Int_Param (Args, Param_Name (Rule, 2), Rule.Param);
   end Map_Parameters;

   overriding procedure Map_Parameters
     (Alias : in out One_Integer_Parameter_Alias;
      Args  : in out Rule_Argument_Vectors.Vector) is
   begin
      Append_Int_Param (Args, Param_Name (Get_Rule (Alias), 2), Alias.Param);
   end Map_Parameters;

   overriding procedure Map_Parameters
     (Rule : in out One_Boolean_Parameter_Rule;
      Args : in out Rule_Argument_Vectors.Vector) is
   begin
      Append_Bool_Param (Args, Param_Name (Rule, 2), Rule.Param);
   end Map_Parameters;

   overriding procedure Map_Parameters
     (Alias : in out One_Boolean_Parameter_Alias;
      Args  : in out Rule_Argument_Vectors.Vector) is
   begin
      Append_Bool_Param (Args, Param_Name (Get_Rule (Alias), 2), Alias.Param);
   end Map_Parameters;

   overriding procedure Map_Parameters
     (Rule : in out One_String_Parameter_Rule;
      Args : in out Rule_Argument_Vectors.Vector) is
   begin
      Append_String_Param (Args, Param_Name (Rule, 2), Rule.Param);
   end Map_Parameters;

   overriding procedure Map_Parameters
     (Alias : in out One_String_Parameter_Alias;
      Args  : in out Rule_Argument_Vectors.Vector)
   is
   begin
      Append_String_Param
        (Args, Param_Name (Get_Rule (Alias), 2), Alias.Param);
   end Map_Parameters;

   overriding procedure Map_Parameters
     (Rule : in out One_Integer_Or_Booleans_Parameter_Rule;
      Args : in out Rule_Argument_Vectors.Vector) is
   begin
      Append_Int_Param (Args, Param_Name (Rule, 2), Rule.Integer_Param);
      for J in 2 .. Rule.Parameters.Last_Child_Index loop
         Append_Bool_Param
           (Args, Param_Name (Rule, J), Rule.Boolean_Params (J));
      end loop;
   end Map_Parameters;

   overriding procedure Map_Parameters
     (Alias : in out One_Integer_Or_Booleans_Parameter_Alias;
      Args  : in out Rule_Argument_Vectors.Vector)
   is
      Rule : constant Rule_Template'Class := Get_Rule (Alias);
   begin
      Append_Int_Param (Args, Param_Name (Rule, 2), Alias.Integer_Param);
      for J in 2 .. Rule.Parameters.Last_Child_Index loop
         Append_Bool_Param
           (Args, Param_Name (Rule, J), Alias.Boolean_Params (J));
      end loop;
   end Map_Parameters;

   overriding procedure Map_Parameters
     (Rule : in out One_Array_Parameter_Rule;
      Args : in out Rule_Argument_Vectors.Vector) is
   begin
      Handle_Array_Param (Args, Rule, Rule.Param);
   end Map_Parameters;

   overriding procedure Map_Parameters
     (Alias : in out One_Array_Parameter_Alias;
      Args  : in out Rule_Argument_Vectors.Vector)
   is
      Rule : One_Array_Parameter_Rule :=
        One_Array_Parameter_Rule (Get_Rule (Alias));
   begin
      Handle_Array_Param (Args, Rule, Alias.Param);
   end Map_Parameters;

   overriding procedure Map_Parameters
     (Rule : in out Identifier_Suffixes_Rule;
      Args : in out Rule_Argument_Vectors.Vector) is
   begin
      Append_String_Param (Args, "type_suffix", Rule.Type_Suffix);
      Append_String_Param (Args, "access_suffix", Rule.Access_Suffix);
      Append_String_Param
        (Args, "access_access_suffix", Rule.Access_Access_Suffix);
      Append_String_Param
        (Args, "class_access_suffix", Rule.Class_Access_Suffix);
      Append_String_Param
        (Args, "class_subtype_suffix", Rule.Class_Subtype_Suffix);
      Append_String_Param
        (Args, "constant_suffix", Rule.Constant_Suffix);
      Append_String_Param
        (Args, "renaming_suffix", Rule.Renaming_Suffix);
      Append_String_Param
        (Args, "access_obj_suffix", Rule.Access_Obj_Suffix);
      Append_String_Param
        (Args, "interrupt_suffix", Rule.Interrupt_Suffix);
   end Map_Parameters;

   overriding procedure Map_Parameters
     (Alias : in out Identifier_Suffixes_Alias;
      Args  : in out Rule_Argument_Vectors.Vector) is
   begin
      Append_String_Param (Args, "type_suffix", Alias.Type_Suffix);
      Append_String_Param (Args, "access_suffix", Alias.Access_Suffix);
      Append_String_Param
        (Args, "access_access_suffix", Alias.Access_Access_Suffix);
      Append_String_Param
        (Args, "class_access_suffix", Alias.Class_Access_Suffix);
      Append_String_Param
        (Args, "class_subtype_suffix", Alias.Class_Subtype_Suffix);
      Append_String_Param
        (Args, "constant_suffix", Alias.Constant_Suffix);
      Append_String_Param
        (Args, "renaming_suffix", Alias.Renaming_Suffix);
      Append_String_Param
        (Args, "access_obj_suffix", Alias.Access_Obj_Suffix);
      Append_String_Param
        (Args, "interrupt_suffix", Alias.Interrupt_Suffix);
   end Map_Parameters;

   overriding procedure Map_Parameters
     (Rule : in out Identifier_Prefixes_Rule;
      Args : in out Rule_Argument_Vectors.Vector) is
   begin
      Append_String_Param (Args, "type", Rule.Type_Prefix);
      Append_String_Param (Args, "concurrent", Rule.Concurrent_Prefix);
      Append_String_Param (Args, "access", Rule.Access_Prefix);
      Append_String_Param
        (Args, "class_access", Rule.Class_Access_Prefix);
      Append_String_Param
        (Args, "subprogram_access", Rule.Subprogram_Access_Prefix);
      Append_String_Param (Args, "constant", Rule.Constant_Prefix);
      Append_String_Param (Args, "exception", Rule.Exception_Prefix);
      Append_String_Param (Args, "enum", Rule.Enum_Prefix);
      Append_Array_Param (Args, "derived", Rule.Derived_Prefix);

      if Rule.Exclusive = Off then
         Args.Append
           (Rule_Argument'
             (Name  => To_Unbounded_Text ("exclusive"),
              Value => To_Unbounded_Text ("false")));
      end if;
   end Map_Parameters;

   overriding procedure Map_Parameters
     (Alias : in out Identifier_Prefixes_Alias;
      Args  : in out Rule_Argument_Vectors.Vector) is
   begin
      Append_String_Param (Args, "type", Alias.Type_Prefix);
      Append_String_Param (Args, "concurrent", Alias.Concurrent_Prefix);
      Append_String_Param (Args, "access", Alias.Access_Prefix);
      Append_String_Param
        (Args, "class_access", Alias.Class_Access_Prefix);
      Append_String_Param
        (Args, "subprogram_access", Alias.Subprogram_Access_Prefix);
      Append_String_Param (Args, "constant", Alias.Constant_Prefix);
      Append_String_Param (Args, "exception", Alias.Exception_Prefix);
      Append_String_Param (Args, "enum", Alias.Enum_Prefix);
      Append_Array_Param (Args, "derived", Alias.Derived_Prefix);

      if Alias.Exclusive = Off then
         Args.Append
           (Rule_Argument'
             (Name  => To_Unbounded_Text ("exclusive"),
              Value => To_Unbounded_Text ("false")));
      end if;
   end Map_Parameters;

   overriding procedure Map_Parameters
     (Rule : in out Identifier_Casing_Rule;
      Args : in out Rule_Argument_Vectors.Vector) is
   begin
      Append_String_Param (Args, "type", Rule.Type_Casing);
      Append_String_Param (Args, "enum", Rule.Enum_Casing);
      Append_String_Param (Args, "constant", Rule.Constant_Casing);
      Append_String_Param (Args, "exception", Rule.Exception_Casing);
      Append_String_Param (Args, "others", Rule.Others_Casing);
      Append_Array_Param (Args, "exclude", Rule.Exclude);
   end Map_Parameters;

   overriding procedure Map_Parameters
     (Alias : in out Identifier_Casing_Alias;
      Args  : in out Rule_Argument_Vectors.Vector) is
   begin
      Append_String_Param (Args, "type", Alias.Type_Casing);
      Append_String_Param (Args, "enum", Alias.Enum_Casing);
      Append_String_Param (Args, "constant", Alias.Constant_Casing);
      Append_String_Param (Args, "exception", Alias.Exception_Casing);
      Append_String_Param (Args, "others", Alias.Others_Casing);
      Append_Array_Param (Args, "exclude", Alias.Exclude);
   end Map_Parameters;

   overriding procedure Map_Parameters
     (Rule : in out Forbidden_Rule;
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

   overriding procedure Map_Parameters
     (Alias : in out Forbidden_Alias;
      Args  : in out Rule_Argument_Vectors.Vector) is
   begin
      if Alias.All_Flag = On then
         Args.Append
           (Rule_Argument'
             (Name  => To_Unbounded_Text ("all"),
              Value => To_Unbounded_Text ("true")));
      end if;

      Append_Array_Param (Args, "forbidden", Alias.Forbidden);
      Append_Array_Param (Args, "allowed", Alias.Allowed);
   end Map_Parameters;

   overriding procedure Map_Parameters
     (Rule : in out Silent_Exception_Handlers_Rule;
      Args : in out Rule_Argument_Vectors.Vector) is
   begin
      Append_Array_Param (Args, "subprograms", Rule.Subprograms);
      Append_Array_Param (Args, "subprogram_regexps", Rule.Subprogram_Regexps);
   end Map_Parameters;

   overriding procedure Map_Parameters
     (Alias : in out Silent_Exception_Handlers_Alias;
      Args  : in out Rule_Argument_Vectors.Vector) is
   begin
      Append_Array_Param (Args, "subprograms", Alias.Subprograms);
      Append_Array_Param
        (Args, "subprogram_regexps", Alias.Subprogram_Regexps);
   end Map_Parameters;

   overriding procedure Map_Parameters
     (Rule : in out Custom_Rule;
      Args : in out Rule_Argument_Vectors.Vector) is
   begin
      for Arg of Rule.Arguments loop
         Args.Append (Arg);
      end loop;
   end Map_Parameters;

   overriding procedure Map_Parameters
     (Alias : in out Custom_Alias;
      Args  : in out Rule_Argument_Vectors.Vector) is
   begin
      for Arg of Alias.Arguments loop
         Args.Append (Arg);
      end loop;
   end Map_Parameters;

   ---------------
   -- Rule_Name --
   ---------------

   function Rule_Name (Rule : Rule_Template) return String is
   begin
      return To_String (Rule.Name);
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
      First_Idx :  Natural := Index (Diag, " ", Going => Backward) + 1;
      Last_Idx  :  Natural := Diag'Last;
   begin

      if Mapping_Mode then
         --  The diagnosis has the following format:
         --
         --     foo.adb:nn:mm: use of pragma Bar [Rule_Name]

         Last_Idx  := First_Idx - 2;
         First_Idx := Index (Diag (Diag'First ..  Last_Idx),
                             " ",
                             Going => Backward) + 1;
      end if;

      return To_Lower (Diag (First_Idx .. Last_Idx));
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
        or else Index (Diag, "protected") /= 0
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

   --------------------
   -- Parameter_Name --
   --------------------

   function Parameter_Name
     (Rule : Rule_Template'Class;
      Param_Index : Integer) return String
   is
      Param_Node : constant Liblkqllang.Analysis.Lkql_Node :=
        Rule.Parameters.Child (Param_Index);
   begin
      if not Param_Node.Is_Null then
         return To_String
           (Param_Node.As_Parameter_Decl.F_Param_Identifier.Text);
      else
         raise Constraint_Error with "Parameter index out of bounds";
      end if;
   end Parameter_Name;

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
                          As_Parameter_Decl.F_Param_Identifier.Text) &
               "</parameter>",
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

      if Length (Rule.Access_Suffix) /= 0 then
         XML_Report_No_EOL
           ("<parameter>Access_Suffix=" & To_String (Rule.Access_Suffix),
            Indent_Level + 1);

         if Length (Rule.Access_Access_Suffix) /= 0 then
            XML_Report_No_EOL ("(" & To_String (Rule.Access_Access_Suffix) &
                                 ")");
         end if;

         XML_Report ("</parameter>");
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
               XML_Report ("</parameter>");
               XML_Report_No_EOL ("<parameter>Exclude=", Indent_Level + 1);
            end if;
         end loop;

         XML_Report ("</parameter>");
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
               XML_Report ("</parameter>");

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

         if Length (Rule.Forbidden) /= 0 then
            XML_Report ("</parameter>");
         end if;
      end if;

      XML_Print (Rule.Allowed, False);

      if Length (Rule.Allowed) /= 0 then
         XML_Report ("</parameter>");
      end if;

      XML_Report ("</rule>", Indent_Level);
   end XML_Print_Rule;

   overriding procedure XML_Print_Rule
     (Rule         : Silent_Exception_Handlers_Rule;
      Indent_Level : Natural := 0)
   is
      procedure XML_Print
        (Items  : Unbounded_Wide_Wide_String;
         Quote : Boolean);
      --  Print Items if not empty. If Quote is True, these items are quoted.

      ---------------
      -- XML_Print --
      ---------------

      procedure XML_Print
        (Items  : Unbounded_Wide_Wide_String;
         Quote : Boolean)
      is
         C   : Character;
         Str : constant String := (if Quote then """" else "");
      begin
         if Length (Items) = 0 then
            return;
         end if;

         XML_Report_No_EOL ("<parameter>" & Str, Indent_Level + 1);

         for J in 1 .. Length (Items) loop
            C := To_Character (Element (Items, J));

            if C = ',' then
               XML_Report (Str & "</parameter>");
               XML_Report_No_EOL ("<parameter>" & Str, Indent_Level + 1);
            else
               XML_Report_No_EOL ([C]);
            end if;
         end loop;

         XML_Report (Str & "</parameter>");
      end XML_Print;

   begin
      XML_Report
        ("<rule id=""" & Rule_Name (Rule) & """>",
         Indent_Level);

      XML_Print (Rule.Subprograms, False);
      XML_Print (Rule.Subprogram_Regexps, True);
      XML_Report ("</rule>", Indent_Level);
   end XML_Print_Rule;

   overriding procedure XML_Print_Rule
     (Rule         : Custom_Rule;
      Indent_Level : Natural := 0) is
   begin
      XML_Report ("<rule id=""" & Rule_Name (Rule) & """>", Indent_Level);

      --  ??? Need to escape Arg.Value

      for Arg of Rule.Arguments loop
         XML_Report
           ("<parameter>" &
            To_String (To_Wide_Wide_String (Arg.Name)) & "=" &
            To_String (To_Wide_Wide_String (Arg.Value)) &
            "</parameter>", Indent_Level + 1);
      end loop;

      XML_Report ("</rule>", Indent_Level);
   end XML_Print_Rule;

   -------------------
   -- XML_Rule_Help --
   -------------------

   procedure XML_Rule_Help (Rule : Rule_Template; Level : Natural) is
   begin
      Info_No_EOL (Level * Indent_String      &
                   "<check switch=""+R"       &
                   To_String (Rule.Name)      &
                   """ label="""              &
                   To_String (Rule.Help_Info) &
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
      Info (Level * Indent_String      &
            "<spin switch=""+R"        &
            To_String (Rule.Name)      &
            """ label="""              &
            To_String (Rule.Help_Info) &
            """ min=""0"""             &
            " max=""99999"""           &
            " default=""-1"""          &
            " separator="":"""         &
            "/>");
   end XML_Rule_Help;

   overriding procedure XML_Rule_Help
     (Rule  : One_Boolean_Parameter_Rule;
      Level : Natural) is
   begin
      Info (Level * Indent_String      &
            "<field switch=""+R"       &
            To_String (Rule.Name)      &
            """ separator="":"""       &
            " label="""                &
            To_String (Rule.Help_Info) &
            """/>");
      Info (Level * Indent_String       &
            "<check switch=""+R"        &
            To_String (Rule.Name) & ":" &
            To_String (Rule.Parameters.Child (2).As_Parameter_Decl.
                       F_Param_Identifier.Text) &
            """ label="""               &
            To_String (Rule.Help_Info)  &
            """/>");
   end XML_Rule_Help;

   overriding procedure XML_Rule_Help
     (Rule  : One_String_Parameter_Rule;
      Level : Natural) is
   begin
      Info (Level * Indent_String      &
            "<field switch=""+R"       &
            To_String (Rule.Name)      &
            """ separator="":"""       &
            " label="""                &
            To_String (Rule.Help_Info) &
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
               To_String (Rule.Name)              &
               ":" & Param & '"'                  &
               " label="""                        &
               "prefix for " & Help               &
               " (empty string disables check)""" &
               " separator=""="""                 &
               " switch-off=""-R"                 &
               To_String (Rule.Name)              &
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
            To_String (Rule.Name) &
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
               To_String (Rule.Name)              &
               ":" & Param & '"'                  &
               " label="""                        &
               "suffix for " & Help & " names"    &
               " (empty string disables check)""" &
               " separator=""="""                 &
               " switch-off=""-R"                 &
               To_String (Rule.Name)              &
               ":" & Param & '"'                  &
               "/>");
      end Print;

   begin
      Info (Level * Indent_String                 &
            "<check switch=""+R"                  &
            To_String (Rule.Name)                 &
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
            To_String (Rule.Name)                         &
            ":Default"""                                  &
            " slave-switch=""+R"                          &
            To_String (Rule.Name)                         &
            ":Type_Suffix=_T""/>");
      Info (Level * Indent_String                         &
           "<default-value-dependency master-switch=""+R" &
            To_String (Rule.Name)                         &
            ":Default"""                                  &
            " slave-switch=""+R"                          &
            To_String (Rule.Name)                         &
            ":Access_Suffix=_A""/>");
      Info (Level * Indent_String                         &
           "<default-value-dependency master-switch=""+R" &
            To_String (Rule.Name)                         &
            ":Default"""                                  &
            " slave-switch=""+R"                          &
            To_String (Rule.Name)                         &
            ":Constant_Suffix=_C""/>");
      Info (Level * Indent_String                         &
           "<default-value-dependency master-switch=""+R" &
            To_String (Rule.Name)                         &
            ":Default"""                                  &
            " slave-switch=""+R"                          &
            To_String (Rule.Name)                         &
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
            To_String (Rule.Name)               &
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
      Name : constant String := To_String (Rule.Name);
      Str  : constant String := Name (11 .. Name'Last);
      --  Strip the leading "forbidden_"

   begin
      Info (Level * Indent_String &
            "<check switch=""+R" &
            To_String (Rule.Name) &
            ":ALL"""              &
            " label="""           &
            "detect all " & Str   &
            " except explicitly disabled""/>");

      Info (Level * Indent_String &
            "<check switch=""+R"  &
            To_String (Rule.Name) &
            ":GNAT"""             &
            " label="""           &
            "detect all GNAT " & Str & " except explicitly disabled""/>");

      Info (Level * Indent_String &
            "<field switch=""+R"  &
            To_String (Rule.Name) &
            """ label="""         &
            "detect specified "   &
            Str & " (use ',' as separator)""" &
            " separator="":""/>");

      Info (Level * Indent_String &
            "<field switch=""-R"  &
            To_String (Rule.Name) &
            """ label="""         &
            "do not detect specified " &
            Str & " (use ',' as separator)""" &
            " separator="":""/>");
   end XML_Rule_Help;

   procedure XML_Rule_Help
     (Rule  : Silent_Exception_Handlers_Rule;
      Level : Natural) is
   begin
      Info (Level * Indent_String      &
            "<field switch=""+R"       &
            To_String (Rule.Name)      &
            """ separator="":"""       &
            " label="""                &
            To_String (Rule.Help_Info) &
            """/>");
   end XML_Rule_Help;

   procedure XML_Rule_Help
     (Rule  : Custom_Rule;
      Level : Natural) is
   begin
      --  Should we do more here???
      XML_Rule_Help (Rule_Template (Rule), Level);
   end XML_Rule_Help;

   ------------------
   -- Create_Alias --
   ------------------

   function Create_Alias (Rule : Rule_Template) return Alias_Access is
   begin
      return new Alias_Template;
   end Create_Alias;

   overriding function Create_Alias
     (Rule : One_Integer_Parameter_Rule) return Alias_Access is
   begin
      return new One_Integer_Parameter_Alias;
   end Create_Alias;

   overriding function Create_Alias
     (Rule : One_Boolean_Parameter_Rule) return Alias_Access is
   begin
      return new One_Boolean_Parameter_Alias;
   end Create_Alias;

   overriding function Create_Alias
     (Rule : One_String_Parameter_Rule) return Alias_Access is
   begin
      return new One_String_Parameter_Alias;
   end Create_Alias;

   overriding function Create_Alias
     (Rule : One_Array_Parameter_Rule) return Alias_Access is
   begin
      return new One_Array_Parameter_Alias;
   end Create_Alias;

   overriding function Create_Alias
     (Rule : One_Integer_Or_Booleans_Parameter_Rule) return Alias_Access is
   begin
      return new One_Integer_Or_Booleans_Parameter_Alias;
   end Create_Alias;

   overriding function Create_Alias
     (Rule : Identifier_Suffixes_Rule) return Alias_Access is
   begin
      return new Identifier_Suffixes_Alias;
   end Create_Alias;

   overriding function Create_Alias
     (Rule : Identifier_Prefixes_Rule) return Alias_Access is
   begin
      return new Identifier_Prefixes_Alias;
   end Create_Alias;

   overriding function Create_Alias
     (Rule : Identifier_Casing_Rule) return Alias_Access is
   begin
      return new Identifier_Casing_Alias;
   end Create_Alias;

   overriding function Create_Alias
     (Rule : Forbidden_Rule) return Alias_Access is
   begin
      return new Forbidden_Alias;
   end Create_Alias;

   overriding function Create_Alias
     (Rule : Silent_Exception_Handlers_Rule) return Alias_Access is
   begin
      return new Silent_Exception_Handlers_Alias;
   end Create_Alias;

   overriding function Create_Alias
     (Rule : Custom_Rule) return Alias_Access is
   begin
      return new Custom_Alias;
   end Create_Alias;

   -----------------------
   -- XML_Rule_Help_Tip --
   -----------------------

   procedure XML_Rule_Help_Tip (Rule : Rule_Template; Level : Natural) is
   begin
      null;
   end XML_Rule_Help_Tip;

   ---------------
   -- Load_File --
   ---------------

   procedure Load_File
     (Rule         : in out One_String_Parameter_Rule;
      To_Load      : String;
      File_Name    : in out Unbounded_String;
      File_Content : in out Unbounded_Wide_Wide_String;
      State        : in out Rule_States)
   is
      Abs_Name : constant String := Find_File (To_Load);
      Str  : GNAT.OS_Lib.String_Access;
      Last : Natural;
   begin
      if Abs_Name /= "" then
         Str := Read_File (Abs_Name);
         Ada.Strings.Unbounded.Set_Unbounded_String (File_Name, Abs_Name);
      else
         Error
           ("(" & To_String (Rule.Name) & "): cannot load file " & To_Load);
         Bad_Rule_Detected := True;
         State := Disabled;
         return;
      end if;

      Last := Str'Last;

      --  If `Last` is null or less, then the file is empty.
      --  Thus don't append anything to the rule parameter.
      if Last > 0 then
         --  Strip trailing end of line
         if Str (Str'Last) = ASCII.LF then
            Last := Last - 1;
         end if;

         Ada.Strings.Wide_Wide_Unbounded.Set_Unbounded_Wide_Wide_String
           (File_Content, To_Wide_Wide_String (Str (1 .. Last)));
         GNAT.OS_Lib.Free (Str);
      end if;
   end Load_File;

end Gnatcheck.Rules;

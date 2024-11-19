--
--  Copyright (C) 2005-2024, AdaCore
--  SPDX-License-Identifier: GPL-3.0-or-later
--

with Ada.Characters.Conversions; use Ada.Characters.Conversions;
with Ada.Strings;                use Ada.Strings;
with Ada.Strings.Fixed;          use Ada.Strings.Fixed;
with Ada.Strings.Maps;

with GNAT.Directory_Operations;  use GNAT.Directory_Operations;
with GNAT.OS_Lib;
with GNAT.String_Split;          use GNAT.String_Split;

with Gnatcheck.Compiler;         use Gnatcheck.Compiler;
with Gnatcheck.JSON_Utilities;   use Gnatcheck.JSON_Utilities;
with Gnatcheck.Options;          use Gnatcheck.Options;
with Gnatcheck.Output;           use Gnatcheck.Output;
with Gnatcheck.Rules.Rule_Table; use Gnatcheck.Rules.Rule_Table;

with Langkit_Support.Text;       use Langkit_Support.Text;

package body Gnatcheck.Rules is

   --  ===== Local subprograms specs =====

   -------------------
   -- Local helpers --
   -------------------

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

   function Load_Dictionary_File (File_Name : String) return Unbounded_String;
   --  Load the `File_Name` file as a dictionary file and return its content
   --  if the file can be loaded. Else, the null unbounded string is returned.

   function To_String (S : Unbounded_Wide_Wide_String) return String
   is (To_String (To_Wide_Wide_String (S)));
   --  Convert an Unbounded_Wide_Wide_String to a String

   function From_Boolean (B : Boolean) return Tri_State is
     (if B then On else Off);
   --  Get the `Tri_State` value corresponding to the given boolean

   function Param_Name
     (Rule : Rule_Info; Index : Positive) return Text_Type is
     (Rule.Parameters.Child
       (Index).As_Parameter_Decl.F_Param_Identifier.Text);
   --  Get the name of the rule parameter at the given index

   function Param_Name
     (Rule : Rule_Info; Index : Positive) return String is
     (To_String (Param_Name (Rule, Index)));
   --  Same as the previous `Param_Name` but returns the result as a string

   function Param_Name
     (Instance : Rule_Instance'Class; Index : Positive) return String is
     (Param_Name (All_Rules (Instance.Rule), Index));

   function Rule_Name (Instance : Rule_Instance_Access) return String is
     (Rule_Name (Instance.all));
   --  Shortcut function to get the rule name from an instance reference

   function Instance_Name (Instance : Rule_Instance_Access) return String is
     (Instance_Name (Instance.all));
   --  Shortcut function to get the instance name from its reference

   procedure Turn_Instance_Off (Instance : Rule_Instance_Access);
   --  Shortcut procedure to turn an instance off from its address

   function XML_Head (Instance : Rule_Instance'Class) return String is
     (if Instance.Is_Alias then
        "<rule alias=""" & Instance_Name (Instance) & """ of=""" &
        Rule_Name (Instance) & """>"
      else
        "<rule id=""" & Rule_Name (Instance) & """>");
   --  Function to get the XML header tag for a rule instance XML display

   function XML_Param (Param : String) return String is
     ("<parameter>" & Param & "</parameter>");
   --  Function to get a parameter XML tag with `Parm` in it

   procedure Print_XML_Params
     (Params       : String;
      Indent_Level : Natural;
      Prefix       : String := "";
      Suffix       : String := "");
   --  Print the comma separated parameter list `Params` as succession of
   --  XML formatted param tags (using `XML_Param` function).
   --  This procedure adds to each parameter value `Prefix` and `Suffix`.

   function XML_Foot return String is ("</rule>");
   --  Function to get the XML footer for a rule instance XML display

   procedure Process_String_Arg
     (Params_Object  : JSON_Value;
      Param_Name     : String;
      Instance_Field : in out Unbounded_Wide_Wide_String;
      Normalize      : Boolean := False);
   --  If the given `Param_Name` is present as a field in the `Params_Object`,
   --  then parse it as a string literal and set `Instance_Field` to the result
   --  value.
   --  If `Params_Object` contains `Param_Name` then unset the field.
   --  If `Narmalize` is true, then remove spaces and lower the extracted
   --  string.

   --------------------------------------
   -- XML rule help printing functions --
   --------------------------------------

   --  The following procedures are used to display rule help in the XML
   --  format. See `Gnatcheck.Rules.Rule_Info.XML_Rule_Help` where those
   --  procedures are stored.

   procedure No_Param_XML_Help
     (Rule : Rule_Info; Indent_Level : Natural);

   procedure Int_Param_XML_Help
     (Rule : Rule_Info; Indent_Level : Natural);

   procedure Bool_Param_XML_Help
     (Rule : Rule_Info; Indent_Level : Natural);

   procedure String_Param_XML_Help
     (Rule : Rule_Info; Indent_Level : Natural);

   procedure Id_Suffix_Param_XML_Help
     (Rule : Rule_Info; Indent_Level : Natural);

   procedure Id_Prefix_Param_XML_Help
     (Rule : Rule_Info; Indent_Level : Natural);

   procedure Id_Casing_Param_XML_Help
     (Rule : Rule_Info; Indent_Level : Natural);

   procedure Forbidden_Param_XML_Help
     (Rule : Rule_Info; Indent_Level : Natural);

   procedure Silent_Exc_Handler_Param_XML_Help
     (Rule : Rule_Info; Indent_Level : Natural);

   -------------------------------------------
   -- Allowed exemption parameter functions --
   -------------------------------------------

   function No_Exemption_Param_Allowed
     (Ignored_Param : String) return Boolean is (False);

   function All_Exemption_Parameter_Allowed
     (Ignored_Param : String) return Boolean is (True);

   function Id_Suffix_Allowed_Exemption_Param
     (Param : String) return Boolean is
   (To_Lower (Param) in
      "access" | "access_obj" | "class_access" | "class_subtype" |
      "constant" | "renaming" | "interrupt" | "type");

   function Id_Prefix_Allowed_Exemption_Param
     (Param : String) return Boolean is
   (To_Lower (Param) in
      "type" | "concurrent" | "access" | "class_access" | "subprogram_access" |
      "derived" | "constant" | "enum" | "exception" | "exclusive");

   function Id_Casing_Allowed_Exemption_Param
     (Param : String) return Boolean is
   (To_Lower (Param) in
      "type" | "constant" | "enum" | "exception" | "others" | "exclude");

   ---------------------------------------------
   -- Rule parameter from diagnosis functions --
   ---------------------------------------------

   function No_Param_From_Diag (Ignored_Diag : String) return String is ("");

   function Id_Suffix_Param_From_Diag (Diag : String) return String;

   function Id_Prefix_Param_From_Diag (Diag : String) return String;

   function Id_Casing_Param_From_Diag (Diag : String) return String;

   function Forbidden_Param_From_Diag (Diag : String) return String;

   --------------------------------------
   -- Rule instance creation functions --
   --------------------------------------

   --  The following functions are used to create rule instances from a
   --  given rule identifier. See `Gnatcheck.Rules.Rule_Info.Create_Instance`
   --  where those functions are stored.

   function Create_No_Param_Instance
     (Is_Alias : Boolean) return Rule_Instance_Access;

   function Create_Int_Instance
     (Is_Alias : Boolean) return Rule_Instance_Access;

   function Create_Bool_Instance
     (Is_Alias : Boolean) return Rule_Instance_Access;

   function Create_String_Instance
     (Is_Alias : Boolean) return Rule_Instance_Access;

   function Create_Array_Instance
     (Is_Alias : Boolean) return Rule_Instance_Access;

   function Create_Int_Or_Bools_Instance
     (Is_Alias : Boolean) return Rule_Instance_Access;

   function Create_Id_Suffix_Instance
     (Is_Alias : Boolean) return Rule_Instance_Access;

   function Create_Id_Prefix_Instance
     (Is_Alias : Boolean) return Rule_Instance_Access;

   function Create_Id_Casing_Instance
     (Is_Alias : Boolean) return Rule_Instance_Access;

   function Create_Forbidden_Instance
     (Is_Alias : Boolean) return Rule_Instance_Access;

   function Create_Silent_Exc_Handler_Instance
     (Is_Alias : Boolean) return Rule_Instance_Access;

   function Create_Custom_Instance
     (Is_Alias : Boolean) return Rule_Instance_Access;

   -------------------------------------------
   -- Actual parameter processing functions --
   -------------------------------------------

   --  The following are functions to handle rule parameter parsing, they are
   --  used during the `Gnatcheck.Rules.Rule_Table.Process_Rule_Option`
   --  procedure. Each rule info record has an associated actual parameter
   --  processing function (see `Gnatcheck.Rules.Rule_Info`).

   function Get_Or_Create_Instance
     (Id            : Rule_Id;
      Instance_Name : String) return Rule_Instance_Access;
   --  Helper function to create an instance with the given name and return a
   --  pointer to it. This function adds the created instance to the global
   --  instance map.

   function Load_Dictionary
     (Instance  : Rule_Instance_Access;
      File_Name : String;
      Param     : in out Unbounded_Wide_Wide_String) return Boolean;
   --  Load dictionary file `File_Name` for instance `Instance` and append the
   --  result in `Param` as a comma separated list. This function returns
   --  whether the dictionnary load was successful.

   procedure Emit_Wrong_Parameter
     (Instance : Rule_Instance_Access;
      Param    : String);
   --  Procedure to call when a wrong parameter is encountered during a
   --  parameter processing. This display an error message and set the
   --  `Bad_Rule_Detected` flag to True.

   procedure Emit_Required_Parameter (Instance : Rule_Instance_Access);
   --  Procedure to emit an error message about required parameter with "+R".
   --  This procedure set `Bad_Rule_Detected` to True.

   procedure Emit_No_Parameter_Allowed (Instance : Rule_Instance_Access);
   --  Procedure to call to emit an error message about not parameter being
   --  allowed with "-R". This procedure set `Bad_Rule_Detected` to True.

   procedure Emit_Redefining
     (Instance   : Rule_Instance_Access;
      Param      : String;
      Defined_At : String);
   --  Procedure to call to emit an error message about a parameter being
   --  redefined. This proceduren set `Bad_Rule_Detected` to True.

   procedure Emit_File_Load_Error
     (Instance  : Rule_Instance_Access;
      File_Name : String);
   --  Procedure to call to emit an error about file which cannot be loaded.
   --  This procedure set `Bad_Rule_Detected` to True.

   function Defined_Str (Defined_At : String) return String is
     (if Defined_At = "" then "command line" else Defined_At);
   --  Helper function to return Defined_At if not null, or "command line"

   procedure No_Param_Process
     (Rule          : Rule_Id;
      Instance_Name : String;
      Param         : String;
      Enable        : Boolean;
      Defined_At    : String);
   --  Function to process an actual parameter for a rule with no formal
   --  parameter. This function expect `Param` to be empty.

   procedure Int_Param_Process
     (Rule          : Rule_Id;
      Instance_Name : String;
      Param         : String;
      Enable        : Boolean;
      Defined_At    : String);
   --  Function to process an integer parametrized rule actual parameter. If
   --  the provided value is not an integer, disable the instance and display
   --  a warning.

   procedure Bool_Param_Process
     (Rule          : Rule_Id;
      Instance_Name : String;
      Param         : String;
      Enable        : Boolean;
      Defined_At    : String);
   --  Function to process a boolean parametrized rule actual parameter. The
   --  provided `Param` should be the name of the formal parameter of the rule
   --  or an empty string. In the first case the instance actual parameter is
   --  set to True, in the second, to False.

   procedure String_Param_Process
     (Rule          : Rule_Id;
      Instance_Name : String;
      Param         : String;
      Enable        : Boolean;
      Defined_At    : String);
   --  Function to process a string parametrized rule actual parameter. The
   --  given `Param` must be a non-empty string.

   procedure Array_Param_Process
     (Rule          : Rule_Id;
      Instance_Name : String;
      Param         : String;
      Enable        : Boolean;
      Defined_At    : String);
   --  Function to process an array parametrized rule actual parameter. The
   --  given `Param` is a possibly empty string.

   procedure Int_Or_Bools_Param_Process
     (Rule          : Rule_Id;
      Instance_Name : String;
      Param         : String;
      Enable        : Boolean;
      Defined_At    : String);
   --  Function to process a integer and booleans parametrized rule actual
   --  parameter value. The provided `Param` can be an integer value or the
   --  name of a boolean formal parameter of the rule.

   procedure Id_Suffix_Param_Process
     (Rule          : Rule_Id;
      Instance_Name : String;
      Param         : String;
      Enable        : Boolean;
      Defined_At    : String);
   --  Function to process an actual parameter for the "identifier_suffix"
   --  rule.

   procedure Id_Prefix_Param_Process
     (Rule          : Rule_Id;
      Instance_Name : String;
      Param         : String;
      Enable        : Boolean;
      Defined_At    : String);
   --  Function to process an actual parameter for the "identifier_prefix"
   --  rule.

   procedure Id_Casing_Param_Process
     (Rule          : Rule_Id;
      Instance_Name : String;
      Param         : String;
      Enable        : Boolean;
      Defined_At    : String);
   --  Function to process an actual parameter for the "identifier_casing"
   --  rule.

   procedure Forbidden_Param_Process
     (Rule          : Rule_Id;
      Instance_Name : String;
      Param         : String;
      Enable        : Boolean;
      Defined_At    : String);
   --  Function to process an actual parameter for the "identifier_casing",
   --  "forbidden_pragmas" and "forbidden_aspects" rules.

   procedure Silent_Exc_Handler_Param_Process
     (Rule          : Rule_Id;
      Instance_Name : String;
      Param         : String;
      Enable        : Boolean;
      Defined_At    : String);
   --  Function to process an actual parameter for the
   --  "silent_exception_handlers" rule.

   procedure Custom_Param_Process
     (Rule          : Rule_Id;
      Instance_Name : String;
      Param         : String;
      Enable        : Boolean;
      Defined_At    : String);
   --  Function to process an actual parameter for a custom rule.

   -----------------------------------------
   -- Actual parameter mapping procedures --
   -----------------------------------------

   --  The following are helpers for rule instances actual parameters mapping.

   procedure Append_Int_Param
     (Args  : in out Rule_Argument_Vectors.Vector;
      Name  : Text_Type;
      Value : Integer);
   --  Add the integer actual parameter to the given argument vector

   procedure Append_Bool_Param
     (Args  : in out Rule_Argument_Vectors.Vector;
      Name  : Text_Type;
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

   procedure Handle_Array_Param
     (Args     : in out Rule_Argument_Vectors.Vector;
      Instance : in out One_Array_Parameter_Instance);
   --  Common procedure to handle array parameter rules instances by adding
   --  their parameter actual value in the provided rule arguments vector.

   --  ===== Local subprograms bodies =====

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

   --------------------------
   -- Load_Dictionary_File --
   --------------------------

   function Load_Dictionary_File (File_Name : String) return Unbounded_String
   is
      Name : constant String := Find_File (File_Name);
      File : File_Type;
      Line : String (1 .. 1024);
      Len  : Natural;
      Res  : Unbounded_String := Null_Unbounded_String;
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
                  if Length (Res) /= 0 then
                     Append (Res, ",");
                  end if;

                  Append (Res, S);
               end if;
            end;
         end loop;

         Close (File);
      end if;

      return Res;
   exception
      when others =>
         return Null_Unbounded_String;
   end Load_Dictionary_File;

   -----------------------
   -- Turn_Instance_Off --
   -----------------------

   procedure Turn_Instance_Off (Instance : Rule_Instance_Access) is
   begin
      Turn_Instance_Off (Instance_Name (Instance));
   end Turn_Instance_Off;

   ----------------------
   -- Print_XML_Params --
   ----------------------

   procedure Print_XML_Params
     (Params       : String;
      Indent_Level : Natural;
      Prefix       : String := "";
      Suffix       : String := "")
   is
      C      : Character;
      Buffer : Unbounded_String;
   begin
      if Params'Length = 0 then
         return;
      end if;

      for J in Params'First .. Params'Last loop
         C := Params (J);

         if C /= ',' then
            Append (Buffer, C);
         elsif Buffer /= "" then
            XML_Report
              (XML_Param (Prefix & To_String (Buffer) & Suffix),
               Indent_Level);
            Set_Unbounded_String (Buffer, "");
         end if;
      end loop;
   end Print_XML_Params;

   ------------------------
   -- Process_String_Arg --
   ------------------------

   procedure Process_String_Arg
     (Params_Object  : JSON_Value;
      Param_Name     : String;
      Instance_Field : in out Unbounded_Wide_Wide_String;
      Normalize      : Boolean := False) is
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
              (Instance_Field, To_Wide_Wide_String (Field_Val));
            Params_Object.Unset_Field (Param_Name);
         end;
      end if;
   end Process_String_Arg;

   --  == XML Help functions

   -----------------------
   -- No_Param_XML_Help --
   -----------------------

   procedure No_Param_XML_Help
     (Rule : Rule_Info; Indent_Level : Natural) is
   begin
      Print (Indent_Level * Indent_String &
             "<check switch=""+R"         &
             Rule_Name (Rule)             &
             """ label="""                &
             To_String (Rule.Help_Info)   &
             """/>");
   end No_Param_XML_Help;

   ------------------------
   -- Int_Param_XML_Help --
   ------------------------

   procedure Int_Param_XML_Help
     (Rule : Rule_Info; Indent_Level : Natural) is
   begin
      Print (Indent_Level * Indent_String &
             "<spin switch=""+R"          &
             Rule_Name (Rule)             &
             """ label="""                &
             To_String (Rule.Help_Info)   &
             """ min=""0"""               &
             " max=""99999"""             &
             " default=""-1"""            &
             " separator="":"""           &
             "/>");
   end Int_Param_XML_Help;

   -------------------------
   -- Bool_Param_XML_Help --
   -------------------------

   procedure Bool_Param_XML_Help
     (Rule : Rule_Info; Indent_Level : Natural) is
   begin
      Print (Indent_Level * Indent_String &
             "<field switch=""+R"         &
             Rule_Name (Rule)             &
             """ separator="":"""         &
             " label="""                  &
             To_String (Rule.Help_Info)   &
             """/>");
      Print (Indent_Level * Indent_String     &
             "<check switch=""+R"             &
             Rule_Name (Rule) & ":"           &
             Param_Name (Rule, 2)             &
             """ label="""                    &
             To_String (Rule.Help_Info)       &
             """/>");
   end Bool_Param_XML_Help;

   ---------------------------
   -- String_Param_XML_Help --
   ---------------------------

   procedure String_Param_XML_Help
     (Rule : Rule_Info; Indent_Level : Natural) is
   begin
      Print (Indent_Level * Indent_String &
             "<field switch=""+R"         &
             Rule_Name (Rule)             &
             """ separator="":"""         &
             " label="""                  &
             To_String (Rule.Help_Info)   &
             """/>");
   end String_Param_XML_Help;

   ------------------------------
   -- Id_Suffix_Param_XML_Help --
   ------------------------------

   procedure Id_Suffix_Param_XML_Help
     (Rule : Rule_Info; Indent_Level : Natural)
   is
      procedure Print_Help (Param : String; Help : String);
      --  Print XML help for parameter Param

      -----------
      -- Print --
      -----------

      procedure Print_Help (Param : String; Help : String) is
      begin
         Print (Indent_Level * Indent_String       &
                "<field switch=""+R"               &
                Rule_Name (Rule)                   &
                ":" & Param & '"'                  &
                " label="""                        &
                "suffix for " & Help & " names"    &
                " (empty string disables check)""" &
                " separator=""="""                 &
                " switch-off=""-R"                 &
                Rule_Name (Rule)                   &
                ":" & Param & '"'                  &
                "/>");
      end Print_Help;
   begin
      Print (Indent_Level * Indent_String          &
             "<check switch=""+R"                  &
             Rule_Name (Rule)                      &
             ":Default"""                          &
             " label="""                           &
             "identifiers use standard suffixes""" &
             "/>");

      Print_Help ("Type_Suffix", "type");
      Print_Help ("Access_Suffix", "access type");
      Print_Help ("Constant_Suffix", "constant");
      Print_Help ("Renaming_Suffix", "package renaming");
      Print_Help ("Access_Obj_Suffix", "access object");
      Print_Help ("Interrupt_Suffix", "interrupt handler");

      --  Specifying the dependencies between the default suffixes and the
      --  content of the fields for specific suffixes

      Print (Indent_Level * Indent_String                   &
             "<default-value-dependency master-switch=""+R" &
             Rule_Name (Rule)                               &
             ":Default"""                                   &
             " slave-switch=""+R"                           &
             Rule_Name (Rule)                               &
             ":Type_Suffix=_T""/>");
      Print (Indent_Level * Indent_String                   &
             "<default-value-dependency master-switch=""+R" &
             Rule_Name (Rule)                               &
             ":Default"""                                   &
             " slave-switch=""+R"                           &
             Rule_Name (Rule)                               &
             ":Access_Suffix=_A""/>");
      Print (Indent_Level * Indent_String                   &
             "<default-value-dependency master-switch=""+R" &
             Rule_Name (Rule)                               &
             ":Default"""                                   &
             " slave-switch=""+R"                           &
             Rule_Name (Rule)                               &
             ":Constant_Suffix=_C""/>");
      Print (Indent_Level * Indent_String                   &
             "<default-value-dependency master-switch=""+R" &
             Rule_Name (Rule)                               &
             ":Default"""                                   &
             " slave-switch=""+R"                           &
             Rule_Name (Rule)                               &
             ":Renaming_Suffix=_R""/>");
   end Id_Suffix_Param_XML_Help;

   ------------------------------
   -- Id_Prefix_Param_XML_Help --
   ------------------------------

   procedure Id_Prefix_Param_XML_Help
     (Rule : Rule_Info; Indent_Level : Natural)
   is
      procedure Print_Help (Param : String; Help : String);
      --  Print XML help for parameter Param

      -----------
      -- Print --
      -----------

      procedure Print_Help (Param : String; Help : String) is
      begin
         Print (Indent_Level * Indent_String       &
                "<field switch=""+R"               &
                Rule_Name (Rule)                   &
                ":" & Param & '"'                  &
                " label="""                        &
                "prefix for " & Help               &
                " (empty string disables check)""" &
                " separator=""="""                 &
                " switch-off=""-R"                 &
                Rule_Name (Rule)                   &
                ":" & Param & '"'                  &
                "/>");
      end Print_Help;

   begin
      Print_Help ("Type", "type names");
      Print_Help ("Concurrent", "task and protected type names");
      Print_Help ("Access", "access type names");
      Print_Help ("Class_Access", "class access type names");
      Print_Help ("Subprogram_Access", "access-to-subprogram type names");
      Print_Help ("Derived", "derived type names");
      Print_Help ("Constant", "constant names");
      Print_Help ("Exception", "exception names");
      Print_Help ("Enum", "enumeration literals");
      Print (Indent_Level * Indent_String &
             "<check switch=""+R"         &
             Rule_Name (Rule)             &
             ":Exclusive"""               &
             " label=""strong check mode""/>");
   end Id_Prefix_Param_XML_Help;

   ------------------------------
   -- Id_Casing_Param_XML_Help --
   ------------------------------

   procedure Id_Casing_Param_XML_Help
     (Rule : Rule_Info; Indent_Level : Natural)
   is
      procedure Print_Combo (Par : String; Descr : String);
      --  Print one combo definition

      -----------------
      -- Print_Combo --
      -----------------

      procedure Print_Combo (Par : String; Descr : String) is
      begin
         Print (Indent_Level * Indent_String           &
                "<combo switch=""+RIdentifier_Casing:" &
                Par & '"' & " label=""" & Descr        &
                " casing"" separator=""="">");
         Print ((Indent_Level + 1) * Indent_String &
                "<combo-entry value=""upper"" />");
         Print ((Indent_Level + 1) * Indent_String &
                "<combo-entry value=""lower"" />");
         Print ((Indent_Level + 1) * Indent_String &
                "<combo-entry value=""mixed"" />");
         Print (Indent_Level * Indent_String & "</combo>");
      end Print_Combo;

   begin
      Print_Combo ("Type", "type name");
      Print_Combo ("Enum", "enumeration literal");
      Print_Combo ("Constant", "constant name");
      Print_Combo ("Exception", "exception name");
      Print_Combo ("Others", "other name");

      Print (Indent_Level * Indent_String        &
             "<field switch=""+R"                &
             Rule_Name (Rule)                    &
             ":Exclude"""                        &
             " label="""                         &
             "dictionary of casing exceptions""" &
             " separator=""="""                  &
             "/>");
   end Id_Casing_Param_XML_Help;

   ------------------------------
   -- Forbidden_Param_XML_Help --
   ------------------------------

   procedure Forbidden_Param_XML_Help
     (Rule : Rule_Info; Indent_Level : Natural)
   is
      Name : constant String := Rule_Name (Rule);
      Str  : constant String := Name (11 .. Name'Last);
   begin
      Print (Indent_Level * Indent_String &
             "<check switch=""+R" & Name  &
             ":ALL"""                     &
             " label="""                  &
             "detect all " & Str          &
             " except explicitly disabled""/>");

      Print (Indent_Level * Indent_String &
             "<check switch=""+R" & Name  &
             ":GNAT"""                    &
             " label="""                  &
             "detect all GNAT " & Str     &
             " except explicitly disabled""/>");

      Print (Indent_Level * Indent_String &
             "<field switch=""+R" & Name  &
             """ label="""                &
             "detect specified " & Str    &
             " (use ',' as separator)"""  &
             " separator="":""/>");

      Print (Indent_Level * Indent_String     &
             "<field switch=""-R"  & Name     &
             """ label="""                    &
             "do not detect specified " & Str &
             " (use ',' as separator)"""      &
             " separator="":""/>");
   end Forbidden_Param_XML_Help;

   ---------------------------------------
   -- Silent_Exc_Handler_Param_XML_Help --
   ---------------------------------------

   procedure Silent_Exc_Handler_Param_XML_Help
     (Rule : Rule_Info; Indent_Level : Natural) is
   begin
      Print (Indent_Level * Indent_String &
             "<field switch=""+R"         &
             Rule_Name (Rule)             &
             """ separator="":"""         &
             " label="""                  &
             To_String (Rule.Help_Info)   &
             """/>");
   end Silent_Exc_Handler_Param_XML_Help;

   --  == Parameter name from diagnosis

   -------------------------------
   -- Id_Suffix_Param_From_Diag --
   -------------------------------

   function Id_Suffix_Param_From_Diag (Diag : String) return String is
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
   end Id_Suffix_Param_From_Diag;

   -------------------------------
   -- Id_Prefix_Param_From_Diag --
   -------------------------------

   function Id_Prefix_Param_From_Diag (Diag : String) return String is
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
   end Id_Prefix_Param_From_Diag;

   -------------------------------
   -- Id_Casing_Param_From_Diag --
   -------------------------------

   function Id_Casing_Param_From_Diag (Diag : String) return String
   is
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
   end Id_Casing_Param_From_Diag;

   -------------------------------
   -- Forbidden_Param_From_Diag --
   -------------------------------

   function Forbidden_Param_From_Diag (Diag : String) return String
   is
      First_Idx :  Natural := Index (Diag, " ", Going => Backward) + 1;
      Last_Idx  :  Natural := Diag'Last;
   begin
      if Arg.Show_Rule.Get then

         --  The diagnosis has the following format:
         --     foo.adb:nn:mm: use of pragma Bar [Rule_Name]
         Last_Idx  := First_Idx - 2;
         First_Idx := Index (Diag (Diag'First ..  Last_Idx),
                             " ",
                             Going => Backward) + 1;
      end if;

      return To_Lower (Diag (First_Idx .. Last_Idx));
   end Forbidden_Param_From_Diag;

   --  == Instance creation functions

   ------------------------
   -- Create_No_Instance --
   ------------------------

   function Create_No_Param_Instance
     (Is_Alias : Boolean) return Rule_Instance_Access is
   begin
      return new Rule_Instance (Is_Alias => Is_Alias);
   end Create_No_Param_Instance;

   -------------------------
   -- Create_Int_Instance --
   -------------------------

   function Create_Int_Instance
     (Is_Alias : Boolean) return Rule_Instance_Access is
   begin
      return new One_Integer_Parameter_Instance (Is_Alias => Is_Alias);
   end Create_Int_Instance;

   --------------------------
   -- Create_Bool_Instance --
   --------------------------

   function Create_Bool_Instance
     (Is_Alias : Boolean) return Rule_Instance_Access is
   begin
      return new One_Boolean_Parameter_Instance (Is_Alias => Is_Alias);
   end Create_Bool_Instance;

   ----------------------------
   -- Create_String_Instance --
   ----------------------------

   function Create_String_Instance
     (Is_Alias : Boolean) return Rule_Instance_Access is
   begin
      return new One_String_Parameter_Instance (Is_Alias => Is_Alias);
   end Create_String_Instance;

   ---------------------------
   -- Create_Array_Instance --
   ---------------------------

   function Create_Array_Instance
     (Is_Alias : Boolean) return Rule_Instance_Access is
   begin
      return new One_Array_Parameter_Instance (Is_Alias => Is_Alias);
   end Create_Array_Instance;

   ----------------------------------
   -- Create_Int_Or_Bools_Instance --
   ----------------------------------

   function Create_Int_Or_Bools_Instance
     (Is_Alias : Boolean) return Rule_Instance_Access is
   begin
      return new One_Integer_Or_Booleans_Parameter_Instance
        (Is_Alias => Is_Alias);
   end Create_Int_Or_Bools_Instance;

   -------------------------------
   -- Create_Id_Suffix_Instance --
   -------------------------------

   function Create_Id_Suffix_Instance
     (Is_Alias : Boolean) return Rule_Instance_Access is
   begin
      return new Identifier_Suffixes_Instance (Is_Alias => Is_Alias);
   end Create_Id_Suffix_Instance;

   -------------------------------
   -- Create_Id_Prefix_Instance --
   -------------------------------

   function Create_Id_Prefix_Instance
     (Is_Alias : Boolean) return Rule_Instance_Access  is
   begin
      return new Identifier_Prefixes_Instance (Is_Alias => Is_Alias);
   end Create_Id_Prefix_Instance;

   -------------------------------
   -- Create_Id_Casing_Instance --
   -------------------------------

   function Create_Id_Casing_Instance
     (Is_Alias : Boolean) return Rule_Instance_Access is
   begin
      return new Identifier_Casing_Instance (Is_Alias => Is_Alias);
   end Create_Id_Casing_Instance;

   ----------------------------------------
   -- Create_Silent_Exc_Handler_Instance --
   ----------------------------------------

   function Create_Silent_Exc_Handler_Instance
     (Is_Alias : Boolean) return Rule_Instance_Access is
   begin
      return new Silent_Exception_Handlers_Instance (Is_Alias => Is_Alias);
   end Create_Silent_Exc_Handler_Instance;

   -------------------------------
   -- Create_Forbidden_Instance --
   -------------------------------

   function Create_Forbidden_Instance
     (Is_Alias : Boolean) return Rule_Instance_Access is
   begin
      return new Forbidden_Instance (Is_Alias => Is_Alias);
   end Create_Forbidden_Instance;

   ----------------------------
   -- Create_Custom_Instance --
   ----------------------------

   function Create_Custom_Instance
     (Is_Alias : Boolean) return Rule_Instance_Access is
   begin
      return new Custom_Instance (Is_Alias => Is_Alias);
   end Create_Custom_Instance;

   --  == Parameter process procedures

   ----------------------------
   -- Get_Or_Create_Instance --
   ----------------------------

   function Get_Or_Create_Instance
     (Id            : Rule_Id;
      Instance_Name : String) return Rule_Instance_Access
   is
      Rule                     : constant Rule_Info := All_Rules (Id);
      Normalized_Rule_Name     : constant String :=
        To_Lower (To_String (Rule.Name));
      Normalized_Instance_Name : constant String := To_Lower (Instance_Name);
      Instance                 : Rule_Instance_Access := null;
   begin
      --  If the instance name is already registered
      if All_Rule_Instances.Contains (Normalized_Instance_Name) then
         return All_Rule_Instances (Normalized_Instance_Name);
      end if;

      --  Else, create a new instance and return it
      if Normalized_Instance_Name = Normalized_Rule_Name then
         Instance := Rule.Create_Instance (Is_Alias => False);
      else
         Instance := Rule.Create_Instance (Is_Alias => True);
         Instance.Alias_Name := To_Unbounded_String (Instance_Name);
      end if;
      Instance.Rule := Id;
      Instance.Source_Mode := General;
      Turn_Instance_On (Instance);
      return Instance;
   end Get_Or_Create_Instance;

   ---------------------
   -- Load_Dictionary --
   ---------------------

   function Load_Dictionary
     (Instance  : Rule_Instance_Access;
      File_Name : String;
      Param     : in out Unbounded_Wide_Wide_String) return Boolean
   is
      Content : constant Unbounded_String := Load_Dictionary_File (File_Name);
   begin
      if Content /= Null_Unbounded_String then
         if Length (Param) /= 0 and then Length (Content) /= 0 then
            Append (Param, ",");
         end if;
         Append (Param, To_Wide_Wide_String (To_String (Content)));
         return True;
      else
         Emit_File_Load_Error (Instance, File_Name);
         return False;
      end if;
   end Load_Dictionary;

   --------------------------
   -- Emit_Wrong_Parameter --
   --------------------------

   procedure Emit_Wrong_Parameter
     (Instance : Rule_Instance_Access;
      Param    : String) is
   begin
      Error ("(" & Instance_Name (Instance) & ") wrong parameter: " & Param);
      Bad_Rule_Detected := True;
   end Emit_Wrong_Parameter;

   -----------------------------
   -- Emit_Required_Parameter --
   -----------------------------

   procedure Emit_Required_Parameter (Instance : Rule_Instance_Access) is
   begin
      Error
        ("(" & Instance_Name (Instance) & ") parameter is required for +R");
      Bad_Rule_Detected := True;
   end Emit_Required_Parameter;

   -------------------------------
   -- Emit_No_Parameter_Allowed --
   -------------------------------

   procedure Emit_No_Parameter_Allowed (Instance : Rule_Instance_Access) is
   begin
      Error ("(" & Instance_Name (Instance) & ") no parameter allowed for -R");
      Bad_Rule_Detected := True;
   end Emit_No_Parameter_Allowed;

   ---------------------
   -- Emit_Redefining --
   ---------------------

   procedure Emit_Redefining
     (Instance   : Rule_Instance_Access;
      Param      : String;
      Defined_At : String) is
   begin
      Error
        ("redefining at " & Defined_Str (Defined_At) &
         " parameter" & (if Param = "" then "" else " " & Param) &
         " for rule " & Instance_Name (Instance) &
         " defined at " & Defined_Str (To_String (Instance.Defined_At)));
      Rule_Option_Problem_Detected := True;
   end Emit_Redefining;

   --------------------------
   -- Emit_File_Load_Error --
   --------------------------

   procedure Emit_File_Load_Error
     (Instance  : Rule_Instance_Access;
      File_Name : String) is
   begin
      Error
        ("(" & Instance_Name (Instance) & "): cannot load file " & File_Name);
      Bad_Rule_Detected := True;
   end Emit_File_Load_Error;

   ----------------------
   -- No_Param_Process --
   ----------------------

   procedure No_Param_Process
     (Rule          : Rule_Id;
      Instance_Name : String;
      Param         : String;
      Enable        : Boolean;
      Defined_At    : String)
   is
      Instance : constant Rule_Instance_Access :=
        Get_Or_Create_Instance (Rule, Instance_Name);
   begin
      --  If there is a provided param display an error
      if Param /= "" then
         Error
           ("no parameter can be set for rule " &
            Gnatcheck.Rules.Instance_Name (Instance) & ", " &
            Param & " ignored");
         Bad_Rule_Detected := True;

      --  Just enable the instance following the command line
      else
         if Enable then
            Instance.Defined_At := To_Unbounded_String (Defined_At);
         else
            Turn_Instance_Off (Instance);
         end if;
      end if;
   end No_Param_Process;

   -----------------------
   -- Int_Param_Process --
   -----------------------

   procedure Int_Param_Process
     (Rule          : Rule_Id;
      Instance_Name : String;
      Param         : String;
      Enable        : Boolean;
      Defined_At    : String)
   is
      Instance        : constant Rule_Instance_Access :=
        Get_Or_Create_Instance (Rule, Instance_Name);
      Tagged_Instance : One_Integer_Parameter_Instance renames
        One_Integer_Parameter_Instance (Instance.all);
   begin
      --  If the param is empty and the command line is enabling the instance,
      --  emit an error
      if Param = "" then
         if Enable then
            Emit_Required_Parameter (Instance);
         end if;
         Turn_Instance_Off (Instance);

      --  Else, the param has a value
      else
         if Enable then
            if Arg.Check_Redefinition.Get and then
              Tagged_Instance.Param /= Integer'First
            then
               Emit_Redefining (Instance, "", Defined_At);
            end if;

            --  Try to parse an integer from the parameter string, and if it is
            --  a valid value place it in the instance. Else emit a an error.
            begin
               Tagged_Instance.Param := Integer'Value (Param);
               if Tagged_Instance.Param >= -1 then
                  Instance.Defined_At := To_Unbounded_String (Defined_At);
               else
                  Emit_Wrong_Parameter (Instance, Param);
                  Turn_Instance_Off (Instance);
               end if;
            exception
               when Constraint_Error =>
                  Emit_Wrong_Parameter (Instance, Param);
                  Turn_Instance_Off (Instance);
            end;

         --  If the command line is disabling the rule, the parameter should
         --  be empty.
         else
            Emit_No_Parameter_Allowed (Instance);
            Turn_Instance_Off (Instance);
         end if;
      end if;
   end Int_Param_Process;

   ------------------------
   -- Bool_Param_Process --
   ------------------------

   procedure Bool_Param_Process
     (Rule          : Rule_Id;
      Instance_Name : String;
      Param         : String;
      Enable        : Boolean;
      Defined_At    : String)
   is
      Instance        : constant Rule_Instance_Access :=
        Get_Or_Create_Instance (Rule, Instance_Name);
      Tagged_Instance : One_Boolean_Parameter_Instance renames
        One_Boolean_Parameter_Instance (Instance.all);
   begin
      --  If the param is empty, just enable or disable the instance
      if Param = "" then
         if Enable then
            Instance.Defined_At := To_Unbounded_String (Defined_At);
         else
            Turn_Instance_Off (Instance);
         end if;

      --  Else, if the parameter has a different value from the LKQL parameter
      --  name, emit an error.
      elsif Param_Name (All_Rules (Rule), 2) /= To_Lower (Param) then
         Emit_Wrong_Parameter (Instance, Param);
         Turn_Instance_Off (Instance);

      --  Else, if the command line is enabling the rule, the parameter is not
      --  empty and is valid. Just set the instance parameter value.
      elsif Enable then
         if Arg.Check_Redefinition.Get and then Tagged_Instance.Param /= Unset
         then
            Emit_Redefining (Instance, Param, Defined_At);
         end if;

         Tagged_Instance.Param := On;
         Instance.Defined_At := To_Unbounded_String (Defined_At);

      --  Else the command line is disabling the rule so no parameter allowed
      else
         Emit_No_Parameter_Allowed (Instance);
         Turn_Instance_Off (Instance);
      end if;
   end Bool_Param_Process;

   --------------------------
   -- String_Param_Process --
   --------------------------

   procedure String_Param_Process
     (Rule          : Rule_Id;
      Instance_Name : String;
      Param         : String;
      Enable        : Boolean;
      Defined_At    : String)
   is
      Instance        : constant Rule_Instance_Access :=
        Get_Or_Create_Instance (Rule, Instance_Name);
      Tagged_Instance : One_String_Parameter_Instance renames
        One_String_Parameter_Instance (Instance.all);
   begin
      --  If the param is empty and the command line enable the instance, emit
      --  an error message.
      if Param = "" then
         if Enable then
            Emit_Required_Parameter (Instance);
         else
            Turn_Instance_Off (Instance);
         end if;

      --  Else, the parameter is not empty, if the instance is enabled check
      --  the parameter value and enable the instance.
      elsif Enable then
         if Arg.Check_Redefinition.Get and then
           not Ada.Strings.Wide_Wide_Unbounded."="
             (Tagged_Instance.Param, Null_Unbounded_Wide_Wide_String)
         then
            Emit_Redefining (Instance, Param, Defined_At);
         end if;

         --  Headers rule takes a file name as parameter, containing the
         --  header contents.
         if Rule_Name (Instance) = "headers" then
            if not Tagged_Instance.Load_File (To_Load => Param) then
               Emit_File_Load_Error (Instance, Param);
            end if;

         --  Other cases: Just add the parameter to the instance acual params
         else
            Append (Tagged_Instance.Param, To_Wide_Wide_String (Param));
         end if;

         --  Set the instance definition location
         Instance.Defined_At := To_Unbounded_String (Defined_At);

      --  Else, emit a message about no parameter allowed for instance
      --  disabling.
      else
         Emit_No_Parameter_Allowed (Instance);
         Turn_Instance_Off (Instance);
      end if;
   end String_Param_Process;

   -------------------------
   -- Array_Param_Process --
   -------------------------

   procedure Array_Param_Process
     (Rule          : Rule_Id;
      Instance_Name : String;
      Param         : String;
      Enable        : Boolean;
      Defined_At    : String)
   is
      Instance        : constant Rule_Instance_Access :=
        Get_Or_Create_Instance (Rule, Instance_Name);
      Tagged_Instance : One_Array_Parameter_Instance renames
        One_Array_Parameter_Instance (Instance.all);
   begin
      --  If the param is empty, just disable the instance following the
      --  command line.
      if Param = "" then
         if not Enable then
            Turn_Instance_Off (Instance);
         end if;

      --  Else, the param is not empty, if the command line is enabling the
      --  instance process the parameter.
      elsif Enable then
         if Arg.Check_Redefinition.Get and then
           not Ada.Strings.Wide_Wide_Unbounded."="
             (Tagged_Instance.Param, Null_Unbounded_Wide_Wide_String)
         then
            Emit_Redefining (Instance, Param, Defined_At);
         end if;

         --  Special case: "name_clashes" takes a file as argument, this file
         --  should be read by the `Load_Dictionary` function.
         if Rule_Name (Instance) = "name_clashes" then
            if Load_Dictionary
              (Instance,
               Expand_Env_Variables (Param),
               Tagged_Instance.Param)
            then
               Ada.Strings.Unbounded.Set_Unbounded_String
               (Tagged_Instance.File, Param);
               Instance.Defined_At := To_Unbounded_String (Defined_At);
            else
               Turn_Instance_Off (Instance);
            end if;

         --  Other cases: Append the array to the instance actual parameter
         --  as a comma separated list.
         else
            --  Check the parameter value for special rules
            if (Rule_Name (Instance) = "parameters_out_of_order"
                and then Param not in
                  "in" | "defaulted_in" | "in_out" | "access" | "out")
              or else (Rule_Name (Instance) = "actual_parameters"
                       and then Slice_Count (Create (Param, ":")) /= 3)
            then
               Emit_Wrong_Parameter (Instance, Param);
               return;
            end if;

            if Length (Tagged_Instance.Param) /= 0 then
               Append (Tagged_Instance.Param, ",");
            end if;

            Append (Tagged_Instance.Param, To_Wide_Wide_String (Param));
            Instance.Defined_At := To_Unbounded_String (Defined_At);
         end if;

      --  Else, the parameter is not empty and the command line is disabling
      --  the instance. Thus emit an error and disable the instance.
      else
         Emit_No_Parameter_Allowed (Instance);
         Turn_Instance_Off (Instance);
      end if;
   end Array_Param_Process;

   --------------------------------
   -- Int_Or_Bools_Param_Process --
   --------------------------------

   procedure Int_Or_Bools_Param_Process
     (Rule          : Rule_Id;
      Instance_Name : String;
      Param         : String;
      Enable        : Boolean;
      Defined_At    : String)
   is
      Instance        : constant Rule_Instance_Access :=
        Get_Or_Create_Instance (Rule, Instance_Name);
      Tagged_Instance : One_Integer_Or_Booleans_Parameter_Instance renames
        One_Integer_Or_Booleans_Parameter_Instance (Instance.all);
      Int_Param_Value : Integer;
      Param_Found     : Boolean := False;
   begin
      --  If the param is empty, just enable of disable the instance following
      --  the command line. Special case for the
      --  "no_others_in_exception_handlers" rule.
      if Param = "" then
         if Enable then
            if Rule_Name (Instance) = "no_others_in_exception_handlers" then
               Emit_Required_Parameter (Instance);
               Turn_Instance_Off (Instance);
            else
               Instance.Defined_At := To_Unbounded_String (Defined_At);
            end if;
         else
            Turn_Instance_Off (Instance);
         end if;

      --  Else the param has a value but we don't know which one
      else
         if Enable then

            --  First try to extract an integer from the param
            begin
               Int_Param_Value := Integer'Value (Param);

               if Arg.Check_Redefinition.Get
                 and then Tagged_Instance.Integer_Param /= Integer'First
               then
                  Emit_Redefining (Instance, "N", Defined_At);
               end if;

               if Int_Param_Value >= 0 then
                  Tagged_Instance.Integer_Param := Int_Param_Value;
                  Instance.Defined_At := To_Unbounded_String (Defined_At);
               else
                  Emit_Wrong_Parameter (Instance, Param);
                  Turn_Instance_Off (Instance);
               end if;

               Param_Found := True;
            exception
               when Constraint_Error =>
                  null;
            end;

            --  Then find the relevant boolean if no integer has been parsed
            if not Param_Found then
               for J in 2 .. All_Rules (Rule).Parameters.Last_Child_Index
               loop
                  if Param_Name (All_Rules (Rule), J) = To_Lower (Param)
                  then
                     if Arg.Check_Redefinition.Get
                       and then Tagged_Instance.Boolean_Params (J) = On
                     then
                        Emit_Redefining (Instance, Param, Defined_At);
                     end if;

                     Tagged_Instance.Boolean_Params (J) := On;
                     Instance.Defined_At := To_Unbounded_String (Defined_At);
                     Param_Found := True;
                  end if;
               end loop;
            end if;

            --  If we get didn't find any valid parameter there is an error
            if not Param_Found then
               Emit_Wrong_Parameter (Instance, Param);
               Turn_Instance_Off (Instance);
            end if;

         --  Else, the command line is disabling the rule with a parameter.
         --  This is forbidden.
         else
            Emit_No_Parameter_Allowed (Instance);
            Turn_Instance_Off (Instance);
         end if;
      end if;
   end Int_Or_Bools_Param_Process;

   -----------------------------
   -- Id_Suffix_Param_Process --
   -----------------------------

   procedure Id_Suffix_Param_Process
     (Rule          : Rule_Id;
      Instance_Name : String;
      Param         : String;
      Enable        : Boolean;
      Defined_At    : String)
   is
      Instance        : constant Rule_Instance_Access :=
        Get_Or_Create_Instance (Rule, Instance_Name);
      Tagged_Instance : Identifier_Suffixes_Instance renames
        Identifier_Suffixes_Instance (Instance.all);
      Paren_Index     : Natural;
      Norm_Param      : constant String := Remove_Spaces (Param);
      Lower_Param     : constant String := To_Lower (Param);

      procedure Set_Field
        (Field : out Unbounded_Wide_Wide_String;
         Value : String);
      --  Set `Field` to `Value`

      ---------------
      -- Set_Field --
      ---------------

      procedure Set_Field
        (Field : out Unbounded_Wide_Wide_String;
         Value : String) is
      begin
         Set_Unbounded_Wide_Wide_String
           (Field,
            To_Wide_Wide_String (Value));
      end Set_Field;

   begin
      --  If the normalized parameter is empty, just disable the instance
      --  following the command line.
      if Norm_Param = "" then
         if not Enable then
            Turn_Instance_Off (Instance);
         end if;

      --  Else, if the command line is enabling the instance then verify and
      --  process the parameter.
      elsif Enable then
         --  Set the instance definition location
         Instance.Defined_At := To_Unbounded_String (Defined_At);

         if Lower_Param = "default" then
            Set_Field (Tagged_Instance.Type_Suffix, "_T");
            Set_Field (Tagged_Instance.Access_Suffix, "_A");
            Set_Field (Tagged_Instance.Access_Access_Suffix, "");
            Set_Field (Tagged_Instance.Class_Access_Suffix, "");
            Set_Field (Tagged_Instance.Class_Subtype_Suffix, "");
            Set_Field (Tagged_Instance.Constant_Suffix, "_C");
            Set_Field (Tagged_Instance.Renaming_Suffix, "_R");
            Set_Field (Tagged_Instance.Access_Obj_Suffix, "");
            Set_Field (Tagged_Instance.Interrupt_Suffix, "");

         elsif Has_Prefix (Norm_Param, "type_suffix=") then
            Set_Field
              (Tagged_Instance.Type_Suffix,
               Norm_Param (Norm_Param'First + 12 .. Norm_Param'Last));

         elsif Has_Prefix (Norm_Param, "access_suffix=") then
            if Norm_Param (Norm_Param'Last) = ')' then
               Paren_Index :=
                 Index
                   (Norm_Param (Norm_Param'First + 14 .. Norm_Param'Last),
                    "(");
               Set_Field
                 (Tagged_Instance.Access_Suffix,
                  Norm_Param (Norm_Param'First + 14 .. Paren_Index - 1));
               Set_Field
                 (Tagged_Instance.Access_Access_Suffix,
                  Norm_Param (Paren_Index + 1 .. Norm_Param'Last - 1));

            else
               Set_Field
                 (Tagged_Instance.Access_Suffix,
                  Norm_Param (Norm_Param'First + 14 .. Norm_Param'Last));
               Set_Field (Tagged_Instance.Access_Access_Suffix, "");
            end if;

         elsif Has_Prefix (Norm_Param, "class_access_suffix=") then
            Set_Field
              (Tagged_Instance.Class_Access_Suffix,
               Norm_Param (Norm_Param'First + 20 .. Norm_Param'Last));

         elsif Has_Prefix (Norm_Param, "class_subtype_suffix=") then
            Set_Field
              (Tagged_Instance.Class_Subtype_Suffix,
               Norm_Param (Norm_Param'First + 21 .. Norm_Param'Last));

         elsif Has_Prefix (Norm_Param, "constant_suffix=") then
            Set_Field
              (Tagged_Instance.Constant_Suffix,
               Norm_Param (Norm_Param'First + 16 .. Norm_Param'Last));

         elsif Has_Prefix (Norm_Param, "renaming_suffix=") then
            Set_Field
              (Tagged_Instance.Renaming_Suffix,
               Norm_Param (Norm_Param'First + 16 .. Norm_Param'Last));

         elsif Has_Prefix (Norm_Param, "access_obj_suffix=") then
            Set_Field
              (Tagged_Instance.Access_Obj_Suffix,
               Norm_Param (Norm_Param'First + 18 .. Norm_Param'Last));

         elsif Has_Prefix (Norm_Param, "interrupt_suffix=") then
            Set_Field
              (Tagged_Instance.Interrupt_Suffix,
               Norm_Param (Norm_Param'First + 17 .. Norm_Param'Last));

         --  If the parameter has not been matched, emit a wrong parameter
         --  error and disable the instance.
         else
            Emit_Wrong_Parameter (Instance, Param);
            Turn_Instance_Off (Instance);
         end if;

      --  Else, the command line is disabling the instance with a parameter,
      --  this is forbidden.
      else
         Emit_No_Parameter_Allowed (Instance);
         Turn_Instance_Off (Instance);
      end if;
   end Id_Suffix_Param_Process;

   -----------------------------
   -- Id_Prefix_Param_Process --
   -----------------------------

   procedure Id_Prefix_Param_Process
     (Rule          : Rule_Id;
      Instance_Name : String;
      Param         : String;
      Enable        : Boolean;
      Defined_At    : String)
   is
      Instance        : constant Rule_Instance_Access :=
        Get_Or_Create_Instance (Rule, Instance_Name);
      Tagged_Instance : Identifier_Prefixes_Instance renames
        Identifier_Prefixes_Instance (Instance.all);
      Col_Index       : Natural;
      Norm_Param      : constant String := Remove_Spaces (Param);
      Lower_Param     : constant String := To_Lower (Param);

      procedure Set_Field
        (Field : out Unbounded_Wide_Wide_String;
         Value : String);
      --  Set `Field` to `Value`

      ---------------
      -- Set_Field --
      ---------------

      procedure Set_Field
        (Field : out Unbounded_Wide_Wide_String;
         Value : String) is
      begin
         Set_Unbounded_Wide_Wide_String
           (Field,
            To_Wide_Wide_String (Value));
      end Set_Field;

   begin
      --  If the normalized param is empty, just disable the instance following
      --  the command line.
      if Norm_Param = "" then
         if not Enable then
            Turn_Instance_Off (Instance);
         end if;

      --  Else, if the command line enable the instance then check and process
      --  the parameter value.
      elsif Enable then
         --  Set the instance definition location
         Instance.Defined_At := To_Unbounded_String (Defined_At);

         if Lower_Param = "exclusive" then
            Tagged_Instance.Exclusive := On;

         elsif Has_Prefix (Norm_Param, "type=") then
            Set_Field
              (Tagged_Instance.Type_Prefix,
               Norm_Param (Norm_Param'First + 5 .. Norm_Param'Last));

         elsif Has_Prefix (Norm_Param, "concurrent=") then
            Set_Field
              (Tagged_Instance.Concurrent_Prefix,
               Norm_Param (Norm_Param'First + 11 .. Norm_Param'Last));

         elsif Has_Prefix (Norm_Param, "access=") then
            Set_Field
              (Tagged_Instance.Access_Prefix,
               Norm_Param (Norm_Param'First + 7 .. Norm_Param'Last));

         elsif Has_Prefix (Norm_Param, "class_access=") then
            Set_Field
              (Tagged_Instance.Class_Access_Prefix,
               Norm_Param (Norm_Param'First + 13 .. Norm_Param'Last));

         elsif Has_Prefix (Norm_Param, "subprogram_access=") then
            Set_Field
              (Tagged_Instance.Subprogram_Access_Prefix,
               Norm_Param (Norm_Param'First + 18 .. Norm_Param'Last));

         elsif Has_Prefix (Norm_Param, "derived=") then
            Col_Index :=
              Index
                (Norm_Param (Norm_Param'First + 8 .. Norm_Param'Last), ":");

            if Col_Index /= 0 then
               if Length (Tagged_Instance.Derived_Prefix) /= 0 then
                  Append (Tagged_Instance.Derived_Prefix, ",");
               end if;

               Append
                 (Tagged_Instance.Derived_Prefix,
                  To_Wide_Wide_String
                    (To_Lower
                       (Norm_Param (Norm_Param'First + 8 .. Col_Index - 1))));
               Append
                 (Tagged_Instance.Derived_Prefix,
                  To_Wide_Wide_String
                    (Norm_Param (Col_Index .. Norm_Param'Last)));

            else
               Emit_Wrong_Parameter (Instance, Param);
               Turn_Instance_Off (Instance);
            end if;

         elsif Has_Prefix (Norm_Param, "constant=") then
            Set_Field
              (Tagged_Instance.Constant_Prefix,
               Norm_Param (Norm_Param'First + 9 .. Norm_Param'Last));

         elsif Has_Prefix (Norm_Param, "exception=") then
            Set_Field
              (Tagged_Instance.Exception_Prefix,
               Norm_Param (Norm_Param'First + 10 .. Norm_Param'Last));

         elsif Has_Prefix (Norm_Param, "enum=") then
            Set_Field
              (Tagged_Instance.Enum_Prefix,
               Norm_Param (Norm_Param'First + 5 .. Norm_Param'Last));

         --  If the param hasn't been matched, emit an error and disable the
         --  instance.
         else
            Emit_Wrong_Parameter (Instance, Param);
            Turn_Instance_Off (Instance);
         end if;

      --  Else, the command line is disabling the instance with a parameter,
      --  this is forbidden.
      else
         Emit_No_Parameter_Allowed (Instance);
         Turn_Instance_Off (Instance);
      end if;
   end Id_Prefix_Param_Process;

   -----------------------------
   -- Id_Casing_Param_Process --
   -----------------------------

   procedure Id_Casing_Param_Process
     (Rule          : Rule_Id;
      Instance_Name : String;
      Param         : String;
      Enable        : Boolean;
      Defined_At    : String)
   is
      Instance        : constant Rule_Instance_Access :=
        Get_Or_Create_Instance (Rule, Instance_Name);
      Tagged_Instance : Identifier_Casing_Instance renames
        Identifier_Casing_Instance (Instance.all);
      Norm_Param      : constant String := To_Lower (Remove_Spaces (Param));

      procedure Check_And_Set
        (S     : in out Unbounded_Wide_Wide_String;
         Val   : String;
         Label : String);
      --  Check the parameter redefinition for `S` and set the `S` to the given
      --  `Val`. `Label` is the name of the parameter, it is used for error
      --  emission.

      procedure Check_And_Set
        (S     : in out Unbounded_Wide_Wide_String;
         Val   : String;
         Label : String) is
      begin
         if Arg.Check_Redefinition.Get and then Length (S) /= 0 then
            Emit_Redefining (Instance, Label, Defined_At);
         end if;

         Set_Unbounded_Wide_Wide_String (S, To_Wide_Wide_String (Val));
      end Check_And_Set;

   begin
      --  If the normalized parameter is empty then just disable the instance
      --  following the command line.
      if Norm_Param = "" then
         if not Enable then
            Turn_Instance_Off (Instance);
         end if;

      --  Else, if the command line enable the instance, check the parameter
      --  and update the instance.
      elsif Enable then
         --  Set the instance definition location
         Instance.Defined_At := To_Unbounded_String (Defined_At);

         if Has_Prefix (Norm_Param, "type=") then
            Check_And_Set
              (Tagged_Instance.Type_Casing,
               Norm_Param (Norm_Param'First + 5 .. Norm_Param'Last),
               "type");

         elsif Has_Prefix (Norm_Param, "enum=") then
            Check_And_Set
              (Tagged_Instance.Enum_Casing,
               Norm_Param (Norm_Param'First + 5 .. Norm_Param'Last),
               "enumeration literal");

         elsif Has_Prefix (Norm_Param, "constant=") then
            Check_And_Set
              (Tagged_Instance.Constant_Casing,
               Norm_Param (Norm_Param'First + 9 .. Norm_Param'Last),
               "constant");

         elsif Has_Prefix (Norm_Param, "exception=") then
            Check_And_Set
              (Tagged_Instance.Exception_Casing,
               Norm_Param (Norm_Param'First + 10 .. Norm_Param'Last),
               "exception");

         elsif Has_Prefix (Norm_Param, "others=") then
            Check_And_Set
              (Tagged_Instance.Others_Casing,
               Norm_Param (Norm_Param'First + 7 .. Norm_Param'Last),
               "others");

         elsif Has_Prefix (Norm_Param, "exclude=") then
            if Load_Dictionary
              (Instance,
               Expand_Env_Variables
                 (Norm_Param (Norm_Param'First + 8 .. Norm_Param'Last)),
               Tagged_Instance.Exclude)
            then
               Set_Unbounded_Wide_Wide_String
                 (Tagged_Instance.Exclude_File,
                  To_Wide_Wide_String (Norm_Param
                    (Norm_Param'First + 8 .. Norm_Param'Last)));
            else
               Turn_Instance_Off (Instance);
            end if;

         --  If the parameter hasn't been processed here, then the param value
         --  is wrong. Emit an error and disable the instance.
         else
            Emit_Wrong_Parameter (Instance, Param);
            Turn_Instance_Off (Instance);
         end if;

      --  Else, the command line is disabling the instance and the parameter
      --  is not empty. This is forbidden so emit an error message and disable
      --  the instance.
      else
         Emit_No_Parameter_Allowed (Instance);
         Turn_Instance_Off (Instance);
      end if;
   end Id_Casing_Param_Process;

   -----------------------------
   -- Forbidden_Param_Process --
   -----------------------------

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

   procedure Forbidden_Param_Process
     (Rule          : Rule_Id;
      Instance_Name : String;
      Param         : String;
      Enable        : Boolean;
      Defined_At    : String)
   is
      Instance        : constant Rule_Instance_Access :=
        Get_Or_Create_Instance (Rule, Instance_Name);
      Tagged_Instance : Forbidden_Instance renames
        Forbidden_Instance (Instance.all);
      Lower_Param     : constant String := To_Lower (Param);

      procedure Process_Param
        (Param : String;
         Field : in out Unbounded_Wide_Wide_String);
      --  Process `Lower_Param` to update the `Field` according to it

      procedure Process_Allowed_List (List : String);
      --  Process `List` as a list of allowed items and add them in the current
      --  instance.

      -------------------
      -- Process_Param --
      -------------------

      procedure Process_Param
        (Param : String;
         Field : in out Unbounded_Wide_Wide_String)
      is
         Rule_Name : constant String := Gnatcheck.Rules.Rule_Name (Instance);
      begin
         --  Add a comma to the field if needed
         if Length (Field) /= 0 then
            Append (Field, ",");
         end if;

         --  Special case: If the param is "gnat" and the rule is
         --  "forbidden_attributes" or "forbidden_pragmas" then add to `Field`
         --  the GNAT defined attributes or pragmas.
         if Param = "gnat" then
            if Rule_Name = "forbidden_attributes" then
               Append (Field, GNAT_Attributes);
            elsif Rule_Name = "forbidden_pragmas" then
               Append (Field, GNAT_Pragmas);
            else
               Append (Field, To_Wide_Wide_String (Param));
            end if;

         --  Other cases: Just add the param to `Field`
         else
            Append (Field, To_Wide_Wide_String (Param));
         end if;
      end Process_Param;

      --------------------------
      -- Process_Allowed_List --
      --------------------------

      procedure Process_Allowed_List (List : String) is
         Sep_Index : Natural := Index (List, ";");
         Item_Start : Natural := List'First;
         Item_End : Natural :=
           (if Sep_Index = 0 then List'Last else Sep_Index - 1);
      begin
         while Item_Start < List'Last loop
            Process_Param
              (List (Item_Start .. Item_End), Tagged_Instance.Allowed);
            Item_Start := Item_End + 2;
            Sep_Index := Index (List (Item_Start .. List'Last), ";");
            Item_End := (if Sep_Index = 0 then List'Last else Sep_Index - 1);
         end loop;
      end Process_Allowed_List;

   begin
      --  If the parameter is empty then just disable the instance following
      --  the command line.
      if Param = "" then
         if not Enable then
            Turn_Instance_Off (Instance);
         end if;

      --  Else, the parameter is not empty. If the command line is enabling the
      --  instance then process the parameter to update the instance.
      elsif Enable then
         if Lower_Param = "all" then
            Tagged_Instance.All_Flag := On;
         elsif Has_Prefix (Lower_Param, "allowed=") then
            Process_Allowed_List
              (Lower_Param (Lower_Param'First + 8 .. Lower_Param'Last));
         else
            Process_Param (Lower_Param, Tagged_Instance.Forbidden);
         end if;
         Instance.Defined_At := To_Unbounded_String (Defined_At);

      --  Else, the command line is disabling the instance with a non-empty
      --  parameter. This is forbidden so emit an error and disable the
      --  instance.
      else
         Emit_No_Parameter_Allowed (Instance);
         Turn_Instance_Off (Instance);
      end if;
   end Forbidden_Param_Process;

   --------------------------------------
   -- Silent_Exc_Handler_Param_Process --
   --------------------------------------

   procedure Silent_Exc_Handler_Param_Process
     (Rule          : Rule_Id;
      Instance_Name : String;
      Param         : String;
      Enable        : Boolean;
      Defined_At    : String)
   is
      Instance        : constant Rule_Instance_Access :=
        Get_Or_Create_Instance (Rule, Instance_Name);
      Tagged_Instance : Silent_Exception_Handlers_Instance renames
        Silent_Exception_Handlers_Instance (Instance.all);

      procedure Add_To
        (Str : in out Unbounded_Wide_Wide_String;
         Val : String);
      --  Add `Val` to `Str`, separated with ","

      ------------
      -- Add_To --
      ------------

      procedure Add_To
        (Str : in out Unbounded_Wide_Wide_String;
         Val : String) is
      begin
         if Length (Str) /= 0 then
            Append (Str, ",");
         end if;
         Append (Str, To_Wide_Wide_String (Val));
      end Add_To;

   begin
      --  If the parameter is empty then just disable the instance following
      --  the command line.
      if Param = "" then
         if not Enable then
            Turn_Instance_Off (Instance);
         end if;

      --  Else, the parameter is not empty. If the command line is enabling the
      --  instance then process the parameter to update the instance.
      elsif Enable then
         if Param (Param'First) = '"' then
            Add_To (Tagged_Instance.Subprogram_Regexps,
                    Param (Param'First + 1 .. Param'Last - 1));
         else
            Add_To (Tagged_Instance.Subprograms, To_Lower (Param));
         end if;
         Instance.Defined_At := To_Unbounded_String (Defined_At);

      --  Else, the command line is disabling the instance with a non-empty
      --  parameter. This is forbidden so emit an error message and disable
      --  the instance.
      else
         Emit_No_Parameter_Allowed (Instance);
         Turn_Instance_Off (Instance);
      end if;
   end Silent_Exc_Handler_Param_Process;

   --------------------------
   -- Custom_Param_Process --
   --------------------------

   procedure Custom_Param_Process
     (Rule          : Rule_Id;
      Instance_Name : String;
      Param         : String;
      Enable        : Boolean;
      Defined_At    : String)
   is
      Instance        : constant Rule_Instance_Access :=
        Get_Or_Create_Instance (Rule, Instance_Name);
      Tagged_Instance : Custom_Instance renames
        Custom_Instance (Instance.all);
      R_Name          : constant String := Rule_Name (Rule);
      First_Equal     : Natural;
      Found           : Boolean := False;
   begin
      --  If the parameter is empty then disable the instance following the
      --  command line.
      if Param = "" then
         if not Enable then
            Turn_Instance_Off (Instance);
         end if;

      --  Else, the parameter is not empty. If the command line is enabling the
      --  instance then process the parameter.
      elsif Enable then
         --  Special case for the "USE_Clauses" rule
         if R_Name = "use_clauses" then
            if To_Lower (Param) = "exempt_operator_packages" then
               if Arg.Check_Redefinition.Get
                 and then not Tagged_Instance.Arguments.Is_Empty
               then
                  Emit_Redefining (Instance, Param, Defined_At);
               else
                  Instance.Defined_At := To_Unbounded_String (Defined_At);
                  Tagged_Instance.Arguments.Append
                    (Rule_Argument'
                       (To_Unbounded_Text ("exempt_operator_packages"),
                        To_Unbounded_Text ("true")));
               end if;
            else
               Emit_Wrong_Parameter (Instance, Param);
               Turn_Instance_Off (Instance);
            end if;
            return;
         end if;

         Instance.Defined_At := To_Unbounded_String (Defined_At);

         --  Get the first "=" index, if this index is 0 then there is an error
         --  in the parameter syntax.
         First_Equal := Index (Param, "=");
         if First_Equal = 0 then
            Error
              ("(" & Gnatcheck.Rules.Instance_Name (Instance) &
               ") missing = in parameter argument: " & Param);
            Bad_Rule_Detected := True;
            Turn_Instance_Off (Instance);
            return;
         end if;

         --  Get the parameter name then check it is a valid parameter for
         --  the rule by iterating over rule's formal parameters.
         declare
            Param_Name : constant String :=
              To_Lower (Param (Param'First .. First_Equal - 1));
         begin
            for J in 1 .. All_Rules (Rule).Parameters.Last_Child_Index loop
               if Gnatcheck.Rules.Param_Name
                 (All_Rules (Rule), J) = Param_Name
               then
                  Found := True;
                  exit;
               end if;
            end loop;

            --  If the parameter name is valid, append the actual value to the
            --  instance's `Arguments`.
            if Found then
               Tagged_Instance.Arguments.Append
                 (Rule_Argument'
                    (Name  => To_Unbounded_Text (To_Text (Param_Name)),
                     Value => To_Unbounded_Text (To_Text
                       (Param (First_Equal + 1 .. Param'Last)))));

            --  Else, display an error and disable the instance
            else
               Error
                 ("(" & Gnatcheck.Rules.Instance_Name (Instance) &
                  ") unknown parameter: " & Param_Name);
               Bad_Rule_Detected := True;
               Turn_Instance_Off (Instance);
            end if;
         end;

      --  Else, the command line is disabling the instance with a non-empty
      --  parameter. This is forbidden so emit an error message and disable
      --  the instance.
      else
         Emit_No_Parameter_Allowed (Instance);
         Turn_Instance_Off (Instance);
      end if;
   end Custom_Param_Process;

   --  == Actual parameter mapping helpers

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
               Value => To_Unbounded_Text (To_Text (Image (Value)))));
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
               Append (Param, """, """);
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
     (Args     : in out Rule_Argument_Vectors.Vector;
      Instance : in out One_Array_Parameter_Instance)
   is
      Rule : constant Rule_Info := All_Rules (Instance.Rule);
      Last : constant Natural := Length (Instance.Param);

      procedure Error;
      --  Emit an error message when an invalid parameter is detected.

      function Find_Char
        (Str      : Wide_Wide_String;
         C        : Wide_Wide_Character;
         Backward : Boolean := False) return Natural;
      --  Return the first occurrence of `C` in `Str`, 0 if none
      --  If Backward is True, search from the end of the string.

      -----------
      -- Error --
      -----------

      procedure Error is
      begin
         Gnatcheck.Output.Error
           ("(" & Instance_Name (Instance) & ") wrong parameter: " &
            To_String (Instance.Param));
         Turn_Instance_Off (Instance_Name (Instance));
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

      --  If the rule is "parameters_out_of_order" then check that we have 5
      --  parameters.
      if Rule.Name = "parameters_out_of_order"
        and then Last /= 0
        and then Slice_Count (Create (To_String (Instance.Param), ",")) /= 5
      then
         Error
           ("(" & Instance_Name (Instance) & ") requires 5 parameters, got: " &
            To_String (Instance.Param));
         Bad_Rule_Detected := True;
         Turn_Instance_Off (Instance_Name (Instance));
         return;
      end if;

      --  If the rule is "actual_parameters" then add the instance parameter
      --  in the argument vector as string tuples.
      if Rule.Name = "actual_parameters" and then Last /= 0 then
         declare
            Param        : Unbounded_Wide_Wide_String;
            C            : Wide_Wide_Character;
            Num_Elements : Positive := 1;
            Lower        : Boolean  := True;
         begin
            Append (Param, "[(""");

            for J in 1 .. Last loop
               C := Element (Instance.Param, J);

               case C is
                  when '"'    =>
                     if Num_Elements = 3 then
                        if Element (Instance.Param, J - 1) = ':' then
                           Append (Param, '|');
                           Lower := False;

                        elsif J /= Last
                          and then Element (Instance.Param, J + 1) /= ','
                        then
                           Error;
                           return;
                        end if;
                     else
                        Append (Param, "\""");
                     end if;

                  when ':'    =>
                     Append (Param, """, """);
                     Num_Elements := @ + 1;
                     Lower := True;

                  when ','    =>
                     Append (Param, """), (""");
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

      --  If the rule is "exception_propagation_from_callbacks" then add
      --  the instance parameter to the argument vector as string tuples.
      elsif Rule.Name = "exception_propagation_from_callbacks"
        and then Last /= 0
      then
         declare
            Param      : Unbounded_Wide_Wide_String;
            Str        : constant Wide_Wide_String :=
              To_Wide_Wide_String (Instance.Param);
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

               Dot := Find_Char
                 (Str (Current .. Next_Comma - 1), '.', Backward => True);

               if Dot = 0 then
                  Error;
                  return;
               end if;

               Append (Param, To_Lower (Str (Current .. Dot - 1)));
               Append (Param, """, """);
               Append (Param, To_Lower (Str (Dot + 1 .. Next_Comma - 1)));
               Current := Next_Comma + 1;

               exit when Current > Str'Last;

               Append (Param, """), (""");
            end loop;

            Append (Param, """)]");
            Args.Append
              (Rule_Argument'
                (Name  => To_Unbounded_Text (Param_Name (Rule, 2)),
                 Value => To_Unbounded_Text (To_Wide_Wide_String (Param))));
         end;

      --  In other cases, just add the instance parameter as a comma separated
      --  string array.
      else
         Append_Array_Param (Args, Param_Name (Rule, 2), Instance.Param);
      end if;
   end Handle_Array_Param;

   --  ===== Package subprograms bodies =====

   --  == Rule info operations

   -----------------
   -- Create_Rule --
   -----------------

   function Create_Rule
     (Param_Kind : Rule_Param_Kind;
      Rule_Name  : String) return Rule_Info
   is
      Res : Rule_Info;
   begin
      Res.Allowed_As_Exemption_Parameter := No_Exemption_Param_Allowed'Access;
      Res.Rule_Param_From_Diag := No_Param_From_Diag'Access;

      case Param_Kind is
         when No_Param =>
            Res.XML_Rule_Help := No_Param_XML_Help'Access;
            Res.Create_Instance := Create_No_Param_Instance'Access;
            Res.Process_Rule_Parameter := No_Param_Process'Access;
         when One_Integer =>
            Res.XML_Rule_Help := Int_Param_XML_Help'Access;
            Res.Create_Instance := Create_Int_Instance'Access;
            Res.Process_Rule_Parameter := Int_Param_Process'Access;
         when One_Boolean =>
            Res.XML_Rule_Help := Bool_Param_XML_Help'Access;
            Res.Create_Instance := Create_Bool_Instance'Access;
            Res.Process_Rule_Parameter := Bool_Param_Process'Access;
         when One_String =>
            Res.XML_Rule_Help := String_Param_XML_Help'Access;
            Res.Create_Instance := Create_String_Instance'Access;
            Res.Process_Rule_Parameter := String_Param_Process'Access;
         when One_Array =>
            Res.XML_Rule_Help := String_Param_XML_Help'Access;
            Res.Create_Instance := Create_Array_Instance'Access;
            Res.Process_Rule_Parameter := Array_Param_Process'Access;
         when One_Integer_Or_Booleans =>
            Res.XML_Rule_Help := No_Param_XML_Help'Access;
            Res.Create_Instance := Create_Int_Or_Bools_Instance'Access;
            Res.Process_Rule_Parameter := Int_Or_Bools_Param_Process'Access;
         when Custom =>
            if Rule_Name = "identifier_suffixes" then
               Res.XML_Rule_Help := Id_Suffix_Param_XML_Help'Access;
               Res.Allowed_As_Exemption_Parameter :=
                 Id_Suffix_Allowed_Exemption_Param'Access;
               Res.Rule_Param_From_Diag := Id_Suffix_Param_From_Diag'Access;
               Res.Create_Instance := Create_Id_Suffix_Instance'Access;
               Res.Process_Rule_Parameter := Id_Suffix_Param_Process'Access;

            elsif Rule_Name = "identifier_prefixes" then
               Res.XML_Rule_Help := Id_Prefix_Param_XML_Help'Access;
               Res.Allowed_As_Exemption_Parameter :=
                 Id_Prefix_Allowed_Exemption_Param'Access;
               Res.Rule_Param_From_Diag := Id_Prefix_Param_From_Diag'Access;
               Res.Create_Instance := Create_Id_Prefix_Instance'Access;
               Res.Process_Rule_Parameter := Id_Prefix_Param_Process'Access;

            elsif Rule_Name = "identifier_casing" then
               Res.XML_Rule_Help := Id_Casing_Param_XML_Help'Access;
               Res.Allowed_As_Exemption_Parameter :=
                 Id_Casing_Allowed_Exemption_Param'Access;
               Res.Rule_Param_From_Diag := Id_Casing_Param_From_Diag'Access;
               Res.Create_Instance := Create_Id_Casing_Instance'Access;
               Res.Process_Rule_Parameter := Id_Casing_Param_Process'Access;

            elsif Rule_Name = "silent_exception_handlers" then
               Res.XML_Rule_Help := Silent_Exc_Handler_Param_XML_Help'Access;
               Res.Create_Instance :=
                 Create_Silent_Exc_Handler_Instance'Access;
               Res.Process_Rule_Parameter :=
                 Silent_Exc_Handler_Param_Process'Access;

            elsif Rule_Name = "forbidden_attributes"
               or else Rule_Name = "forbidden_pragmas"
               or else Rule_Name = "forbidden_aspects"
            then
               Res.XML_Rule_Help := Forbidden_Param_XML_Help'Access;
               Res.Allowed_As_Exemption_Parameter :=
                 All_Exemption_Parameter_Allowed'Access;
               Res.Rule_Param_From_Diag := Forbidden_Param_From_Diag'Access;
               Res.Create_Instance := Create_Forbidden_Instance'Access;
               Res.Process_Rule_Parameter := Forbidden_Param_Process'Access;

            else
               if Rule_Name = "use_clauses" then
                  Res.XML_Rule_Help := Bool_Param_XML_Help'Access;
               else
                  Res.XML_Rule_Help := No_Param_XML_Help'Access;
               end if;
               Res.Create_Instance := Create_Custom_Instance'Access;
               Res.Process_Rule_Parameter := Custom_Param_Process'Access;
            end if;
      end case;
      return Res;
   end Create_Rule;

   ---------------
   -- Rule_Name --
   ---------------

   function Rule_Name (Rule : Rule_Info) return String is
   begin
      return To_String (Rule.Name);
   end Rule_Name;

   ----------------
   -- Is_Enabled --
   ----------------

   function Is_Enabled (Rule : Rule_Info) return Boolean is
   begin
      return Natural (Rule.Instances.Length) > 0;
   end Is_Enabled;

   ---------------------
   -- Print_Rule_Help --
   ---------------------

   procedure Print_Rule_Help (Rule : Rule_Info) is
   begin
      Print
        (" " & Rule_Name (Rule) & " - " & To_String (Rule.Help_Info) & " - " &
         Rule.Remediation_Level'Img);
   end Print_Rule_Help;

   ---------------------------------------
   -- Print_Rule_Instances_To_LKQL_File --
   ---------------------------------------

   procedure Print_Rule_Instances_To_LKQL_File
     (Rule       : Rule_Info;
      Rule_File  : File_Type;
      Mode       : Source_Modes;
      For_Worker : Boolean;
      First_Rule : in out Boolean)
   is
      Mode_String : constant String :=
        (case Mode is
            when General    => "rules",
            when Ada_Only   => "ada_rules",
            when Spark_Only => "spark_rules");

      Instance_Names : String_Vector;
      Instance       : Rule_Instance_Access;
      Args           : Rule_Argument_Vectors.Vector;

      First_Instance : Boolean := True;
      First_Param    : Boolean := True;

      procedure Postprocess_Args;
      --  Perform the required post-processing on mapped args to emit an LKQL
      --  file usable by GNATcheck.

      procedure Postprocess_Args is
         Lower_Rule_Name : constant String := To_Lower (Rule_Name (Rule));
      begin
         --  For "headers" and "name_clashes" rules, set the argument value to
         --  the name of the file.
         if Lower_Rule_Name = "headers"
           or else Lower_Rule_Name = "name_clashes"
         then
            declare
               Str_Instance : constant One_String_Parameter_Instance :=
                 One_String_Parameter_Instance (Instance.all);
               Filename : constant Text_Type :=
                 To_Text (To_String (Str_Instance.File));
            begin
               Set_Unbounded_Wide_Wide_String
                 (Args (1).Value, '"' & Filename & '"');
            end;

         --  For the "identifier_suffixes" rule, process the "access_suffix"
         --  arg to format it like `<access_suffix>(<access_access_suffix>)`.
         elsif Lower_Rule_Name = "identifier_suffixes" then
            declare
               Id_Suf_Instance : constant Identifier_Suffixes_Instance :=
                 Identifier_Suffixes_Instance (Instance.all);
               To_Delete : Natural := 0;
               Acc_Suf_Value : constant Text_Type := To_Text
                 ((if Length (Id_Suf_Instance.Access_Access_Suffix) = 0
                   then Id_Suf_Instance.Access_Suffix
                   else Id_Suf_Instance.Access_Suffix &
                       '(' & Id_Suf_Instance.Access_Access_Suffix & ')'));
            begin
               for I in Args.First_Index .. Args.Last_Index loop
                  if Args (I).Name = "access_suffix" then
                     Set_Unbounded_Wide_Wide_String
                       (Args (I).Value, '"' & Acc_Suf_Value & '"');
                  elsif Args (I).Name = "access_access_suffix" then
                     To_Delete := I;
                  end if;
               end loop;
               if To_Delete /= 0 then
                  Args.Delete (To_Delete);
               end if;
            end;

         --  For the "identifier_casing" rule, if there is a "exclude" file,
         --  add it into the argument list.
         elsif Lower_Rule_Name = "identifier_casing" then
            declare
               Id_Cas_Instance : constant Identifier_Casing_Instance :=
                 Identifier_Casing_Instance (Instance.all);
               Filename : constant Text_Type :=
                 To_Text (Id_Cas_Instance.Exclude_File);
            begin
               for I in Args.First_Index .. Args.Last_Index loop
                  if Args (I).Name = "exclude" then
                     Set_Unbounded_Wide_Wide_String
                       (Args (I).Value, '"' & Filename & '"');
                  end if;
               end loop;
            end;

         --  For the "silent_exception_handlers", process all subprogram
         --  regexps to add a '|' char before
         elsif Lower_Rule_Name = "silent_exception_handlers" then
            declare
               Subp_Value : Unbounded_Text_Type;
            begin
               for I in Args.First_Index .. Args.Last_Index loop
                  if Args (I).Name = "subprograms" then
                     Append (Subp_Value, Slice
                       (Args (I).Value, 2, Length (Args (I).Value) - 1));
                  elsif Args (I).Name = "subprogram_regexps" then
                     for R of Parse_LKQL_List (To_String (Args (I).Value)) loop
                        if Length (Subp_Value) /= 0 then
                           Append (Subp_Value, ", ");
                        end if;
                        Append (Subp_Value, To_Text (Insert (R, 2, "|")));
                     end loop;
                  end if;
               end loop;

               Args.Clear;
               Args.Append (Rule_Argument'
                 (Name  => To_Unbounded_Text ("subprograms"),
                  Value => To_Unbounded_Text
                    ('[' & To_Text (Subp_Value) & ']')));
            end;
         end if;
      end Postprocess_Args;
   begin
      --  Get all instances to print into the LKQL file
      for Instance of Rule.Instances loop
         if Instance.Source_Mode = Mode then
            Instance_Names.Append (To_Lower (Instance_Name (Instance)));
         end if;
      end loop;

      --  Then iterate oven them to output them if there are some
      if not Instance_Names.Is_Empty then
         --  If this is the first rule to be displayed, print the object
         --  declaration.
         if First_Rule then
            if Mode /= General then
               Put_Line (Rule_File, "val " & Mode_String & " = @{");
            end if;
            First_Rule := False;

         --  Else, print a comma
         else
            Put_Line (Rule_File, ",");
         end if;
         Put (Rule_File, "    " & Rule_Name (Rule));

         for Name of Instance_Names loop
            Instance    := All_Rule_Instances (Name);
            First_Param := True;
            Args.Clear;
            Map_Parameters (Instance.all, Args);
            if not For_Worker then
               Postprocess_Args;
            end if;

            if not Args.Is_Empty
              or else Instance.Is_Alias
              or else Natural (Instance_Names.Length) > 1
            then
               if First_Instance then
                  First_Instance := False;
                  Put (Rule_File, ": [{");
               else
                  Put (Rule_File, ", {");
               end if;

               --  Print the poential instance name
               if Instance.Is_Alias then
                  Put
                    (Rule_File,
                     "instance_name: """ & To_String (Instance.Alias_Name) &
                     """");
                  First_Param := False;
               end if;

               --  Then print the instance arguments
               for Param of Args loop
                  if First_Param then
                     First_Param := False;
                  else
                     Put (Rule_File, ", ");
                  end if;
                  Put
                    (Rule_File,
                     To_String (To_Wide_Wide_String (Param.Name)) & ": " &
                     To_String (To_Wide_Wide_String (Param.Value)));
               end loop;

               Put (Rule_File, "}");
            end if;
         end loop;

         if not First_Instance then
            Put (Rule_File, "]");
         end if;
      end if;
   end Print_Rule_Instances_To_LKQL_File;

   --------------------------------------
   -- Print_Compiler_Rule_To_LKQL_File --
   --------------------------------------

   procedure Print_Compiler_Rule_To_LKQL_File
     (Compiler_Rule : Rule_Id;
      Rule_File     : File_Type;
      First_Rule    : in out Boolean)
   is
   begin
      --  First of all, check that the rule is enabled
      if not Is_Enabled (Compiler_Rule) then
         return;
      end if;

      --  If this is not the first rule to be printed, add a comma
      --  before anything.
      if First_Rule then
         First_Rule := False;
      else
         Put_Line (Rule_File, ",");
      end if;
      Put (Rule_File, "    " & Rule_Name (Compiler_Rule) & ": [{arg: ");

      --  Print the rule options
      if Compiler_Rule = Style_Checks_Id then
         Put (Rule_File, """" & Get_Specified_Style_Option & """");
      elsif Compiler_Rule = Warnings_Id then
         Put (Rule_File, """" & Get_Specified_Warning_Option & """");
      else
         Put
           (Rule_File,
            "[" &
            Active_Restrictions_List
              (Separator => ", ",
               Elem_Prefix => """",
               Elem_Postfix => """") &
            "]");
      end if;

      --  Close the instances list
      Put (Rule_File, "}]");
   end Print_Compiler_Rule_To_LKQL_File;

   --  == Generic operations on rule instances

   ---------------
   -- Rule_Name --
   ---------------

   function Rule_Name (Instance : Rule_Instance'Class) return String is
   begin
      return Rule_Name (Instance.Rule);
   end Rule_Name;

   -------------------
   -- Instance_Name --
   -------------------

   function Instance_Name (Instance : Rule_Instance'Class) return String is
   begin
      return (if Instance.Is_Alias
              then To_String (Instance.Alias_Name)
              else Rule_Name (Instance));
   end Instance_Name;

   -------------------
   -- Annotate_Diag --
   -------------------

   function Annotate_Diag (Instance : Rule_Instance'Class) return String is
   begin
      if Arg.Show_Rule.Get then
         return " [" &
                (if Instance.Is_Alias
                 then To_String (Instance.Alias_Name) & "|"
                 else "") &
                Rule_Name (Instance) & "]";
      else
         return "";
      end if;
   end Annotate_Diag;

   --  == Overriding operations on rule instances

   ------------------------------------
   -- Process_Instance_Params_Object --
   ------------------------------------

   overriding procedure Process_Instance_Params_Object
     (Instance      : in out One_Integer_Parameter_Instance;
      Params_Object : in out JSON_Value)
   is
      P_Name : constant String := Param_Name (Instance, 2);
   begin
      Instance.Param := Expect_Literal (Params_Object, P_Name);
      Params_Object.Unset_Field (P_Name);
   end Process_Instance_Params_Object;

   overriding procedure Process_Instance_Params_Object
     (Instance      : in out One_Boolean_Parameter_Instance;
      Params_Object : in out JSON_Value)
   is
      P_Name : constant String := Param_Name (Instance, 2);
   begin
      if Params_Object.Has_Field (P_Name) then
         Instance.Param :=
           From_Boolean (Expect_Literal (Params_Object, P_Name));
         Params_Object.Unset_Field (P_Name);
      end if;
   end Process_Instance_Params_Object;

   overriding procedure Process_Instance_Params_Object
     (Instance      : in out One_String_Parameter_Instance;
      Params_Object : in out JSON_Value)
   is
      P_Name : constant String := Param_Name (Instance, 2);
   begin
      --  Handle the "headers" rule in a special way since the argument should
      --  be a file name.
      if Rule_Name (Instance) = "headers" then
         declare
            File_Name : constant String :=
              Expect_Literal (Params_Object, P_Name);
         begin
            Params_Object.Unset_Field (P_Name);
            if not Instance.Load_File (To_Load => File_Name) then
               raise Invalid_Value with "cannot load file " & File_Name;
            end if;
         end;

      --  Else, handle the parameter as a simple string
      else
         Set_Unbounded_Wide_Wide_String
            (Instance.Param,
            To_Wide_Wide_String
              (Expect_Literal (Params_Object, P_Name)));
         Params_Object.Unset_Field (P_Name);
      end if;
   end Process_Instance_Params_Object;

   overriding procedure Process_Instance_Params_Object
     (Instance      : in out One_Array_Parameter_Instance;
      Params_Object : in out JSON_Value)
   is
      P_Name : constant String := Param_Name (Instance, 2);
   begin
      --  If the rule is "name_clashes", then load the provided file a
      --  dictionary file.
      if Rule_Name (Instance) = "name_clashes" then
         declare
            Param_Value : constant String :=
              Expect_Literal (Params_Object, "dictionary_file");
            File_Content : constant Unbounded_String :=
              Load_Dictionary_File (Expand_Env_Variables (Param_Value));
         begin
            Params_Object.Unset_Field ("dictionary_file");
            if File_Content /= Null_Unbounded_String then
               Set_Unbounded_String (Instance.File, Param_Value);
               Set_Unbounded_Wide_Wide_String
                 (Instance.Param,
                  To_Wide_Wide_String (To_String (File_Content)));
            else
               raise Invalid_Value with "cannot load file " & Param_Value;
            end if;
         end;

      --  Else, handle the real array parametrized rules
      elsif Params_Object.Has_Field (P_Name) then
         declare
            Param_Value : constant String_Vector :=
               Expect_Literal (Params_Object, P_Name);
            Res : String_Vector;
         begin

            --  Special case for the "parameters_out_of_order" rule to verify
            --  that the argument value contains valid values.
            if Rule_Name (Instance) = "parameters_out_of_order" then
               for S of Param_Value loop
                  if To_Lower (S) not in
                    "in" | "defaulted_in" | "in_out" | "access" | "out"
                  then
                     raise Invalid_Value with
                       "'" & P_Name & "' should contains only 'in', " &
                       "'defaulted_in', 'in_out', 'access' or 'out' strings";
                  end if;
               end loop;

            --  Special case for the "actual_parameters" rule which should
            --  have a list of three-elem tuples of strings.
            elsif Rule_Name (Instance) = "actual_parameters" then
               begin
                  for S of Param_Value loop
                     Res.Append (Join (Parse_String_Tuple (S), ":"));
                  end loop;
               exception
                  when Invalid_Type =>
                     raise Invalid_Type with
                       "'" & P_Name & "' should be a list of string " &
                       "tuples";
               end;

            --  Special case for the "exception_propagation_from_callbacks"
            --  rule which should have a list of two-elems tuples of strings.
            elsif Rule_Name (Instance) = "exception_propagation_from_callbacks"
            then
               begin
                  for S of Param_Value loop
                     Res.Append (Join (Parse_String_Tuple (S), "."));
                  end loop;
               exception
                  when Invalid_Type =>
                     raise Invalid_Type with
                       "'" & P_Name & "' should be a list of string " &
                       "tuples";
               end;

            --  Else the rule parameter is just a list of strings
            else
               Res := Param_Value;
            end if;
            Set_Unbounded_Wide_Wide_String
              (Instance.Param, To_Wide_Wide_String (Join (Res, ",")));
         end;
         Params_Object.Unset_Field (P_Name);
      end if;
   end Process_Instance_Params_Object;

   overriding procedure Process_Instance_Params_Object
     (Instance      : in out One_Integer_Or_Booleans_Parameter_Instance;
      Params_Object : in out JSON_Value) is
   begin
      --  Iterate over all rules parameters and try getting it from the
      --  arguments object.
      for I in 2 .. All_Rules (Instance.Rule).Parameters.Last_Child_Index loop
         if Params_Object.Has_Field (Param_Name (Instance, I)) then
            --  Try getting the parameter as an integer
            begin
               Instance.Integer_Param :=
                 Expect_Literal (Params_Object, Param_Name (Instance, I));

            --  If it fails, then the argument should be a boolean
            exception
               when Invalid_Type =>
                  Instance.Boolean_Params (I) :=
                    From_Boolean
                      (Expect_Literal
                         (Params_Object, Param_Name (Instance, I)));
            end;
            Params_Object.Unset_Field (Param_Name (Instance, I));
         end if;
      end loop;
   end Process_Instance_Params_Object;

   overriding procedure Process_Instance_Params_Object
     (Instance      : in out Identifier_Suffixes_Instance;
      Params_Object : in out JSON_Value) is
   begin
      --  Process the "default" boolean parameter
      if Params_Object.Has_Field ("default") then
         if Expect_Literal (Params_Object, "default") then
            Set_Unbounded_Wide_Wide_String (Instance.Type_Suffix, "_T");
            Set_Unbounded_Wide_Wide_String (Instance.Access_Suffix, "_A");
            Set_Unbounded_Wide_Wide_String (Instance.Access_Access_Suffix, "");
            Set_Unbounded_Wide_Wide_String (Instance.Class_Access_Suffix, "");
            Set_Unbounded_Wide_Wide_String (Instance.Class_Subtype_Suffix, "");
            Set_Unbounded_Wide_Wide_String (Instance.Constant_Suffix, "_C");
            Set_Unbounded_Wide_Wide_String (Instance.Renaming_Suffix, "_R");
            Set_Unbounded_Wide_Wide_String (Instance.Access_Obj_Suffix, "");
            Set_Unbounded_Wide_Wide_String (Instance.Interrupt_Suffix, "");
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
                 (Instance.Access_Suffix,
                  To_Wide_Wide_String (Arg (Arg'First .. Paren_Index - 1)));
               Set_Unbounded_Wide_Wide_String
                 (Instance.Access_Access_Suffix,
                  To_Wide_Wide_String (Arg (Paren_Index + 1 .. Arg'Last - 1)));
            else
               Set_Unbounded_Wide_Wide_String
                 (Instance.Access_Suffix, To_Wide_Wide_String (Arg));
            end if;
         end;
         Params_Object.Unset_Field ("access_suffix");
      end if;

      --  Then process the other arguments
      Process_String_Arg (Params_Object, "type_suffix", Instance.Type_Suffix);
      Process_String_Arg
        (Params_Object, "class_access_suffix", Instance.Class_Access_Suffix);
      Process_String_Arg
        (Params_Object, "class_subtype_suffix", Instance.Class_Subtype_Suffix);
      Process_String_Arg
        (Params_Object, "constant_suffix", Instance.Constant_Suffix);
      Process_String_Arg
        (Params_Object, "renaming_suffix", Instance.Renaming_Suffix);
      Process_String_Arg
        (Params_Object, "access_obj_suffix", Instance.Access_Obj_Suffix);
      Process_String_Arg
        (Params_Object, "interrupt_suffix", Instance.Interrupt_Suffix);
   end Process_Instance_Params_Object;

   overriding procedure Process_Instance_Params_Object
     (Instance      : in out Identifier_Prefixes_Instance;
      Params_Object : in out JSON_Value)
   is
      Derived_Param : String_Vector;
      Col_Index : Natural;
   begin
      --  Process the exclusive boolean argument
      if Params_Object.Has_Field ("exclusive") then
         Instance.Exclusive :=
           From_Boolean (Expect_Literal (Params_Object, "exclusive"));
         Params_Object.Unset_Field ("exclusive");
      end if;

      --  Process the "derived" argument
      if Params_Object.Has_Field ("derived") then
         Derived_Param := Expect_Literal (Params_Object, "derived");
         for S of Derived_Param loop
            if Length (Instance.Derived_Prefix) /= 0 then
               Append (Instance.Derived_Prefix, ",");
            end if;
            Col_Index := Index (S, ":");
            if Col_Index /= 0 then
               Append
                 (Instance.Derived_Prefix,
                  To_Wide_Wide_String
                    (To_Lower (S (S'First .. Col_Index - 1))));
               Append
                 (Instance.Derived_Prefix,
                  To_Wide_Wide_String (S (Col_Index .. S'Last)));
            else
               raise Invalid_Value with
                 "'derived' elements should contain a colon";
            end if;
         end loop;
         Params_Object.Unset_Field ("derived");
      end if;

      --  The process the other arguments
      Process_String_Arg (Params_Object, "type", Instance.Type_Prefix);
      Process_String_Arg
        (Params_Object, "concurrent", Instance.Concurrent_Prefix);
      Process_String_Arg (Params_Object, "access", Instance.Access_Prefix);
      Process_String_Arg
        (Params_Object, "class_access", Instance.Class_Access_Prefix);
      Process_String_Arg
        (Params_Object,
         "subprogram_access",
         Instance.Subprogram_Access_Prefix);
      Process_String_Arg (Params_Object, "constant", Instance.Constant_Prefix);
      Process_String_Arg
        (Params_Object, "exception", Instance.Exception_Prefix);
      Process_String_Arg (Params_Object, "enum", Instance.Enum_Prefix);
   end Process_Instance_Params_Object;

   overriding procedure Process_Instance_Params_Object
     (Instance      : in out Identifier_Casing_Instance;
      Params_Object : in out JSON_Value) is
   begin
      --  Process the "exclude" argument
      if Params_Object.Has_Field ("exclude") then
         declare
            Exclude_File : constant String :=
              Expect_Literal (Params_Object, "exclude");
            File_Content : constant Unbounded_String :=
              Load_Dictionary_File (Expand_Env_Variables (Exclude_File));
         begin
            Params_Object.Unset_Field ("exclude");
            if File_Content /= Null_Unbounded_String then
               Set_Unbounded_Wide_Wide_String
                 (Instance.Exclude_File,
                  To_Wide_Wide_String (Exclude_File));
               Set_Unbounded_Wide_Wide_String
                 (Instance.Exclude,
                  To_Wide_Wide_String (To_String (File_Content)));
            else
               raise Invalid_Value with "cannot load file " & Exclude_File;
            end if;
         end;
      end if;

      --  Then process the other arguments
      Process_String_Arg
        (Params_Object, "type", Instance.Type_Casing, Normalize => True);
      Process_String_Arg
        (Params_Object, "enum", Instance.Enum_Casing, Normalize => True);
      Process_String_Arg
        (Params_Object,
         "constant",
         Instance.Constant_Casing,
         Normalize => True);
      Process_String_Arg
        (Params_Object,
         "exception",
         Instance.Exception_Casing,
         Normalize => True);
      Process_String_Arg
        (Params_Object, "others", Instance.Others_Casing, Normalize => True);
   end Process_Instance_Params_Object;

   overriding procedure Process_Instance_Params_Object
     (Instance      : in out Forbidden_Instance;
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
                     if Rule_Name (Instance) = "forbidden_attributes" then
                        Append (Field, GNAT_Attributes);
                     elsif Rule_Name (Instance) = "forbidden_pragmas" then
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
         Instance.All_Flag :=
           From_Boolean (Expect_Literal (Params_Object, "all"));
         Params_Object.Unset_Field ("all");
      end if;

      --  Process the "forbidden" and "allowed" list argument
      Process_List_Field ("forbidden", Instance.Forbidden);
      Process_List_Field ("allowed", Instance.Allowed);
   end Process_Instance_Params_Object;

   overriding procedure Process_Instance_Params_Object
     (Instance      : in out Silent_Exception_Handlers_Instance;
      Params_Object : in out JSON_Value)
   is
      Subp_Value : String_Vector;
   begin
      --  Process the "subprograms" list argument
      if Params_Object.Has_Field ("subprograms") then
         Subp_Value := Expect_Literal (Params_Object, "subprograms");
         for S of Subp_Value loop
            if S (S'First) = '|' then
               if Length (Instance.Subprogram_Regexps) /= 0 then
                  Append (Instance.Subprogram_Regexps, ",");
               end if;
               Append
                 (Instance.Subprogram_Regexps,
                  To_Wide_Wide_String (S (S'First + 1 .. S'Last)));
            else
               if Length (Instance.Subprograms) /= 0 then
                  Append (Instance.Subprograms, ",");
               end if;
               Append
                 (Instance.Subprograms, To_Wide_Wide_String (To_Lower (S)));
            end if;
         end loop;
         Params_Object.Unset_Field ("subprograms");
      end if;
   end Process_Instance_Params_Object;

   overriding procedure Process_Instance_Params_Object
     (Instance      : in out Custom_Instance;
      Params_Object : in out JSON_Value) is
   begin
      for I in 2 .. All_Rules (Instance.Rule).Parameters.Last_Child_Index loop
         declare
            P_Name : constant String := Param_Name (Instance, I);
         begin
            if Params_Object.Has_Field (P_Name) then
               Instance.Arguments.Append
                 (Rule_Argument'
                   (Name => To_Unbounded_Text (To_Text (P_Name)),
                    Value => To_Unbounded_Text
                               (To_Text
                                  (Expect (Params_Object, P_Name)))));
               Params_Object.Unset_Field (P_Name);
            end if;
         end;
      end loop;
   end Process_Instance_Params_Object;

   --------------------
   -- Map_Parameters --
   --------------------

   overriding procedure Map_Parameters
     (Instance : in out One_Integer_Parameter_Instance;
      Args     : in out Rule_Argument_Vectors.Vector) is
   begin
      Append_Int_Param
        (Args, Param_Name (All_Rules (Instance.Rule), 2), Instance.Param);
   end Map_Parameters;

   overriding procedure Map_Parameters
     (Instance : in out One_Boolean_Parameter_Instance;
      Args     : in out Rule_Argument_Vectors.Vector) is
   begin
      Append_Bool_Param
        (Args, Param_Name (All_Rules (Instance.Rule), 2), Instance.Param);
   end Map_Parameters;

   overriding procedure Map_Parameters
     (Instance : in out One_String_Parameter_Instance;
      Args     : in out Rule_Argument_Vectors.Vector) is
   begin
      Append_String_Param
        (Args, Param_Name (All_Rules (Instance.Rule), 2), Instance.Param);
   end Map_Parameters;

   overriding procedure Map_Parameters
     (Instance : in out One_Array_Parameter_Instance;
      Args     : in out Rule_Argument_Vectors.Vector) is
   begin
      Handle_Array_Param (Args, Instance);
   end Map_Parameters;

   overriding procedure Map_Parameters
     (Instance : in out One_Integer_Or_Booleans_Parameter_Instance;
      Args     : in out Rule_Argument_Vectors.Vector) is
   begin
      Append_Int_Param
        (Args, Param_Name
          (All_Rules (Instance.Rule), 2), Instance.Integer_Param);
      for J in 2 .. All_Rules (Instance.Rule).Parameters.Last_Child_Index loop
         Append_Bool_Param
           (Args, Param_Name
             (All_Rules (Instance.Rule), J), Instance.Boolean_Params (J));
      end loop;
   end Map_Parameters;

   overriding procedure Map_Parameters
     (Instance : in out Identifier_Suffixes_Instance;
      Args     : in out Rule_Argument_Vectors.Vector) is
   begin
      Append_String_Param (Args, "type_suffix", Instance.Type_Suffix);
      Append_String_Param (Args, "access_suffix", Instance.Access_Suffix);
      Append_String_Param
        (Args, "access_access_suffix", Instance.Access_Access_Suffix);
      Append_String_Param
        (Args, "class_access_suffix", Instance.Class_Access_Suffix);
      Append_String_Param
        (Args, "class_subtype_suffix", Instance.Class_Subtype_Suffix);
      Append_String_Param
        (Args, "constant_suffix", Instance.Constant_Suffix);
      Append_String_Param
        (Args, "renaming_suffix", Instance.Renaming_Suffix);
      Append_String_Param
        (Args, "access_obj_suffix", Instance.Access_Obj_Suffix);
      Append_String_Param
        (Args, "interrupt_suffix", Instance.Interrupt_Suffix);
   end Map_Parameters;

   overriding procedure Map_Parameters
     (Instance : in out Identifier_Prefixes_Instance;
      Args     : in out Rule_Argument_Vectors.Vector) is
   begin
      Append_String_Param (Args, "type", Instance.Type_Prefix);
      Append_String_Param (Args, "concurrent", Instance.Concurrent_Prefix);
      Append_String_Param (Args, "access", Instance.Access_Prefix);
      Append_String_Param
        (Args, "class_access", Instance.Class_Access_Prefix);
      Append_String_Param
        (Args, "subprogram_access", Instance.Subprogram_Access_Prefix);
      Append_String_Param (Args, "constant", Instance.Constant_Prefix);
      Append_String_Param (Args, "exception", Instance.Exception_Prefix);
      Append_String_Param (Args, "enum", Instance.Enum_Prefix);
      Append_Array_Param (Args, "derived", Instance.Derived_Prefix);

      if Instance.Exclusive = Off then
         Args.Append
           (Rule_Argument'
             (Name  => To_Unbounded_Text ("exclusive"),
              Value => To_Unbounded_Text ("false")));
      end if;
   end Map_Parameters;

   overriding procedure Map_Parameters
     (Instance : in out Identifier_Casing_Instance;
      Args     : in out Rule_Argument_Vectors.Vector) is
   begin
      Append_String_Param (Args, "type", Instance.Type_Casing);
      Append_String_Param (Args, "enum", Instance.Enum_Casing);
      Append_String_Param (Args, "constant", Instance.Constant_Casing);
      Append_String_Param (Args, "exception", Instance.Exception_Casing);
      Append_String_Param (Args, "others", Instance.Others_Casing);
      Append_Array_Param (Args, "exclude", Instance.Exclude);
   end Map_Parameters;

   overriding procedure Map_Parameters
     (Instance : in out Forbidden_Instance;
      Args     : in out Rule_Argument_Vectors.Vector) is
   begin
      if Instance.All_Flag = On then
         Args.Append
           (Rule_Argument'
             (Name  => To_Unbounded_Text ("all"),
              Value => To_Unbounded_Text ("true")));
      end if;

      Append_Array_Param (Args, "forbidden", Instance.Forbidden);
      Append_Array_Param (Args, "allowed", Instance.Allowed);
   end Map_Parameters;

   overriding procedure Map_Parameters
     (Instance : in out Silent_Exception_Handlers_Instance;
      Args     : in out Rule_Argument_Vectors.Vector) is
   begin
      Append_Array_Param (Args, "subprograms", Instance.Subprograms);
      Append_Array_Param
        (Args, "subprogram_regexps", Instance.Subprogram_Regexps);
   end Map_Parameters;

   overriding procedure Map_Parameters
     (Instance : in out Custom_Instance;
      Args     : in out Rule_Argument_Vectors.Vector) is
   begin
      for Arg of Instance.Arguments loop
         Args.Append (Arg);
      end loop;
   end Map_Parameters;

   ---------------------------------
   -- Print_Rule_Instance_To_File --
   ---------------------------------

   procedure Print_Rule_Instance_To_File
     (Instance     : Rule_Instance;
      Rule_File    : File_Type;
      Indent_Level : Natural := 0) is
   begin
      for J in 1 .. Indent_Level loop
         Put (Rule_File, Get_Indent_String);
      end loop;

      Put (Rule_File, "+R" & Rule_Name (Instance));
   end Print_Rule_Instance_To_File;

   overriding procedure Print_Rule_Instance_To_File
     (Instance     : One_Integer_Parameter_Instance;
      Rule_File    : File_Type;
      Indent_Level : Natural := 0) is
   begin
      Print_Rule_Instance_To_File
        (Rule_Instance (Instance), Rule_File, Indent_Level);

      if Instance.Param /= Integer'First then
         Put (Rule_File, ":" & Image (Instance.Param));
      end if;
   end Print_Rule_Instance_To_File;

   overriding procedure Print_Rule_Instance_To_File
     (Instance     : One_Boolean_Parameter_Instance;
      Rule_File    : File_Type;
      Indent_Level : Natural := 0) is
   begin
      Print_Rule_Instance_To_File
        (Rule_Instance (Instance), Rule_File, Indent_Level);

      if Instance.Param = On then
         Put (Rule_File, ":" & Param_Name (All_Rules (Instance.Rule), 2));
      end if;
   end Print_Rule_Instance_To_File;

   overriding procedure Print_Rule_Instance_To_File
     (Instance     : One_String_Parameter_Instance;
      Rule_File    : File_Type;
      Indent_Level : Natural := 0) is
   begin
      Print_Rule_Instance_To_File
        (Rule_Instance (Instance), Rule_File, Indent_Level);

      if Length (Instance.File) /= 0 then
         Put (Rule_File, ":" & To_String (Instance.File));

      elsif Length (Instance.Param) /= 0 then
         Put (Rule_File, ":" & To_String (Instance.Param));
      end if;
   end Print_Rule_Instance_To_File;

   overriding procedure Print_Rule_Instance_To_File
     (Instance     : One_Integer_Or_Booleans_Parameter_Instance;
      Rule_File    : File_Type;
      Indent_Level : Natural := 0)
   is
      Has_Param : Boolean := False;
   begin
      Print_Rule_Instance_To_File
        (Rule_Instance (Instance), Rule_File, Indent_Level);

      if Instance.Integer_Param /= Integer'First then
         Put (Rule_File, ":" & Image (Instance.Integer_Param));
         Has_Param := True;
      end if;

      for J in Instance.Boolean_Params'Range loop
         if Instance.Boolean_Params (J) = On then
            Put (Rule_File, (if Has_Param then "," else ":"));
            Put (Rule_File, Param_Name (All_Rules (Instance.Rule), J));
            Has_Param := True;
         end if;
      end loop;
   end Print_Rule_Instance_To_File;

   overriding procedure Print_Rule_Instance_To_File
     (Instance     : Identifier_Prefixes_Instance;
      Rule_File    : File_Type;
      Indent_Level : Natural := 0)
   is
      First_Param       : Boolean         := True;
      Rule_Name_Padding : constant String :=
        [1 .. Instance_Name (Instance)'Length + 3 => ' '];

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
      Print_Rule_Instance_To_File
        (Rule_Instance (Instance), Rule_File, Indent_Level);

      Print ("Type", Instance.Type_Prefix);
      Print ("Concurrent", Instance.Concurrent_Prefix);
      Print ("Access", Instance.Access_Prefix);
      Print ("Class_Access", Instance.Class_Access_Prefix);
      Print ("Subprogram_Access", Instance.Subprogram_Access_Prefix);
      Print ("Constant", Instance.Constant_Prefix);
      Print ("Exception", Instance.Exception_Prefix);
      Print ("Enum", Instance.Enum_Prefix);

      if Length (Instance.Derived_Prefix) /= 0 then
         Print ("Derived", Null_Unbounded_Wide_Wide_String, Force => True);

         for J in 1 .. Length (Instance.Derived_Prefix) loop
            declare
               C : constant Character :=
                 To_Character (Element (Instance.Derived_Prefix, J));
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

      if not First_Param and then Instance.Exclusive /= Off then
         Put_Line (Rule_File, ",");
         Put (Rule_File, Rule_Name_Padding & "Exclusive");
      end if;
   end Print_Rule_Instance_To_File;

   overriding procedure Print_Rule_Instance_To_File
     (Instance     : Identifier_Suffixes_Instance;
      Rule_File    : File_Type;
      Indent_Level : Natural := 0)
   is
      First_Param       : Boolean         := True;
      Rule_Name_Padding : constant String :=
        [1 .. Instance_Name (Instance)'Length + 3 => ' '];

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
      Print_Rule_Instance_To_File
        (Rule_Instance (Instance), Rule_File, Indent_Level);

      Print ("Type_Suffix", Instance.Type_Suffix);
      Print ("Access_Suffix", Instance.Access_Suffix);

      if Length (Instance.Access_Access_Suffix) /= 0 then
         Put
           (Rule_File, "(" & To_String (Instance.Access_Access_Suffix) & ")");
      end if;

      Print ("Class_Subtype_Suffix", Instance.Class_Subtype_Suffix);
      Print ("Class_Access_Suffix", Instance.Class_Access_Suffix);
      Print ("Constant_Suffix", Instance.Constant_Suffix);
      Print ("Renaming_Suffix", Instance.Renaming_Suffix);
      Print ("Access_Obj_Suffix", Instance.Access_Obj_Suffix);
      Print ("Interrupt_Suffix", Instance.Interrupt_Suffix);
   end Print_Rule_Instance_To_File;

   overriding procedure Print_Rule_Instance_To_File
     (Instance     : Identifier_Casing_Instance;
      Rule_File    : File_Type;
      Indent_Level : Natural := 0)
   is
      First_Param       : Boolean         := True;
      Rule_Name_Padding : constant String :=
        [1 .. Instance_Name (Instance)'Length + 3 => ' '];

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
      Print_Rule_Instance_To_File
        (Rule_Instance (Instance), Rule_File, Indent_Level);

      Print ("Type", Instance.Type_Casing);
      Print ("Enum", Instance.Enum_Casing);
      Print ("Constant", Instance.Constant_Casing);
      Print ("Exception", Instance.Exception_Casing);
      Print ("Others", Instance.Others_Casing);

      if Length (Instance.Exclude) /= 0 then
         Print ("Exclude", Null_Unbounded_Wide_Wide_String, Force => True);

         for J in 1 .. Length (Instance.Exclude) loop
            C := To_Character (Element (Instance.Exclude, J));

            if C /= ',' then
               Put (Rule_File, C);
            else
               Print ("Exclude",
                      Null_Unbounded_Wide_Wide_String,
                      Force => True);
            end if;
         end loop;
      end if;
   end Print_Rule_Instance_To_File;

   overriding procedure Print_Rule_Instance_To_File
     (Instance     : Forbidden_Instance;
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
      Print_Rule_Instance_To_File
        (Rule_Instance (Instance), Rule_File, Indent_Level);

      if Instance.All_Flag = On then
         Put (Rule_File, ":ALL");
         First_Param := False;
      else
         Print (Instance.Forbidden);
      end if;

      if Length (Instance.Allowed) /= 0 then
         New_Line (Rule_File);
         First_Param := True;

         for J in 1 .. Indent_Level loop
            Put (Rule_File, Get_Indent_String);
         end loop;

         Put (Rule_File, "-R" & Rule_Name (Instance));

         Print (Instance.Allowed);
      end if;
   end Print_Rule_Instance_To_File;

   overriding procedure Print_Rule_Instance_To_File
     (Instance     : Silent_Exception_Handlers_Instance;
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
      Print_Rule_Instance_To_File
        (Rule_Instance (Instance), Rule_File, Indent_Level);

      Print (Instance.Subprograms);
      Print (Instance.Subprogram_Regexps, Quote => True);
   end Print_Rule_Instance_To_File;

   overriding procedure Print_Rule_Instance_To_File
     (Instance     : Custom_Instance;
      Rule_File    : File_Type;
      Indent_Level : Natural := 0)
   is
      First_Param : Boolean := True;
   begin
      Print_Rule_Instance_To_File
        (Rule_Instance (Instance), Rule_File, Indent_Level);

      for Param of Instance.Arguments loop
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
   end Print_Rule_Instance_To_File;

   -----------------------------
   -- XML_Print_Rule_Instance --
   -----------------------------

   procedure XML_Print_Rule_Instance
     (Instance     : Rule_Instance;
      Indent_Level : Natural := 0) is
   begin
      XML_Report (XML_Head (Instance) & XML_Foot, Indent_Level);
   end XML_Print_Rule_Instance;

   overriding procedure XML_Print_Rule_Instance
     (Instance     : One_Integer_Parameter_Instance;
      Indent_Level : Natural := 0) is
   begin
      XML_Report (XML_Head (Instance), Indent_Level);

      if Instance.Param /= Integer'First then
         XML_Report (XML_Param (Image (Instance.Param)), Indent_Level + 1);
      end if;

      XML_Report (XML_Foot, Indent_Level);
   end XML_Print_Rule_Instance;

   overriding procedure XML_Print_Rule_Instance
     (Instance     : One_Boolean_Parameter_Instance;
      Indent_Level : Natural := 0) is
   begin
      XML_Report (XML_Head (Instance), Indent_Level);

      if Instance.Param = On then
         XML_Report
           (XML_Param (Param_Name (All_Rules (Instance.Rule), 2)),
            Indent_Level + 1);
      end if;

      XML_Report (XML_Foot, Indent_Level);
   end XML_Print_Rule_Instance;

   overriding procedure XML_Print_Rule_Instance
     (Instance     : One_String_Parameter_Instance;
      Indent_Level : Natural := 0) is
   begin
      XML_Report (XML_Head (Instance), Indent_Level);

      if Length (Instance.Param) /= 0 then
         XML_Report
           (XML_Param (To_String (Instance.Param)), Indent_Level + 1);
      end if;

      XML_Report (XML_Foot, Indent_Level);
   end XML_Print_Rule_Instance;

   overriding procedure XML_Print_Rule_Instance
     (Instance     : One_Integer_Or_Booleans_Parameter_Instance;
      Indent_Level : Natural := 0) is
   begin
      XML_Report (XML_Head (Instance), Indent_Level);

      if Instance.Integer_Param /= Integer'First then
         XML_Report
           (XML_Param (Image (Instance.Integer_Param)), Indent_Level + 1);
      end if;

      for J in Instance.Boolean_Params'Range loop
         if Instance.Boolean_Params (J) = On then
            XML_Report
              (XML_Param (Param_Name (All_Rules (Instance.Rule), J)),
               Indent_Level + 1);
         end if;
      end loop;

      XML_Report (XML_Foot, Indent_Level);
   end XML_Print_Rule_Instance;

   overriding procedure XML_Print_Rule_Instance
     (Instance     : Identifier_Prefixes_Instance;
      Indent_Level : Natural := 0)
   is
      Prefix_Specified : Boolean := False;

      procedure Print (Param : String; Prefix : Unbounded_Wide_Wide_String);
      --  Print in XML format the value `Prefix` for parameter `Param` if
      --  not empty.

      -----------
      -- Print --
      -----------

      procedure Print (Param : String; Prefix : Unbounded_Wide_Wide_String) is
      begin
         if Length (Prefix) /= 0 then
            XML_Report
              (XML_Param (Param & "=" & To_String (Prefix)),
               Indent_Level + 1);
            Prefix_Specified := True;
         end if;
      end Print;

   begin
      XML_Report (XML_Head (Instance), Indent_Level);

      Print ("Type", Instance.Type_Prefix);
      Print ("Concurrent", Instance.Concurrent_Prefix);
      Print ("Access", Instance.Access_Prefix);
      Print ("Class_Access", Instance.Class_Access_Prefix);
      Print ("Subprogram_Access", Instance.Subprogram_Access_Prefix);
      Print ("Constant", Instance.Constant_Prefix);
      Print ("Exception", Instance.Exception_Prefix);
      Print ("Enum", Instance.Enum_Prefix);
      Print ("Derived", Instance.Derived_Prefix);

      --  We have to print out Exclusive parameter, but this would make sense
      --  only if at least one prefix is specified.
      if Prefix_Specified and then Instance.Exclusive /= Off then
         XML_Report (XML_Param ("Exclusive"), Indent_Level + 1);
      end if;

      XML_Report (XML_Foot, Indent_Level);
   end XML_Print_Rule_Instance;

   overriding procedure XML_Print_Rule_Instance
     (Instance     : Identifier_Suffixes_Instance;
      Indent_Level : Natural := 0)
   is
      procedure Print (Param : String; Suffix : Unbounded_Wide_Wide_String);
      --  Print in XML format the value `Suffix` for parameter `Param` if
      --  not empty

      -----------
      -- Print --
      -----------

      procedure Print (Param : String; Suffix : Unbounded_Wide_Wide_String) is
      begin
         if Length (Suffix) /= 0 then
            XML_Report
              (XML_Param (Param & "=" & To_String (Suffix)),
               Indent_Level + 1);
         end if;
      end Print;

   begin
      XML_Report (XML_Head (Instance), Indent_Level);

      Print ("Type_Suffix", Instance.Type_Suffix);

      if Length (Instance.Access_Suffix) /= 0 then
         XML_Report
           (XML_Param ("Access_Suffix=" & To_String (Instance.Access_Suffix) &
                       (if Length (Instance.Access_Access_Suffix) /= 0 then
                          "(" & To_String (Instance.Access_Access_Suffix) & ")"
                        else "")),
            Indent_Level + 1);
      end if;

      Print ("Class_Subtype_Suffix", Instance.Class_Subtype_Suffix);
      Print ("Class_Access_Suffix", Instance.Class_Access_Suffix);
      Print ("Constant_Suffix", Instance.Constant_Suffix);
      Print ("Renaming_Suffix", Instance.Renaming_Suffix);
      Print ("Access_Obj_Suffix", Instance.Access_Obj_Suffix);
      Print ("Interrupt_Suffix", Instance.Interrupt_Suffix);

      XML_Report (XML_Foot, Indent_Level);
   end XML_Print_Rule_Instance;

   overriding procedure XML_Print_Rule_Instance
     (Instance     : Identifier_Casing_Instance;
      Indent_Level : Natural := 0)
   is
      procedure Print (Param : String; Casing : Unbounded_Wide_Wide_String);
      --  Print in XML format the value `Casing` for parameter `Param` if
      --  not empty

      -----------
      -- Print --
      -----------

      procedure Print (Param : String; Casing : Unbounded_Wide_Wide_String) is
      begin
         if Length (Casing) /= 0 then
            XML_Report
              (XML_Param (Param & "=" & To_String (Casing)),
               Indent_Level + 1);
         end if;
      end Print;

   begin
      XML_Report (XML_Head (Instance), Indent_Level);

      Print ("Type", Instance.Type_Casing);
      Print ("Enum", Instance.Enum_Casing);
      Print ("Constant", Instance.Constant_Casing);
      Print ("Exception", Instance.Exception_Casing);
      Print ("Others", Instance.Others_Casing);

      Print_XML_Params
        (To_String (Instance.Exclude),
         Indent_Level + 1,
         Prefix => "Exclude=");

      XML_Report (XML_Foot, Indent_Level);
   end XML_Print_Rule_Instance;

   overriding procedure XML_Print_Rule_Instance
     (Instance     : Forbidden_Instance;
      Indent_Level : Natural := 0) is
   begin
      XML_Report (XML_Head (Instance), Indent_Level);

      if Instance.All_Flag = On then
         XML_Report (XML_Param ("ALL"), Indent_Level + 1);
      else
         Print_XML_Params (To_String (Instance.Forbidden), Indent_Level + 1);
      end if;
      Print_XML_Params (To_String (Instance.Allowed), Indent_Level + 1, "-");

      XML_Report (XML_Foot, Indent_Level);
   end XML_Print_Rule_Instance;

   overriding procedure XML_Print_Rule_Instance
     (Instance     : Silent_Exception_Handlers_Instance;
      Indent_Level : Natural := 0) is
   begin
      XML_Report (XML_Head (Instance), Indent_Level);

      Print_XML_Params (To_String (Instance.Subprograms), Indent_Level + 1);
      Print_XML_Params
        (To_String (Instance.Subprogram_Regexps),
         Indent_Level + 1,
         """",
         """");

      XML_Report (XML_Foot, Indent_Level);
   end XML_Print_Rule_Instance;

   overriding procedure XML_Print_Rule_Instance
     (Instance     : Custom_Instance;
      Indent_Level : Natural := 0) is
   begin
      XML_Report (XML_Head (Instance), Indent_Level);

      for Arg of Instance.Arguments loop
         XML_Report
           (XML_Param
             (To_String (To_Wide_Wide_String (Arg.Name)) & "=" &
              To_String (To_Wide_Wide_String (Arg.Value))),
            Indent_Level + 1);
      end loop;

      XML_Report (XML_Foot, Indent_Level);
   end XML_Print_Rule_Instance;

   ---------------
   -- Load_File --
   ---------------

   function Load_File
     (Instance : in out One_String_Parameter_Instance'Class;
      To_Load  : String) return Boolean
   is
      Abs_Name : constant String := Find_File (To_Load);
      Str  : GNAT.OS_Lib.String_Access;
      Last : Natural;
   begin
      if Abs_Name /= "" then
         Str := Read_File (Abs_Name);
         Ada.Strings.Unbounded.Set_Unbounded_String (Instance.File, To_Load);

         Last := Str'Last;

         --  If `Last` is null or less, then the file is empty.
         --  Thus don't append anything to the rule parameter.
         if Last > 0 then
            --  Strip trailing end of line
            if Str (Str'Last) = ASCII.LF then
               Last := Last - 1;
            end if;

            Ada.Strings.Wide_Wide_Unbounded.Set_Unbounded_Wide_Wide_String
            (Instance.Param, To_Wide_Wide_String (Str (1 .. Last)));
            GNAT.OS_Lib.Free (Str);
         end if;
         return True;
      else
         return False;
      end if;
   end Load_File;

end Gnatcheck.Rules;

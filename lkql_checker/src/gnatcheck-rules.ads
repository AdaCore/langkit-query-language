--
--  Copyright (C) 2005-2024, AdaCore
--  SPDX-License-Identifier: GPL-3.0-or-later
--

--  This is the top of gnatcheck hierarchy that defines individual rules, rule
--  table and rule checking process. It contains some basic type declarations

with Ada.Characters.Handling;         use Ada.Characters.Handling;
with Ada.Containers.Indefinite_Ordered_Sets;
with Ada.Containers.Vectors;
with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;
with Ada.Strings.Unbounded;           use Ada.Strings.Unbounded;
with Ada.Text_IO;                     use Ada.Text_IO;
with Ada.Unchecked_Deallocation;

with Gnatcheck.Ids;              use Gnatcheck.Ids;
with Gnatcheck.String_Utilities; use Gnatcheck.String_Utilities;

with GNATCOLL.JSON; use GNATCOLL.JSON;

with Liblkqllang.Analysis;

with Rule_Commands; use Rule_Commands;

with System.Rident;

package Gnatcheck.Rules is

   pragma Warnings (Off);
   package Rident is new System.Rident;
   pragma Warnings (On);

   package Exemption_Parameters is new Ada.Containers.Indefinite_Ordered_Sets
     (Element_Type => String);
   --  Needed to keep/process strings that can be used as rule parameters for
   --  rule exemptions.

   subtype Rule_Params is Exemption_Parameters.Set;

   subtype Diagnosis_Variant is Natural;
   --  Used to numerate the variants of diagnostic message for the same rule.
   --  Zero means that the rule has exactly one variant of the diagnostic
   --  message

   Invalid_Value : exception;
   --  Exception to raise when an unexpected valud is provided to a rule
   --  processing function.

   type Rule_States is (Enabled, Disabled);
   --  Each rule and alias may have one of the following states:
   --   Enabled -  the rule/alias should be checked at the moment, this state
   --              may be changed during the gnatcheck run (as a result of
   --              passing a control comment in the analyzed source)
   --   Disabled - the rule/alias is disabled for the given gnatcheck run,
   --              this state cannot be changed during the gnatcheck run

   ----------------
   -- Rule types --
   ----------------

   type Source_Modes is (General, Ada_Only, Spark_Only);
   --  Each rule may have a source mode, this information sets on which types
   --  of source code the rule should be executed:
   --  Ada     - Only on pure Ada code (not run on SPARK snippets)
   --  Spark   - Only on SPARK code
   --  General - On both

   type Rule_Instance (Is_Alias : Boolean) is tagged record
      Rule : Rule_Id;
      --  Identifier of the rule which is instanciated

      Source_Mode : Source_Modes;
      --  Mode of the instance regarding the Ada sources

      Defined_At : Unbounded_String;
      --  Location of the instance definition in the rule file

      case Is_Alias is
         when True =>
            Alias_Name : Unbounded_String;
            --  Name of the alias as the user wrote it
         when False =>
            null;
      end case;
   end record;
   --  Represents an instance of a rule. An instance is on way to run a rule,
   --  this instance may be an alias, contains all needed arguments for the
   --  rule run and refers to the rule information.

   type Rule_Instance_Access is access all Rule_Instance'Class;

   package Rule_Instance_Vector is new Ada.Containers.Vectors
     (Index_Type => Natural, Element_Type => Rule_Instance_Access);

   type Rule_Info is record
      Name : Unbounded_String;
      --  The only means of rule identification outside gnatcheck. All the
      --  rules implemented in gnatcheck should have unique names, the casing
      --  is not important.

      Defined_At : Unbounded_String;
      --  Location in the rule file where the rule has been enabled. Set to
      --  Nil_String_Loc if the rule has been enabled by command line option.

      Remediation_Level : Remediation_Levels := Medium;
      --  The remediation level of the rule, the default value is Medium but
      --  it may be updated during the GNATcheck execution.

      Help_Info : Unbounded_String;
      --  Short help information for the rule

      Category : Unbounded_String;
      --  Category for this rule

      Subcategory : Unbounded_String;
      --  Subcategory for this rule, "" if none

      Allows_Parametrized_Exemption : Boolean;
      --  Whether you can specify a rule parameter when defining an exemption
      --  section for this Rule. In case if a rule parameter has the form like
      --  'Param_Name => Param_Value', in the exemption section you can specify
      --  only "Param_Name'

      Impact : Regexp_Access;
      --  For a KP detector, regexp to match relevant releases impacted, if
      --  any. Ignored if null.

      Target : Regexp_Access;
      --  For a KP detector, regexp to match relevant target triplets impacted,
      --  if any. Ignored if null.

      Parameters : Liblkqllang.Analysis.Parameter_Decl_List;
      --  List of formal parameters for this rule

      Instances : Rule_Instance_Vector.Vector;
      --  Enabled instances of the rule

      XML_Rule_Help : access procedure
        (Rule : Rule_Info; Indent_Level : Natural);
      --  Access to the function to print rule information (including those
      --  about rule parameters) in XML format, The template prints out the
      --  following string:
      --
      --  <check switch="+RRule_Name" label="brief rule help"/>

      Allowed_As_Exemption_Parameter : access function
        (Param : String) return Boolean;
      --  Access to the function required to get whether a given exemption
      --  parameter is allowed for the rule.

      Rule_Param_From_Diag : access function
        (Diag : String) return String;
      --  Access to a function which assumes that
      --  `Allows_Parametrized_Exemption (Rule)` is True and `Diag` is a
      --  diagnosis emitted for `Rule`. This function returns the formal
      --  parameter name which `Diag` has been generated for.

      Create_Instance : access function
        (Is_Alias : Boolean) return Rule_Instance_Access;
      --  Access to the function required to create a new instance of the given
      --  rule. This function allocate a new instance record and return an
      --  access to it. The returned instance must be freed when not needed
      --  anymore.

      Process_Rule_Parameter : access procedure
        (Rule          : Rule_Id;
         Instance_Name : String;
         Param         : String;
         Enable        : Boolean;
         Defined_At    : String);
      --  Access to the function required to process a given parameter. This
      --  function updates the global instances map according to the
      --  provided `Param` and `Enable` values. This function also check for
      --  parameter redefinition if the flobal flag `Check_Redefinition`
      --  is on.
      --
      --  The `Instance_Name` is the name of the instance to create or update.
      --  If the rule parameter string consists of more than one parameter
      --  separated by ',', then this procedure is called for each `Param`
      --  substring.
      --
      --  `Defined_At` represents the location of the instance last
      --  definintion.
   end record;
   --  This record store all needed read-only information about a rule.

   ------------------------------------
   -- Operations on rule information --
   ------------------------------------

   function Create_Rule
     (Param_Kind : Rule_Param_Kind;
      Rule_Name  : String) return Rule_Info;
   --  Allocate a new rule information record with the given param kind. The
   --  returned pointer should be freed by the caller.

   function Rule_Name (Rule : Rule_Info) return String;
   --  Returns the rule name

   function Is_Enabled (Rule : Rule_Info) return Boolean;
   --  Checks if the rule has any instance so is enabled

   procedure Print_Rule_Help (Rule : Rule_Info);
   --  Prints into Stderr the rule help information. If the help info is long,
   --  no wrapping is performed.

   procedure Print_Rule_Instances_To_LKQL_File
     (Rule       : Rule_Info;
      Rule_File  : File_Type;
      Mode       : Source_Modes;
      For_Worker : Boolean;
      First_Rule : in out Boolean);
   --  Prints information about all instances of the rule into the specified
   --  file with the LKQL rule options file syntax. This function will only
   --  prints instance with the provided `Mode`.
   --  The ``For_Worker`` boolean tells whether this procedure should emit an
   --  LKQL rule configuration file for the worker or for it to be used by
   --  another GNATcheck run.

   procedure Print_Compiler_Rule_To_LKQL_File
     (Compiler_Rule : Rule_Id;
      Rule_File     : File_Type;
      First_Rule    : in out Boolean);
   --  Prints configuration of the compiler rule identified by
   --  ``Compiler_Rule`` in the LKQL rule fiel format.

   -----------------------------------------------------------------
   -- Operations that may be redefined by specific rule instances --
   -----------------------------------------------------------------

   procedure Process_Instance_Params_Object
     (Instance      : in out Rule_Instance;
      Params_Object : in out JSON_Value)
   is null;
   --  Process the given JSON value as an arguments object for `Rule`. This
   --  object contains argument names as keys, associated with their value
   --  expressed by an LKQL literal value.
   --  This function will remove a key from `Args_Object` if it is used as an
   --  argument for the rule.
   --
   --  This function may throw `Gnatcheck.JSON_Utilities.Field_Not_Found` if an
   --  argument is missing, or `Gnatcheck.JSON_Utilities.Invalid_Type` if an
   --  argument is not of the valid type.

   procedure Map_Parameters
     (Instance : in out Rule_Instance;
      Args : in out Rule_Argument_Vectors.Vector)
   is null;
   --  After parameters have been processed, store the relevant parameters for
   --  Rule in Map.

   procedure Print_Rule_Instance_To_File
     (Instance     : Rule_Instance;
      Rule_File    : File_Type;
      Indent_Level : Natural := 0);
   --  Prints information about the rule instance into the specified file. For
   --  the rules that have parameters this procedure should be rewritten to
   --  print out actual parameter settings.

   procedure XML_Print_Rule_Instance
     (Instance     : Rule_Instance;
      Indent_Level : Natural := 0);
   --  Print the rule instance information follwing the XML file format

   ------------------------------------------
   -- Generic operations on rule instances --
   ------------------------------------------

   function Rule_Name (Instance : Rule_Instance'Class) return String;
   --  Returns the name of the rule which is instantiated

   function Instance_Name (Instance : Rule_Instance'Class) return String;
   --  Returns the name of the instance; user defined alias if the instance
   --  is an alias, rule name else.

   function Annotate_Diag (Instance : Rule_Instance'Class) return String;
   --  Return a string to annotate a diagnostic about the rule instance

   procedure Free is new Ada.Unchecked_Deallocation
     (Rule_Instance'Class, Rule_Instance_Access);
   --  Free the memory allocated for a rule instance

   ---------------------------------------
   -- "One integer parameter" instances --
   ---------------------------------------

   type One_Integer_Parameter_Instance is new Rule_Instance with record
      Param : Integer := Integer'First;
   end record;
   --  Represents an instance of a rule with only one integer parameter

   overriding procedure Process_Instance_Params_Object
     (Instance      : in out One_Integer_Parameter_Instance;
      Params_Object : in out JSON_Value);

   overriding procedure Map_Parameters
     (Instance : in out One_Integer_Parameter_Instance;
      Args     : in out Rule_Argument_Vectors.Vector);

   overriding procedure Print_Rule_Instance_To_File
     (Instance     : One_Integer_Parameter_Instance;
      Rule_File    : File_Type;
      Indent_Level : Natural := 0);

   overriding procedure XML_Print_Rule_Instance
     (Instance     : One_Integer_Parameter_Instance;
      Indent_Level : Natural := 0);

   ---------------------------------------
   -- "One boolean parameter" instances --
   ---------------------------------------

   type Tri_State is (Unset, On, Off);

   type One_Boolean_Parameter_Instance is new Rule_Instance with record
      Param : Tri_State := Unset;
   end record;
   --  Represents an instance of a rule with only one boolean parameter

   overriding procedure Process_Instance_Params_Object
     (Instance      : in out One_Boolean_Parameter_Instance;
      Params_Object : in out JSON_Value);

   overriding procedure Map_Parameters
     (Instance : in out One_Boolean_Parameter_Instance;
      Args     : in out Rule_Argument_Vectors.Vector);

   overriding procedure Print_Rule_Instance_To_File
     (Instance     : One_Boolean_Parameter_Instance;
      Rule_File    : File_Type;
      Indent_Level : Natural := 0);

   overriding procedure XML_Print_Rule_Instance
     (Instance     : One_Boolean_Parameter_Instance;
      Indent_Level : Natural := 0);

   --------------------------------------
   -- "One string parameter" instances --
   --------------------------------------

   type One_String_Parameter_Instance is new Rule_Instance with record
      Param : Unbounded_Wide_Wide_String := Null_Unbounded_Wide_Wide_String;
      File  : Unbounded_String := Null_Unbounded_String;
   end record;
   --  Represents an instance of a rule with only one string parameter

   overriding procedure Process_Instance_Params_Object
     (Instance      : in out One_String_Parameter_Instance;
      Params_Object : in out JSON_Value);

   overriding procedure Map_Parameters
     (Instance : in out One_String_Parameter_Instance;
      Args     : in out Rule_Argument_Vectors.Vector);

   overriding procedure Print_Rule_Instance_To_File
     (Instance     : One_String_Parameter_Instance;
      Rule_File    : File_Type;
      Indent_Level : Natural := 0);

   overriding procedure XML_Print_Rule_Instance
     (Instance     : One_String_Parameter_Instance;
      Indent_Level : Natural := 0);

   function Load_File
     (Instance : in out One_String_Parameter_Instance'Class;
      To_Load  : String) return Boolean;
   --  Load the `File_Name` file content in the rule parameter, then returns
   --  if the file loading was successful.

   -------------------------------------
   -- "One array parameter" instances --
   -------------------------------------

   type One_Array_Parameter_Instance is new One_String_Parameter_Instance
   with null record;
   --  Represents an instance of a rule with only on array parameter

   overriding procedure Process_Instance_Params_Object
     (Instance      : in out One_Array_Parameter_Instance;
      Params_Object : in out JSON_Value);

   overriding procedure Map_Parameters
     (Instance : in out One_Array_Parameter_Instance;
      Args     : in out Rule_Argument_Vectors.Vector);

   ------------------------------------------------------------
   -- "One Integer and/or Many Booleans parameter" instances --
   ------------------------------------------------------------

   type Boolean_Parameters is array (2 .. 10) of Tri_State;

   type One_Integer_Or_Booleans_Parameter_Instance is new Rule_Instance
   with record
      Integer_Param  : Integer := Integer'First;
      Boolean_Params : Boolean_Parameters := [others => Unset];
   end record;
   --  Represents an instance of a rule with one integer and boolean parameters

   overriding procedure Process_Instance_Params_Object
     (Instance      : in out One_Integer_Or_Booleans_Parameter_Instance;
      Params_Object : in out JSON_Value);

   overriding procedure Map_Parameters
     (Instance : in out One_Integer_Or_Booleans_Parameter_Instance;
      Args     : in out Rule_Argument_Vectors.Vector);

   overriding procedure Print_Rule_Instance_To_File
     (Instance     : One_Integer_Or_Booleans_Parameter_Instance;
      Rule_File    : File_Type;
      Indent_Level : Natural := 0);

   overriding procedure XML_Print_Rule_Instance
     (Instance     : One_Integer_Or_Booleans_Parameter_Instance;
      Indent_Level : Natural := 0);

   -----------------------------------
   -- Identifier_Suffixes instances --
   -----------------------------------

   type Identifier_Suffixes_Instance is new Rule_Instance with record
      Type_Suffix,
      Access_Suffix,
      Access_Access_Suffix,
      Class_Access_Suffix,
      Class_Subtype_Suffix,
      Constant_Suffix,
      Renaming_Suffix,
      Access_Obj_Suffix,
      Interrupt_Suffix : Unbounded_Wide_Wide_String :=
        Null_Unbounded_Wide_Wide_String;
   end record;
   --  Represents an instance of a rule about identifier suffixes

   overriding procedure Process_Instance_Params_Object
     (Instance      : in out Identifier_Suffixes_Instance;
      Params_Object : in out JSON_Value);

   overriding procedure Map_Parameters
     (Instance : in out Identifier_Suffixes_Instance;
      Args     : in out Rule_Argument_Vectors.Vector);

   overriding procedure Print_Rule_Instance_To_File
     (Instance     : Identifier_Suffixes_Instance;
      Rule_File    : File_Type;
      Indent_Level : Natural := 0);

   overriding procedure XML_Print_Rule_Instance
     (Instance     : Identifier_Suffixes_Instance;
      Indent_Level : Natural := 0);

   -----------------------------------
   -- Identifier_Prefixes instances --
   -----------------------------------

   type Identifier_Prefixes_Instance is new Rule_Instance with record
      Type_Prefix,
      Concurrent_Prefix,
      Access_Prefix,
      Class_Access_Prefix,
      Subprogram_Access_Prefix,
      Derived_Prefix,
      Constant_Prefix,
      Exception_Prefix,
      Enum_Prefix : Unbounded_Wide_Wide_String :=
        Null_Unbounded_Wide_Wide_String;
      Exclusive   : Tri_State := Unset;
   end record;
   --  Represents an instance of a rule about identifier prefixes

   overriding procedure Process_Instance_Params_Object
     (Instance      : in out Identifier_Prefixes_Instance;
      Params_Object : in out JSON_Value);

   overriding procedure Map_Parameters
     (Instance : in out Identifier_Prefixes_Instance;
      Args     : in out Rule_Argument_Vectors.Vector);

   overriding procedure Print_Rule_Instance_To_File
     (Instance     : Identifier_Prefixes_Instance;
      Rule_File    : File_Type;
      Indent_Level : Natural := 0);

   overriding procedure XML_Print_Rule_Instance
     (Instance     : Identifier_Prefixes_Instance;
      Indent_Level : Natural := 0);

   ---------------------------------
   -- Identifier_Casing instances --
   ---------------------------------

   type Identifier_Casing_Instance is new Rule_Instance with record
      Type_Casing,
      Enum_Casing,
      Constant_Casing,
      Exception_Casing,
      Others_Casing,
      Exclude,
      Exclude_File : Unbounded_Wide_Wide_String :=
        Null_Unbounded_Wide_Wide_String;
   end record;
   --  Represents an instance of a rule about identifiers casing

   overriding procedure Process_Instance_Params_Object
     (Instance      : in out Identifier_Casing_Instance;
      Params_Object : in out JSON_Value);

   overriding procedure Map_Parameters
     (Instance : in out Identifier_Casing_Instance;
      Args     : in out Rule_Argument_Vectors.Vector);

   overriding procedure Print_Rule_Instance_To_File
     (Instance     : Identifier_Casing_Instance;
      Rule_File    : File_Type;
      Indent_Level : Natural := 0);

   overriding procedure XML_Print_Rule_Instance
     (Instance     : Identifier_Casing_Instance;
      Indent_Level : Natural := 0);

   ---------------------------
   -- Forbidden_* instances --
   ---------------------------

   type Forbidden_Instance is new Rule_Instance with record
      All_Flag  : Tri_State := Unset;
      Forbidden,
      Allowed   : Unbounded_Wide_Wide_String :=
        Null_Unbounded_Wide_Wide_String;
   end record;
   --  Represents an instance of a rule about forbiddening things

   overriding procedure Process_Instance_Params_Object
     (Instance      : in out Forbidden_Instance;
      Params_Object : in out JSON_Value);

   overriding procedure Map_Parameters
     (Instance : in out Forbidden_Instance;
      Args     : in out Rule_Argument_Vectors.Vector);

   overriding procedure Print_Rule_Instance_To_File
     (Instance     : Forbidden_Instance;
      Rule_File    : File_Type;
      Indent_Level : Natural := 0);

   overriding procedure XML_Print_Rule_Instance
     (Instance     : Forbidden_Instance;
      Indent_Level : Natural := 0);

   -----------------------------------------
   -- Silent_Exception_Handlers instances --
   -----------------------------------------

   type Silent_Exception_Handlers_Instance is new Rule_Instance with record
      Subprograms,
      Subprogram_Regexps : Unbounded_Wide_Wide_String :=
        Null_Unbounded_Wide_Wide_String;
   end record;
   --  Represents an instance of a rule about silent exception handlers

   overriding procedure Process_Instance_Params_Object
     (Instance      : in out Silent_Exception_Handlers_Instance;
      Params_Object : in out JSON_Value);

   overriding procedure Map_Parameters
     (Instance : in out Silent_Exception_Handlers_Instance;
      Args     : in out Rule_Argument_Vectors.Vector);

   overriding procedure Print_Rule_Instance_To_File
     (Instance     : Silent_Exception_Handlers_Instance;
      Rule_File    : File_Type;
      Indent_Level : Natural := 0);

   overriding procedure XML_Print_Rule_Instance
     (Instance     : Silent_Exception_Handlers_Instance;
      Indent_Level : Natural := 0);

   ----------------------
   -- Custom instances --
   ----------------------

   type Custom_Instance is new Rule_Instance with record
      Arguments : Rule_Argument_Vectors.Vector;
   end record;
   --  Represents an instance of a rule with arbitrary parameters

   overriding procedure Process_Instance_Params_Object
     (Instance      : in out Custom_Instance;
      Params_Object : in out JSON_Value);

   overriding procedure Map_Parameters
     (Instance : in out Custom_Instance;
      Args     : in out Rule_Argument_Vectors.Vector);

   overriding procedure Print_Rule_Instance_To_File
     (Instance     : Custom_Instance;
      Rule_File    : File_Type;
      Indent_Level : Natural := 0);

   overriding procedure XML_Print_Rule_Instance
     (Instance     : Custom_Instance;
      Indent_Level : Natural := 0);

   ------------------------------
   -- Compiler-based instances --
   ------------------------------

   type Compiler_Instance is new Rule_Instance with record
      Arguments : String_Vector;
   end record;

end Gnatcheck.Rules;

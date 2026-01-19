--
--  Copyright (C) 2005-2026, AdaCore
--  SPDX-License-Identifier: GPL-3.0-or-later
--

with Ada.Containers.Vectors;

with GNAT.Regexp;

with GNATCOLL.JSON; use GNATCOLL.JSON;

with Langkit_Support.Text; use Langkit_Support.Text;

with Liblkqllang.Analysis;

--  A diagnostic is composed of a collection of individual rule commands

package Rule_Commands is

   package L renames Liblkqllang.Analysis;

   Rule_Error : exception;

   type Rule_Argument is record
      Name : Unbounded_Text_Type;
      --  Name of the argument

      Value : Unbounded_Text_Type;
      --  Value of the argument, as a string.
   end record;

   package Rule_Argument_Vectors is new
     Ada.Containers.Vectors (Positive, Rule_Argument);

   type Rule_Param_Kind is
     (No_Param,
      One_Integer,
      One_Boolean,
      One_String,
      One_Array,
      One_Integer_Or_Booleans,
      Custom);
   --  Specifies how parameters are handled by the rule

   type Remediation_Levels is (Trivial, Easy, Medium, Major, High, Complex);
   --  Difficulty to address the rule violation.

   type Regexp_Access is access all GNAT.Regexp.Regexp;

   type Rule_Command is tagged record
      Name : Unbounded_Text_Type;
      --  Name of the Rule

      Help : Unbounded_Text_Type;
      --  Short help message associated with the rule.
      --  Defaults to Message if not specified.

      Category : Unbounded_Text_Type;
      --  Category of this rule, if relevant

      Subcategory : Unbounded_Text_Type;
      --  Subcategory of this rule, if relevant

      Param_Kind : Rule_Param_Kind;
      --  Category of parameters.

      Parameters : L.Parameter_Decl_List;
      --  List of formal parameters for this rule.

      Remediation_Level : Remediation_Levels;
      --  Remediation level to compute technical debt.

      Parametric_Exemption : Boolean;

      Impact : Regexp_Access;
      --  For a KP detector, regexp to match relevant releases impacted, if
      --  any. Ignored if null.

      Target : Regexp_Access;
      --  For a KP detector, regexp to match relevant target triplets impacted,
      --  if any. Ignored if null.
   end record;

   function Create_Rule_Command
     (Lkql_File_Path : String;
      Ctx            : L.Analysis_Context;
      Impacts        : JSON_Value;
      Rc             : out Rule_Command) return Boolean;
   --  Create a Rule_Command value with the given name and arguments and
   --  store it in ``Rc``. Return ``True`` if this succeeded, ie. the file
   --  corresponds to a rule file, ``False`` otherwise.

end Rule_Commands;

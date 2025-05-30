--
--  Copyright (C) 2005-2025, AdaCore
--  SPDX-License-Identifier: GPL-3.0-or-later
--

with Ada.Containers.Vectors;

with GNAT.Regexp;

with GNATCOLL.JSON; use GNATCOLL.JSON;

with Langkit_Support.Diagnostics;          use Langkit_Support.Diagnostics;
with Langkit_Support.Generic_API.Analysis;
use Langkit_Support.Generic_API.Analysis;
with Langkit_Support.Text;                 use Langkit_Support.Text;

with Libadalang.Analysis;

with Liblkqllang.Analysis;

--  A diagnostic is composed of a collection of individual rule commands

package Rule_Commands is

   package L renames Liblkqllang.Analysis;
   package LAL renames Libadalang.Analysis;

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

      Message : Unbounded_Text_Type;
      --  Diagnostic messages associated with the rule.
      --  Defaults to Name if not specified.

      Help : Unbounded_Text_Type;
      --  Short help message associated with the rule.
      --  Defaults to Message if not specified.

      Category : Unbounded_Text_Type;
      --  Category of this rule, if relevant

      Subcategory : Unbounded_Text_Type;
      --  Subcategory of this rule, if relevant

      Lkql_Root : L.Lkql_Node;
      --  Root of the LKQL AST

      Function_Expr : L.Expr;
      --  Body expr of the LKQL function. Will be used in case of node checks,
      --  to directly eval the expr without creating a function scope.

      Rule_Args : Rule_Argument_Vectors.Vector;
      --  Optional arguments to pass to the rule. Empty by default.

      Code : L.Lkql_Node;
      --  Store (cache) the code for the rule, so as to not recompute it
      --  everytime we want to evaluate it.

      Is_Unit_Check : Boolean;
      --  Whether the rule is expressed via a unit check (function that
      --  returns a list of messages) or a node check (function that returns a
      --  node).

      Kind_Pattern : L.Node_Kind_Pattern;
      --  If we determined that the rule only needs to run on a specific node
      --  kind, store the corresponding node pattern here.

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

   type Output_Style is (Default, GNATcheck, Silent);
   --  Style of output messages.
   --  Default: gcc style colored output with source highlighting.
   --  GNATcheck: gnatcheck default output with one line per detection.

   type Eval_Diagnostic is record
      Diag : Diagnostic;
      Unit : Lk_Unit;
   end record;

   package Eval_Diagnostic_Vectors is new
     Ada.Containers.Vectors (Positive, Eval_Diagnostic);

   function Create_Rule_Command
     (Lkql_File_Path : String;
      Ctx            : L.Analysis_Context;
      Impacts        : JSON_Value;
      Rc             : out Rule_Command) return Boolean;
   --  Create a Rule_Command value with the given name and arguments and
   --  store it in ``Rc``. Return ``True`` if this succeeded, ie. the file
   --  corresponds to a rule file, ``False`` otherwise.

end Rule_Commands;

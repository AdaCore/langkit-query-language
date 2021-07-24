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

with Ada.Containers.Vectors;

with LKQL.Eval_Contexts; use LKQL.Eval_Contexts;

with Liblkqllang.Analysis;

with Libadalang.Analysis;

with Langkit_Support.Diagnostics; use Langkit_Support.Diagnostics;
with Langkit_Support.Text; use Langkit_Support.Text;

--  A diagnostic is composed of a collection of individual rule commands
package Rule_Commands is

   package L renames Liblkqllang.Analysis;
   package LAL renames Libadalang.Analysis;

   Rule_Error : exception;

   type Rule_Argument is record
      Name  : Unbounded_Text_Type;
      --  Name of the argument

      Value : Unbounded_Text_Type;
      --  Value of the argument, as a string.
   end record;

   package Rule_Argument_Vectors is new
     Ada.Containers.Vectors (Positive, Rule_Argument);

   type Rule_Param_Kind is
     (No_Param, One_Integer, One_Boolean, One_String, One_Integer_Or_Booleans,
      Custom);
   --  Specifies how parameters are handled by the rule

   type Remediation_Levels is (Trivial, Easy, Medium, Major, High, Complex);
   --  Difficulty to address the rule violation.

   type Rule_Command is tagged record
      Name          : Unbounded_Text_Type;
      --  Name of the Rule

      Message       : Unbounded_Text_Type;
      --  Diagnostic messages associated with the rule.
      --  Defaults to Name if not specified.

      Help          : Unbounded_Text_Type;
      --  Short help message associated with the rule.
      --  Defaults to Message if not specified.

      LKQL_Root     : L.LKQL_Node;
      --  Root of the LKQL AST

      Rule_Args    : Rule_Argument_Vectors.Vector;
      --  Optional arguments to pass to the rule. Empty by default.

      Code          : L.LKQL_Node;
      --  Store (cache) the code for the rule, so as to not recompute it
      --  everytime we want to evaluate it.

      Is_Unit_Check : Boolean;
      --  Whether the rule is expressed via a boolean check (function that
      --  returns a boolean) or a node check (function that returns a node).

      Eval_Ctx      : Eval_Context;
      --  LKQL eval context in which to eval the rule. Each rule will have a
      --  separate frame, so as to not leak identifier from one rule to the
      --  other.

      Kind_Pattern  : L.Node_Kind_Pattern;
      --  If we determined that the rule only needs to run on a specific node
      --  kind, store the corresponding node pattern here.

      Follow_Instantiations : Boolean;
      --  Whether we should follow generic instantiations or not for this rule.

      Param_Kind : Rule_Param_Kind;
      --  Category of parameters.

      Parameters : L.Parameter_Decl_List;
      --  List of formal parameters for this rule.

      Remediation_Level : Remediation_Levels;
      --  Remediation level to compute technical debt.

      Parametric_Exemption : Boolean;
      --  Whether this rule allows parametric exemption.
   end record;

   type Output_Style is (Default, GNATcheck);
   --  Style of output messages.
   --  Default: gcc style colored output with source highlighting.
   --  GNATcheck: gnatcheck default output with one line per detection.

   type Eval_Diagnostic is record
      Diag : Diagnostic;
      Unit : Libadalang.Analysis.Analysis_Unit;
   end record;

   package Eval_Diagnostic_Vectors
   is new Ada.Containers.Vectors (Positive, Eval_Diagnostic);

   function Evaluate
     (Self : Rule_Command;
      Ctx  : Eval_Context)
      return Eval_Diagnostic_Vectors.Vector;
   --  Execute the LKQL script of the rule and return a Rule_Result value
   --  containing the flagged nodes.

   procedure Prepare (Self : in out Rule_Command);

   function Create_Rule_Command
     (LKQL_File_Path : String;
      Ctx            : Eval_Context;
      Rc             : out Rule_Command) return Boolean;
   --  Create a Rule_Command value with the given name and arguments and
   --  store it in ``Rc``. Return ``True`` if this succeeded, ie. the file
   --  corresponds to a rule file, ``False`` otherwise.

   procedure Destroy (Self : in out Rule_Command);
   --  Destroy the rule and associated data

end Rule_Commands;

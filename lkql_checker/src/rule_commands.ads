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

with LKQL.Primitives;    use LKQL.Primitives;
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

   package Rule_Argument_Vectors
   is new Ada.Containers.Vectors (Positive, Rule_Argument);

   type Rule_Command is tagged record
      Name          : Unbounded_Text_Type;
      --  Name of the Rule

      Message       : Unbounded_Text_Type;
      --  Diagnostic messages associated with the rule.
      --  Defaults to Name if not specified.

      LKQL_Root     : L.LKQL_Node;
      --  Root of the LKQL AST

      LKQL_Context  : L.Analysis_Context;

      Rule_Args    : Rule_Argument_Vectors.Vector;
      --  Optional arguments to pass to the rule. Empty by default.

      Code          : L.LKQL_Node;
      --  Store (cache) the code for the rule, so as to not recompute it
      --  everytime we want to evaluate it.

      Is_Node_Check : Boolean;
      --  Whether the rule is expressed via a boolean check (function that
      --  returns a boolean) or a node check (function that returns a node).
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
      Ctx            : L.Analysis_Context) return Rule_Command;
   --  Create a Rule_Command value with the given name and arguments

   procedure Check_Kind
     (Expected_Kind : Valid_Primitive_Kind; Actual_Kind : Valid_Primitive_Kind;
      Context       : String);
   --  Raise a Rule_error if 'Expected_Kind' is different from 'Actual_Kind'.
   --  The error message will start with the context String.

end Rule_Commands;

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

with Ada.Directories; use Ada.Directories;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Wide_Wide_Text_IO; use Ada.Wide_Wide_Text_IO;
with Ada.Wide_Wide_Characters.Handling; use Ada.Wide_Wide_Characters.Handling;

with Langkit_Support.Images; use Langkit_Support.Images;
with Langkit_Support.Slocs; use Langkit_Support.Slocs;
with Langkit_Support.Diagnostics.Output;

with Libadalang.Common; use Libadalang.Common;
with Libadalang.Introspection;

with Rules_Factory; use Rules_Factory;

with LKQL.Eval_Contexts; use LKQL.Eval_Contexts;
with Ada_AST_Nodes; use Ada_AST_Nodes;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;
with Ada.Strings.Wide_Wide_Unbounded.Wide_Wide_Hash;
with Ada.Containers.Hashed_Maps;
with LKQL.Primitives; use LKQL.Primitives;
with LKQL.Evaluation; use LKQL.Evaluation;

with Langkit_Support.Diagnostics; use Langkit_Support.Diagnostics;
with LKQL.AST_Nodes; use LKQL.AST_Nodes;
with LKQL.Errors; use LKQL.Errors;
with Liblkqllang.Analysis;

package body Checker_App is

   Ctx : Eval_Context;

   package Rules_Args_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Unbounded_Text_Type,
      Element_Type    => Rule_Argument_Vectors.Vector,
      Hash            => Ada.Strings.Wide_Wide_Unbounded.Wide_Wide_Hash,
      Equivalent_Keys => Ada.Strings.Wide_Wide_Unbounded."=",
      "="             => Rule_Argument_Vectors."=");

   type Rules_By_Kind is array (Ada_Node_Kind_Type) of Rule_Vector;

   procedure Process_Rules;
   --  Process input rules: Put the rules that have been requested by the user
   --  in the ``Cached_Rules`` and ``Cached_Flat_Rules`` data structures.

   Cached_Rules          : Rules_By_Kind
     := (others => Rule_Vectors.Empty_Vector);
   --  Data structure mapping node kinds to the checks that should be ran when
   --  this node type is encountered.

   Cached_Flat_Rules     : Rule_Vector;
   --  Simple vector of requested rules

   Computed_Cached_Rules : Boolean := False;

   -----------
   -- Rules --
   -----------

   procedure Process_Rules is
      package LI renames Libadalang.Introspection;
      package LCO renames Libadalang.Common;

      --  TODO: This should be removed once we have Kind_First/Kind_Last on
      --  Node_Type_Id (U412-016).

      type Ada_Node_Kind_Set is array (Ada_Node_Kind_Type) of Boolean;
      --  A set of ada nodes, represented as a boolean array
      --  NOTE: Should we pack it ?

      function Kind_Set (Id : LCO.Node_Type_Id) return Ada_Node_Kind_Set;
      --  Return the ``Kind_Set`` corresponding to a given Type_Id. For a leaf
      --  node type (e.g. with no child types), it will return a set with only
      --  the bit for  this type set. For a non leaf node, will return an array
      --  with the bit for the type and all descendants set.

      procedure Append_Rule (Rule : Rule_Command);
      --  Append the given rule to ``Cached_Rules`` and ``Cached_Flat_Rules``.

      Explicit_Rules_Names : constant Args.Rules.Result_Array :=
         Args.Rules.Get;

      Additional_Rules_Dirs : constant Path_Array :=
         Path_Array (Args.Rules_Dirs.Get);

      Rules_Args_Map : Rules_Args_Maps.Map;
      --  Map from argument names to argument values.

      function Kind_Set (Id : LCO.Node_Type_Id) return Ada_Node_Kind_Set is

         procedure Internal (Id : LCO.Node_Type_Id);

         Ret : Ada_Node_Kind_Set := (others => False);

         --------------
         -- Internal --
         --------------

         procedure Internal (Id : LCO.Node_Type_Id) is
         begin
            if LI.Is_Concrete (Id) then
               Ret (LI.Kind_For (Id)) := True;
            end if;
            for T of LI.Derived_Types (Id) loop
               Internal (T);
            end loop;
         end Internal;

      begin
         Internal (Id);
         return Ret;
      end Kind_Set;

      use Rule_Vectors;

      procedure Append_Rule (Rule : Rule_Command) is
         use Liblkqllang.Analysis;
      begin
         if Rule.Kind_Pattern /= No_Node_Kind_Pattern then
            declare
               Type_Id : constant LCO.Node_Type_Id :=
                 LI.Lookup_DSL_Name (Rule.Kind_Pattern.F_Kind_Name.Text);
               KS      : constant Ada_Node_Kind_Set :=
                 Kind_Set (Type_Id);
            begin
               for I in KS'Range loop
                  if KS (I) then
                     Cached_Rules (I).Append (Rule);
                  end if;
               end loop;
            end;
         else
            for I in Cached_Rules'Range loop
               Cached_Rules (I).Append (Rule);
            end loop;
         end if;
      end Append_Rule;

   begin
      if Computed_Cached_Rules then
         return;
      end if;

      Cached_Flat_Rules := All_Rules (Ctx, Additional_Rules_Dirs);

      --  Compute the map of argument names to values.

      for Rule_Arg of Args.Rules_Args.Get loop
         declare
            Dummy : Boolean;
            C     : Rules_Args_Maps.Cursor;
         begin
            Rules_Args_Map.Insert
              (Rule_Arg.Rule_Name,
               Rule_Argument_Vectors.Empty_Vector,
               C, Dummy);

            Rules_Args_Map.Reference (C).Append (Rule_Arg.Arg);
         end;
      end loop;

      --  Then, process potential arguments for those rules

      for Rule of Cached_Flat_Rules loop
         declare
            Rule_Name : constant Unbounded_Text_Type := Rule.Name;
            C         : constant Rules_Args_Maps.Cursor
              := Rules_Args_Map.Find (Rule_Name);
         begin
            --  Modify the rule command in place, by appending an argument to
            --  the Rule_Command's arg vector.

            Rule.Rule_Args.Append
              (Rule_Argument'(Name  => To_Unbounded_Text ("node"),
                              Value => To_Unbounded_Text ("node")));

            if Rules_Args_Maps.Has_Element (C) then
               for Arg of Rules_Args_Map.Reference (C) loop
                  Rule.Rule_Args.Append (Arg);
               end loop;
            end if;
         end;

         --  Call prepare *after* processing the arguments, since it needs the
         --  arguments processed.
         Rule.Prepare;
      end loop;

      --  First, process the set of rules that has to be ran.

      if Explicit_Rules_Names'Length = 0 then
         --  No rules passed by the user: return all rules
         for Rule of Cached_Flat_Rules loop
            Append_Rule (Rule);
         end loop;

      else
         --  Some rules passed by the user: only return the ones specified

         for R of Cached_Flat_Rules loop
            for Explicit_Rule_Name of Explicit_Rules_Names loop
               if To_Lower
                 (To_Text (To_String (Explicit_Rule_Name))) = To_Text (R.Name)
               then
                  Append_Rule (R);
               end if;
            end loop;
         end loop;
      end if;

      Computed_Cached_Rules := True;
   end Process_Rules;

   ---------------
   -- Job_Setup --
   ---------------

   procedure Job_Setup (Context : App_Job_Context) is
      Dummy : Primitive;
   begin
      Ctx := Make_Eval_Context (Context.Units_Processed);

      Process_Rules;

      for Rule of Cached_Flat_Rules loop
         --  Eval the rule's code (which should contain only definitions). TODO
         --  this should be encapsulated.
         begin
            Dummy := Eval (Rule.Eval_Ctx, Rule.LKQL_Root);
         exception
            when others =>
               Put (String'("internal error loading rule "));
               Put (To_Wide_Wide_String (Rule.Name));
               Put_Line (String'(":"));
               raise;
         end;
      end loop;

      --  Set property error recovery with the value of the command line flag.
      LKQL.Errors.Property_Error_Recovery := Args.Property_Error_Recovery.Get;
   end Job_Setup;

   ------------------
   -- Process_Unit --
   ------------------

   procedure Process_Unit
     (Context : App_Job_Context; Unit : Analysis_Unit)
   is
      pragma Unreferenced (Context);

      function Visit (Node : Ada_Node'Class) return Visit_Status;

      -----------
      -- Visit --
      -----------

      function Visit (Node : Ada_Node'Class) return Visit_Status is
         Result  : Primitive;
         Rc_Node : constant AST_Node_Rc :=
           Make_Ada_AST_Node (Node.As_Ada_Node);
      begin
         --  We add the binding to "node" to the root frame, so that it's
         --  accessible to every rule in its sub context. This is a bit
         --  hackish admittedly.
         Ctx.Add_Binding ("node", To_Primitive (Rc_Node));

         for Rule of Cached_Rules (Node.Kind) loop
            declare
               Result_Node : Ada_Node;
            begin
               if Rule.Is_Node_Check then

                  --  The check is a "node check", ie. a check that returns a
                  --  node on which to put the diagnostic, rather than a
                  --  boolean: The result node is the node directly returned by
                  --  the function

                  Result_Node :=
                    Ada_AST_Node
                      (Node_Val (Eval (Rule.Eval_Ctx, Rule.Code, Kind_Node))
                       .Unchecked_Get.all).Node;
               else

                  --  The check is a "bool check", ie. a check that returns a
                  --  boolean.  Eval the call to the check function

                  Result := Eval (Rule.Eval_Ctx, Rule.Code, Kind_Bool);

                  --  The result node is the current node, if the check
                  --  returned true.

                  Result_Node :=
                    (if Bool_Val (Result)
                     then Node.As_Ada_Node
                     else No_Ada_Node);
               end if;

               if Result_Node /= No_Ada_Node then

                  --  If the result node is a decl, grab its defining
                  --  identifier, so that the diagnostic spans only one line.
                  --  TODO: this logic could somehow be hoisted directly into
                  --  langkit diagnostics.

                  Result_Node :=
                    (if Result_Node.Kind in Ada_Basic_Decl and then
                        --  Some basic decls don't have a defining name,
                        --  e.g. Anonymous_Type_Decl.
                        not Result_Node.As_Basic_Decl.P_Defining_Name.Is_Null
                     then Result_Node.As_Basic_Decl.P_Defining_Name.As_Ada_Node
                     else Result_Node.As_Ada_Node);

                  case Args.Output_Style.Get is
                     when GNATcheck =>
                        Put
                          (Simple_Name (Result_Node.Unit.Get_Filename) & ":"
                           & Stripped_Image
                               (Integer (Result_Node.Sloc_Range.Start_Line))
                           & ":"
                           & Stripped_Image
                               (Integer (Result_Node.Sloc_Range.Start_Column))
                           & ": ");
                        Put_Line (To_Wide_Wide_String (Rule.Message));

                     when Default =>
                        declare
                           Diag : constant Eval_Diagnostic := Eval_Diagnostic'
                             (Diagnostic'
                                (Result_Node.Sloc_Range,
                                 To_Unbounded_Text (To_Text (Rule.Message))),
                              Result_Node.Unit);
                        begin
                           Langkit_Support.Diagnostics.Output.Print_Diagnostic
                             (Diag.Diag,
                              Diag.Unit,
                              Simple_Name (Diag.Unit.Get_Filename));
                        end;
                  end case;
               end if;
            exception
               when E : others =>
                  Put (String'("internal error processing rule "));
                  Put (To_Wide_Wide_String (Rule.Name));

                  --  If Last_Error is set and the Sloc points to some
                  --  interesting line (line 1 typically corresponds to some
                  --  internal/inlined context), then display this extra info.

                  if Is_Error (Rule.Eval_Ctx.Last_Error) then
                     declare
                        Node : constant Liblkqllang.Analysis.LKQL_Node :=
                          Rule.Eval_Ctx.Last_Error.AST_Node;
                     begin
                        if Node.Sloc_Range.Start_Line /= 1 then
                           Put (" [" &
                                Simple_Name (Node.Unit.Get_Filename) & ":" &
                                Stripped_Image
                                  (Integer (Node.Sloc_Range.Start_Line)) &
                                "]");
                        end if;
                     end;
                  end if;

                  Put_Line
                    (" at " & Simple_Name (Node.Unit.Get_Filename) & ":" &
                     Stripped_Image (Integer (Node.Sloc_Range.Start_Line)) &
                     ": " & Exception_Information (E));
            end;
         end loop;

         return Into;
      end Visit;
   begin
      Traverse (Unit.Root, Visit'Access);
   end Process_Unit;

   ----------------------
   -- App_Post_Process --
   ----------------------

   procedure App_Post_Process
     (Context : App_Context;
      Jobs    : App_Job_Context_Array)
   is
      pragma Unreferenced (Context, Jobs);
   begin
      Finalize_Rules (Ctx);
   end App_Post_Process;

   package body Args is

      -------------
      -- Convert --
      -------------

      function Convert (Raw_Arg : String) return Qualified_Rule_Argument is
         First_Dot   : constant Natural :=
           Index (Raw_Arg, Pattern => ".");
         First_Equal : constant Natural :=
           Index (Raw_Arg, Pattern => "=", From => First_Dot);
         Ret         : Qualified_Rule_Argument;
      begin
         if First_Dot = 0 or First_Equal = 0 then
            raise Opt_Parse_Error
              with "Wrong format for rule argument: " & Raw_Arg;
         end if;
         Ret.Rule_Name :=
           To_Unbounded_Text
             (To_Lower (To_Text (Raw_Arg (Raw_Arg'First .. First_Dot - 1))));
         Ret.Arg.Name :=
           To_Unbounded_Text
             (To_Text (Raw_Arg (First_Dot + 1 .. First_Equal - 1)));
         Ret.Arg.Value :=
           To_Unbounded_Text
             (To_Text (Raw_Arg (First_Equal + 1 .. Raw_Arg'Last)));

         return Ret;
      end Convert;

   end Args;

end Checker_App;

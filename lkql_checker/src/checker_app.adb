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

with Ada.Assertions; use Ada.Assertions;
with Ada.Characters.Conversions; use Ada.Characters.Conversions;
with Ada.Directories; use Ada.Directories;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO;
with Ada.Unchecked_Deallocation;
with Ada.Wide_Wide_Text_IO; use Ada.Wide_Wide_Text_IO;
with Ada.Wide_Wide_Characters.Handling; use Ada.Wide_Wide_Characters.Handling;

with Langkit_Support.Diagnostics.Output;
with Langkit_Support.Images; use Langkit_Support.Images;

with Libadalang.Introspection;

with Ada_AST_Nodes; use Ada_AST_Nodes;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;
with Ada.Strings.Wide_Wide_Unbounded.Wide_Wide_Hash;
with Ada.Containers.Hashed_Maps;
with LKQL.Primitives; use LKQL.Primitives;
with LKQL.Evaluation; use LKQL.Evaluation;

with Langkit_Support.Diagnostics; use Langkit_Support.Diagnostics;
with LKQL.Errors; use LKQL.Errors;
with Liblkqllang.Analysis;

with GNAT.Traceback.Symbolic;
with GNATCOLL.Terminal; use GNATCOLL.Terminal;
with LKQL.Partial_AST_Nodes; use LKQL.Partial_AST_Nodes;

package body Checker_App is

   type LKQL_Context_Array is array (Job_ID range <>) of LKQL_Context_Access;
   --  Array of LKQL_Contexts

   type LKQL_Context_Array_Access is access all LKQL_Context_Array;
   --  Access to array of contexts

   LKQL_Contexts : LKQL_Context_Array_Access := null;
   --  Global reference to an array of LKQL contexts. Each Job will get one
   --  context.

   function Get_Context (ID : Job_ID) return LKQL_Context_Access
   is (LKQL_Contexts (ID));
   --  Helper to get the context corresponding to a job ID

   ------------------
   -- Process_Unit --
   ------------------

   procedure Process_Unit
     (Ctx          : LKQL_Context;
      Unit         : Analysis_Unit;
      Emit_Message :
        access procedure (Message    : Unbounded_Text_Type;
                          Unit       : Analysis_Unit;
                          Rule       : Unbounded_Text_Type;
                          Kind       : Message_Kinds;
                          Sloc_Range : Source_Location_Range) := null)
   is
      procedure Handle_Error
        (Rule : Rule_Command;
         Node : Ada_Node'Class;
         Exc  : Exception_Occurrence);
      --  Factorize the error handling code, so that it can be shared amongst
      --  the two kinds of checkers, node checkers and unit checkers.

      function Strip_LF (S : String) return String is
      (if S (S'Last) = ASCII.LF then S (S'First .. S'Last - 1) else S);
      --  Remove trailing LF if any

      ------------------
      -- Handle_Error --
      ------------------

      procedure Handle_Error
        (Rule : Rule_Command;
         Node : Ada_Node'Class;
         Exc  : Exception_Occurrence) is
      begin
         declare
            Data      : constant Error_Data := Ctx.Eval_Ctx.Last_Error;
            LKQL_Node : constant LKQL.L.LKQL_Node := Data.AST_Node;
            Diag      : constant Diagnostic :=
              (Sloc_Range => LKQL_Node.Sloc_Range,
               Message    => Data.Short_Message);
            E         : Exception_Occurrence_Access :=
              Data.Property_Error_Info;

            procedure Unchecked_Free is new Ada.Unchecked_Deallocation
              (Exception_Occurrence, Exception_Occurrence_Access);

            procedure Internal_Error (Msg : Wide_Wide_String);
            --  Call Emit_Message to store an internal error message

            procedure Internal_Error (Msg : Wide_Wide_String) is
            begin
               Emit_Message
                 (To_Unbounded_Wide_Wide_String
                    (Msg & " at " &
                     To_Wide_Wide_String
                       (Simple_Name (LKQL_Node.Unit.Get_Filename)) & ":" &
                     To_Wide_Wide_String
                       (Stripped_Image
                         (Integer (LKQL_Node.Sloc_Range.Start_Line))) &
                     ":" &
                     To_Wide_Wide_String
                       (Stripped_Image
                         (Integer (LKQL_Node.Sloc_Range.Start_Column))) &
                     ": " &
                     To_Wide_Wide_String
                       (Strip_LF
                         (Exception_Information
                           (if E /= null then E.all else Exc)))),
                  Node.Unit, Rule.Name, Internal_Error, Node.Sloc_Range);
            end Internal_Error;

         begin
            case Property_Error_Recovery is
            when Continue_And_Log =>
               if Emit_Message /= null then
                  Internal_Error ("internal warning");
               else
                  Eval_Trace.Trace ("Evaluating rule predicate failed");
                  Eval_Trace.Trace ("rule => " & Image (To_Text (Rule.Name)));
                  Eval_Trace.Trace ("ada node => " & Node.Image);

                  if E /= null then
                     Eval_Trace.Trace (Exception_Information (E.all));
                     Eval_Trace.Trace
                       (GNAT.Traceback.Symbolic.Symbolic_Traceback
                          (E.all));
                  end if;
               end if;

            when Continue_And_Warn =>
               if Emit_Message /= null then
                  Internal_Error ("internal error");
               else
                  Put ("ERROR! evaluating rule predicate failed");

                  if E /= null then
                     Put_Line (" in a property call");
                  end if;

                  Put_Line (" on node => " & To_Text (Node.Image));

                  Langkit_Support.Diagnostics.Output.Print_Diagnostic
                    (Self        => Diag,
                     Buffer      => LKQL_Node.Unit,
                     Path        => LKQL_Node.Unit.Get_Filename,
                     Output_File => Standard_Error);

                  if E /= null then
                     Ada.Text_IO.Put_Line
                       (Ada.Text_IO.Standard_Error,
                        Exception_Information (E.all));
                     Ada.Text_IO.Put_Line
                       (Ada.Text_IO.Standard_Error,
                        GNAT.Traceback.Symbolic.Symbolic_Traceback (E.all));
                  end if;
               end if;

            when Raise_Error =>
               Reraise_Occurrence (Exc);
            end case;

            --  If we didn't raise and there is exception information
            --  linked to a wrapped property error, free it.

            Unchecked_Free (E);
         end;
      end Handle_Error;

      In_Generic_Instantiation : Boolean := False;
      --  Track whether we are in the traversal of a generic instantiation, to
      --  only call rules that want to follow generic instantiations.

      function Visit (Node : Ada_Node'Class) return Visit_Status;

      -----------
      -- Visit --
      -----------

      function Visit (Node : Ada_Node'Class) return Visit_Status is
         Rc_Node : constant H.AST_Node_Holder :=
           Make_Ada_AST_Node (Node.As_Ada_Node);
         In_Generic_Instantiation_Old_Val : Boolean;

      begin
         if Ctx.Traverse_Instantiations
           and then Node.Kind in Ada_Generic_Instantiation
         then
            --  Save old value, and set In_Generic_Instantiation to true
            In_Generic_Instantiation_Old_Val := In_Generic_Instantiation;
            In_Generic_Instantiation := True;
            Traverse
              (Node.As_Generic_Instantiation.P_Designated_Generic_Decl,
               Visit'Access);

            --  Also traverse the body of the generic, if there is one
            declare
               Generic_Body : constant Body_Node :=
                 Node.As_Generic_Instantiation.
                   P_Designated_Generic_Decl.P_Body_Part_For_Decl;
            begin
               if not Generic_Body.Is_Null then
                  Traverse (Generic_Body, Visit'Access);
               end if;
            end;

            --  Restore old value
            In_Generic_Instantiation := In_Generic_Instantiation_Old_Val;
         end if;

         Mark (Ctx.Eval_Ctx.Pools);

         Ctx.Eval_Ctx.Add_Binding
           ("node", To_Primitive (Rc_Node, Ctx.Eval_Ctx.Pool));

         for Rule of Ctx.Cached_Rules (Node.Kind) loop

            --  Skip unit check rules

            if Rule.Is_Unit_Check then
               goto Next;
            end if;

            --  If we are in a generic instantiation and the rule doesn't care
            --  about them, bail out.

            if In_Generic_Instantiation and then not Rule.Follow_Instantiations
            then
               goto Next;
            end if;

            declare
               Result_Node : Ada_Node;
            begin
               Rule.Eval_Ctx.Add_Binding
                 ("node", To_Primitive (Rc_Node, Rule.Eval_Ctx.Pool));

               --  The check is a "bool check", ie. a check that returns a
               --  boolean.  Eval the call to the check function

               if Bool_Val
                 (Eval (Rule.Eval_Ctx, Rule.Function_Expr, Kind_Bool))
               then

                  --  The result node is the current node

                  Result_Node := Node.As_Ada_Node;

                  --  If the result node is a decl, grab its defining
                  --  identifier, so that the diagnostic spans only one line.
                  --  TODO: this logic could somehow be hoisted directly into
                  --  langkit diagnostics.

                  if Result_Node.Kind in Ada_Basic_Decl and then
                     --  Some basic decls don't have a defining name,
                     --  e.g. Anonymous_Type_Decl.
                    not Result_Node.As_Basic_Decl.P_Defining_Name.Is_Null
                  then
                     Result_Node :=
                       Result_Node.As_Basic_Decl.P_Defining_Name.As_Ada_Node;
                  end if;

                  if Emit_Message /= null then
                     if not Rule.Follow_Instantiations then
                        Emit_Message
                          (Rule.Message, Result_Node.Unit, Rule.Name,
                           Rule_Violation, Result_Node.Sloc_Range);
                     else
                        declare
                           Insts : constant Generic_Instantiation_Array :=
                             Result_Node.P_Generic_Instantiations;
                           Msg   : Unbounded_Text_Type := Rule.Message;

                        begin
                           --  For generic instantiations, append
                           --  [instance at file:line [file:line [...]]]

                           for J in Insts'Range loop
                              if J = Insts'First then
                                 Append (Msg, " [instance at ");
                              else
                                 Append (Msg, " [");
                              end if;

                              Append (Msg,
                                      To_Wide_Wide_String (Simple_Name
                                        (Insts (J).Unit.Get_Filename)));
                              Append (Msg, ":");
                              Append (Msg,
                                      To_Wide_Wide_String (Stripped_Image
                                        (Integer (Insts (J).As_Ada_Node.
                                                  Sloc_Range.Start_Line))));
                           end loop;

                           for J in Insts'Range loop
                              Append (Msg, "]");
                           end loop;

                           Emit_Message
                             (Msg, Result_Node.Unit, Rule.Name,
                              Rule_Violation, Result_Node.Sloc_Range);
                        end;
                     end if;
                  else
                     declare
                        Diag : constant Eval_Diagnostic := Eval_Diagnostic'
                          (Diagnostic'
                             (Result_Node.Sloc_Range,
                              To_Unbounded_Text (To_Text (Rule.Message))),
                           Result_Node.Unit);
                     begin
                        Output.Print_Diagnostic
                          (Diag.Diag,
                           Diag.Unit,
                           Simple_Name (Diag.Unit.Get_Filename),
                           Style => Output.Diagnostic_Style'
                             (Label => To_Unbounded_Text ("rule violation"),
                              Color => Yellow));
                     end;
                  end if;
               end if;

            exception
               when E : LKQL.Errors.Stop_Evaluation_Error =>
                  Handle_Error (Rule, Node, E);
               when E : others =>
                  if Emit_Message /= null then
                     Emit_Message
                       (To_Unbounded_Wide_Wide_String
                          ("internal error on rule " &
                           To_Text (Rule.Name) & ": " &
                           To_Wide_Wide_String
                             (Strip_LF (Exception_Information (E)))),
                        Node.Unit, Rule.Name, Internal_Error, Node.Sloc_Range);

                  else
                     Put_Line
                       (Standard_Error,
                        "Evaluating query predicate failed unrecoverably");
                     Put_Line
                       (Standard_Error, "rule => " & To_Text (Rule.Name));
                     Put_Line
                       (Standard_Error, "ada node => " & To_Text (Node.Image));
                     Ada.Text_IO.Put_Line (Exception_Information (E));
                     Ada.Text_IO.Put_Line
                       (GNAT.Traceback.Symbolic.Symbolic_Traceback (E));
                  end if;
            end;

            <<Next>>
         end loop;

         Release (Ctx.Eval_Ctx.Pools);
         return Into;
      end Visit;

      List : Primitive_List_Access;

   begin
      --  Run node checks
      Traverse (Unit.Root, Visit'Access);

      --  Run unit checks
      for Rule of Ctx.Cached_Rules (Unit.Root.Kind) loop
         begin
            Mark (Rule.Eval_Ctx.Pools);

            if Rule.Is_Unit_Check then
               declare
                  Result : Primitive;
               begin
                  Rule.Eval_Ctx.Add_Binding
                    ("unit",
                     To_Primitive
                       (H.Create_Unit_Ref (Ada_AST_Unit'(Unit => Unit)),
                        Rule.Eval_Ctx.Pool));

                  Result := Eval (Rule.Eval_Ctx, Rule.Code);

                  if Result.Kind = Kind_Iterator then
                     Consume (Result);
                     List := Result.Iter_Cache;
                  else
                     Check_Kind
                       (Rule.Eval_Ctx, Rule.LKQL_Root, Kind_List, Result);

                     List := Result.List_Val;
                  end if;

                  for El of List.Elements loop
                     Check_Kind
                       (Rule.Eval_Ctx, Rule.LKQL_Root, Kind_Object, El);

                     declare
                        Loc_Val : constant Primitive :=
                          Extract_Value (El, "loc", Rule.Eval_Ctx, No_Kind,
                                         Location => Rule.LKQL_Root);

                        Loc : Source_Location_Range;

                        Message : constant Unbounded_Text_Type :=
                          To_Unbounded_Text
                            (Extract_Value
                               (El, "message", Rule.Eval_Ctx, Kind_Str,
                                Location => Rule.LKQL_Root).Str_Val.all);

                        Diag     : Diagnostic;
                        Loc_Unit : Analysis_Unit;

                     begin
                        --  Loc can be either a token value or a node value. In
                        --  both cases we'll extract the source location and
                        --  the unit from it.

                        if Loc_Val.Kind = Kind_Node then
                           declare
                              Node : constant Ada_AST_Node :=
                                 Ada_AST_Node
                                   (Loc_Val.Node_Val.Unchecked_Get.all);
                           begin
                              Loc := Node.Node.Sloc_Range;
                              Loc_Unit := Node.Node.Unit;
                           end;

                        elsif Loc_Val.Kind = Kind_Token then
                           declare
                              Token : constant Ada_AST_Token :=
                                Ada_AST_Token
                                  (Loc_Val.Token_Val.Unchecked_Get.all);
                           begin
                              Loc := Token.Sloc_Range;
                              Loc_Unit := Token.Unit;
                           end;
                        end if;

                        if Emit_Message /= null then
                           Emit_Message
                             (Message, Loc_Unit, Rule.Name,
                              Rule_Violation, Loc);
                        else
                           Diag := (Message => Message, Sloc_Range => Loc);
                           Output.Print_Diagnostic
                             (Self        => Diag,
                              Buffer      => Loc_Unit,
                              Path        =>
                                Simple_Name (Loc_Unit.Get_Filename),
                              Style       => Output.Diagnostic_Style'
                               (Label => To_Unbounded_Text ("rule violation"),
                                Color => Yellow));
                        end if;
                     end;
                  end loop;

               exception
                  when E : LKQL.Errors.Stop_Evaluation_Error =>
                     Handle_Error (Rule, Unit.Root, E);
               end;
            end if;

            Release (Rule.Eval_Ctx.Pools);
         exception
            when E : LKQL.Errors.Stop_Evaluation_Error | Assertion_Error =>
               Release (Rule.Eval_Ctx.Pools);
               Handle_Error (Rule, Unit.Root, E);
         end;
      end loop;
   end Process_Unit;

   procedure Process_Unit (Context : App_Job_Context; Unit : Analysis_Unit) is
      procedure No_Message
        (Message    : Unbounded_Text_Type;
         Unit       : Analysis_Unit;
         Rule       : Unbounded_Text_Type;
         Kind       : Message_Kinds;
         Sloc_Range : Source_Location_Range) is null;

      procedure Emit_Message
        (Message    : Unbounded_Text_Type;
         Unit       : Analysis_Unit;
         Rule       : Unbounded_Text_Type;
         Kind       : Message_Kinds;
         Sloc_Range : Source_Location_Range);
      --  Callback to emit a gnatcheck-like message on stdout

      ------------------
      -- Emit_Message --
      ------------------

      procedure Emit_Message
        (Message    : Unbounded_Text_Type;
         Unit       : Analysis_Unit;
         Rule       : Unbounded_Text_Type;
         Kind       : Message_Kinds;
         Sloc_Range : Source_Location_Range)
      is
         pragma Unreferenced (Rule, Kind);
      begin
         Ada.Text_IO.Put
           (Simple_Name (Unit.Get_Filename) & ":"
            & Stripped_Image
                (Integer (Sloc_Range.Start_Line))
            & ":"
            & Stripped_Image
                (Integer (Sloc_Range.Start_Column))
            & ": ");
         Put_Line (To_Text (Message));
      end Emit_Message;

   begin
      Process_Unit
        (Get_Context (Context.ID).all, Unit,
         (if Args.Output_Style.Get = GNATcheck
          then Emit_Message'Access
          elsif Args.Output_Style.Get = Silent
          then No_Message'Access
          else null));
   end Process_Unit;

   package Rules_Args_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Unbounded_Text_Type,
      Element_Type    => Rule_Argument_Vectors.Vector,
      Hash            => Ada.Strings.Wide_Wide_Unbounded.Wide_Wide_Hash,
      Equivalent_Keys => Ada.Strings.Wide_Wide_Unbounded."=",
      "="             => Rule_Argument_Vectors."=");

   procedure Process_Rules (Ctx : in out LKQL_Context);
   --  Process input rules: Put the rules that have been requested by the user
   --  in the ``Cached_Rules`` data structures.

   -----------
   -- Rules --
   -----------

   procedure Process_Rules (Ctx : in out LKQL_Context) is
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
      --  Append the given rule to ``Cached_Rules``.

      Explicit_Rules_Names : constant Args.Rules.Result_Array :=
        Args.Rules.Get;

      Additional_Rules_Dirs : Path_Vector;

      Rules_Args_Map : Rules_Args_Maps.Map;
      --  Map from argument names to argument values.

      function Kind_Set (Id : LCO.Node_Type_Id) return Ada_Node_Kind_Set is

         procedure Internal (Id : LCO.Node_Type_Id);

         Ret : Ada_Node_Kind_Set := [others => False];

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
                     Ctx.Cached_Rules (I).Append (Rule);
                  end if;
               end loop;
            end;
         else
            for I in Ctx.Cached_Rules'Range loop
               Ctx.Cached_Rules (I).Append (Rule);
            end loop;
         end if;

         --  If we have one rule that needs to follow instantiations, then set
         --  the traversal to traverse them.

         if Rule.Follow_Instantiations then
            Ctx.Traverse_Instantiations := True;
         end if;
      end Append_Rule;

   begin
      if not Ctx.All_Rules.Is_Empty then
         return;
      end if;

      for Dir of Args.Rules_Dirs.Get loop
         Additional_Rules_Dirs.Append (To_String (Dir));
      end loop;

      Ctx.All_Rules := All_Rules (Ctx.Eval_Ctx, Additional_Rules_Dirs);

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

      for Rule of Ctx.All_Rules loop
         declare
            Rule_Name : constant Unbounded_Text_Type := Rule.Name;
            C         : constant Rules_Args_Maps.Cursor
              := Rules_Args_Map.Find (Rule_Name);
         begin
            --  Modify the rule command in place, by appending an argument to
            --  the Rule_Command's arg vector.

            if Rule.Is_Unit_Check then
               Rule.Rule_Args.Append
                 (Rule_Argument'(Name  => To_Unbounded_Text ("unit"),
                                 Value => To_Unbounded_Text ("unit")));
            else
               Rule.Rule_Args.Append
                 (Rule_Argument'(Name  => To_Unbounded_Text ("node"),
                                 Value => To_Unbounded_Text ("node")));
            end if;

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
         for Rule of Ctx.All_Rules loop
            Append_Rule (Rule);
         end loop;

      else
         --  Some rules passed by the user: only return the ones specified
         for Explicit_Rule_Name of Explicit_Rules_Names loop
            declare
               Found : Boolean := False;
            begin
               for R of Ctx.All_Rules loop
                  if To_Lower (To_Text (To_String (Explicit_Rule_Name)))
                    = To_Text (R.Name)
                  then
                     Append_Rule (R);
                     Found := True;
                  end if;
               end loop;
               if not Found then
                  raise Exit_App with "no such rule - "
                    & To_String (Explicit_Rule_Name);
               end if;
            end;
         end loop;
      end if;
   end Process_Rules;

   procedure App_Setup
     (Context : App_Context; Jobs : App_Job_Context_Array)
   is
      pragma Unreferenced (Context);
   begin
      LKQL_Contexts := new LKQL_Context_Array (Jobs'Range);
      for I in LKQL_Contexts'Range loop
         LKQL_Contexts (I) := new LKQL_Context;
      end loop;
   end App_Setup;

   ---------------
   -- Job_Setup --
   ---------------

   procedure Job_Setup (Context : App_Job_Context) is
      Dummy : Primitive;
      Units : Unit_Vectors.Vector;
      Files : String_Vectors.Vector;

   begin
      declare
         Ctx : LKQL_Context_Access renames Get_Context (Context.ID);
      begin

         case Context.App_Ctx.Provider.Kind is
            when Project_File =>
               List_Sources_From_Project
                 (Context.App_Ctx.Provider.Project.all, False, Files);

               for F of Files loop
                  Units.Append
                    (Context.Analysis_Ctx.Get_From_File (To_String (F)));
               end loop;

            when Default =>
               for F of App.Args.Files.Get loop
                  Units.Append
                    (Context.Analysis_Ctx.Get_From_File (To_String (F)));
               end loop;

            when others =>
               --  ??? Should we worry about this case and fill Units
               null;
         end case;

         Ctx.Eval_Ctx := Make_Eval_Context (Units);
         Ctx.Analysis_Ctx := Context.Analysis_Ctx;
         Process_Rules (Get_Context (Context.ID).all);

         for Rule of Ctx.All_Rules loop
            --  Eval the rule's code (which should contain only definitions).
            --  TODO this should be encapsulated.
            begin
               Dummy := Eval (Rule.Eval_Ctx, Rule.LKQL_Root);
            exception
               when others =>
                  Put ("internal error loading rule ");
                  Put (To_Wide_Wide_String (Rule.Name));
                  Put_Line (":");
                  raise;
            end;
         end loop;

         --  Set property error recovery with the value of the command line
         --  flag.
         LKQL.Errors.Property_Error_Recovery
           := Args.Property_Error_Recovery.Get;
      end;
   end Job_Setup;

   procedure Job_Post_Process (Context : App_Job_Context) is
      Ctx : LKQL_Context renames Get_Context (Context.ID).all;
   begin
      Finalize_Rules (Ctx.Eval_Ctx);
      Free_Eval_Context (Ctx.Eval_Ctx);
   end Job_Post_Process;

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

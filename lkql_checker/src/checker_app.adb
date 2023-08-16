------------------------------------------------------------------------------
--                                                                          --
--                                   LKQL                                   --
--                                                                          --
--                     Copyright (C) 2019-2023, AdaCore                     --
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
with Ada.Directories; use Ada.Directories;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO;
with Ada.Unchecked_Deallocation;
with Ada.Wide_Wide_Text_IO; use Ada.Wide_Wide_Text_IO;

with Langkit_Support.Diagnostics.Output;
with Langkit_Support.Images; use Langkit_Support.Images;

with Libadalang.Config_Pragmas;
with Libadalang.Project_Provider; use Libadalang.Project_Provider;
with Libadalang.Generic_API; use Libadalang.Generic_API;

with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;
with Ada.Strings.Wide_Wide_Unbounded.Wide_Wide_Hash;
with Ada.Containers.Hashed_Maps;
with LKQL.Primitives; use LKQL.Primitives;
with LKQL.Evaluation; use LKQL.Evaluation;

with Langkit_Support.Diagnostics; use Langkit_Support.Diagnostics;
with LKQL.Errors; use LKQL.Errors;
with Liblkqllang.Analysis;

with GNAT.OS_Lib;
with GNAT.Traceback.Symbolic;

with GNATCOLL.Terminal; use GNATCOLL.Terminal;
with GNATCOLL.Utils;

package body Checker_App is

   use Langkit_Support.Generic_API;

   type Lkql_Context_Array is array (Job_ID range <>) of Lkql_Context_Access;
   --  Array of Lkql_Contexts

   type Lkql_Context_Array_Access is access all Lkql_Context_Array;
   --  Access to array of contexts

   Lkql_Contexts : Lkql_Context_Array_Access := null;
   --  Global reference to an array of LKQL contexts. Each Job will get one
   --  context.

   function Get_Context (ID : Job_ID) return Lkql_Context_Access
   is (Lkql_Contexts (ID));
   --  Helper to get the context corresponding to a job ID

   procedure Process_Rules (Ctx : in out Lkql_Context);
   --  Process input rules: Put the rules that have been requested by the user
   --  in the ``Cached_Rules`` data structures.

   ------------------
   -- Process_Unit --
   ------------------

   procedure Process_Unit
     (Ctx          : Lkql_Context;
      Unit         : Analysis_Unit;
      Emit_Message :
        access procedure (Message    : Unbounded_Text_Type;
                          Unit       : LK.Lk_Unit;
                          Rule       : Unbounded_Text_Type;
                          Kind       : Message_Kinds;
                          Sloc_Range : Source_Location_Range) := null)
   is
      Lk_Unit : constant LK.Lk_Unit := To_Generic_Unit (Unit);

      Ada_Node_T : constant LKI.Type_Ref :=
        Ctx.Eval_Ctx.Get_Name_Map.Lookup_Type
         (Ctx.Eval_Ctx.Symbol ("AdaNode"));

      Generic_Instantiation : constant LKI.Type_Ref :=
        Ctx.Eval_Ctx.Get_Name_Map.Lookup_Type
         (Ctx.Eval_Ctx.Symbol ("GenericInstantiation"));

      Basic_Decl : constant LKI.Type_Ref :=
        Ctx.Eval_Ctx.Get_Name_Map.Lookup_Type
         (Ctx.Eval_Ctx.Symbol ("BasicDecl"));

      Body_Stub : constant LKI.Type_Ref :=
        Ctx.Eval_Ctx.Get_Name_Map.Lookup_Type
         (Ctx.Eval_Ctx.Symbol ("BodyStub"));

      Designated_Generic_Decl : constant LKI.Struct_Member_Ref :=
        Ctx.Eval_Ctx.Get_Name_Map.Lookup_Struct_Member
          (Generic_Instantiation,
           Ctx.Eval_Ctx.Symbol ("p_designated_generic_decl"));

      Body_Part_For_Decl : constant LKI.Struct_Member_Ref :=
        Ctx.Eval_Ctx.Get_Name_Map.Lookup_Struct_Member
          (Basic_Decl,
           Ctx.Eval_Ctx.Symbol ("p_body_part_for_decl"));

      Next_Part_For_Decl : constant LKI.Struct_Member_Ref :=
        Ctx.Eval_Ctx.Get_Name_Map.Lookup_Struct_Member
          (Basic_Decl,
           Ctx.Eval_Ctx.Symbol ("p_next_part_for_decl"));

      Defining_Name : constant LKI.Struct_Member_Ref :=
        Ctx.Eval_Ctx.Get_Name_Map.Lookup_Struct_Member
          (Basic_Decl,
           Ctx.Eval_Ctx.Symbol ("p_defining_name"));

      Generic_Instantiations : constant LKI.Struct_Member_Ref :=
        Ctx.Eval_Ctx.Get_Name_Map.Lookup_Struct_Member
          (Ada_Node_T,
           Ctx.Eval_Ctx.Symbol ("p_generic_instantiations"));

      procedure Handle_Error
        (Rule   : Rule_Command;
         Node : LK.Lk_Node;
         Exc    : Exception_Occurrence;
         Severe : Boolean);
      --  Factorize the error handling code, so that it can be shared amongst
      --  the two kinds of checkers, node checkers and unit checkers.
      --  "Severe" flags severe errors that should not be hidden.

      function Strip_LF (S : String) return String is
      (if S (S'Last) = ASCII.LF then S (S'First .. S'Last - 1) else S);
      --  Remove trailing LF if any

      ------------------
      -- Handle_Error --
      ------------------

      procedure Handle_Error
        (Rule : Rule_Command;
         Node   : LK.Lk_Node;
         Exc    : Exception_Occurrence;
         Severe : Boolean)
      is
      begin
         declare
            Data      : constant Error_Data := Ctx.Eval_Ctx.Last_Error;
            Lkql_Node : constant LKQL.L.Lkql_Node := Data.AST_Node;
            Diag      : constant Diagnostic :=
              (Sloc_Range => Lkql_Node.Sloc_Range,
               Message    => Data.Short_Message);
            E         : Exception_Occurrence_Access :=
              Data.Property_Error_Info;

            procedure Unchecked_Free is new Ada.Unchecked_Deallocation
              (Exception_Occurrence, Exception_Occurrence_Access);

            procedure Internal_Error (Msg : Wide_Wide_String);
            --  Call Emit_Message to store an internal error message

            procedure Internal_Error (Msg : Wide_Wide_String) is
               Exception_Msg : constant String :=
                 Strip_LF
                   (Exception_Information (if E /= null then E.all else Exc));
               Type_Error    : constant Boolean :=
                 Index (Exception_Msg, "Type error:") /= 0;

            begin
               Emit_Message
                 (To_Unbounded_Text
                    (Msg & " at " &
                     To_Text
                       (Simple_Name (Lkql_Node.Unit.Get_Filename)) & ":" &
                     To_Text
                       (Stripped_Image
                         (Integer (Lkql_Node.Sloc_Range.Start_Line))) &
                     ":" &
                     To_Text
                       (Stripped_Image
                         (Integer (Lkql_Node.Sloc_Range.Start_Column))) &
                     ": " &
                     To_Text (Exception_Msg)),
                  Node.Unit, Rule.Name,
                  (if Severe or else Type_Error
                   then Severe_Internal_Error else Internal_Error),
                  Node.Sloc_Range);
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
                     Buffer      => Lkql_Node.Unit,
                     Path        => Lkql_Node.Unit.Get_Filename,
                     Output_File => Ada.Text_IO.Standard_Error);

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

      function Visit (Node : LK.Lk_Node) return LK.Visit_Status;

      -----------
      -- Visit --
      -----------

      function Visit (Node : LK.Lk_Node) return LK.Visit_Status is
         In_Generic_Instantiation_Old_Val : Boolean;
      begin
         if Ctx.Traverse_Instantiations then
            if LKI.Type_Matches
                 (LKI.From_Node
                   (LK.Language (Node), Node), Generic_Instantiation)
            then
               --  Save old value, and set In_Generic_Instantiation to true
               In_Generic_Instantiation_Old_Val := In_Generic_Instantiation;
               In_Generic_Instantiation := True;

               declare
                  Gen_Decl : constant LK.Lk_Node := LKI.As_Node
                    (LKI.Eval_Node_Member
                      (Node, Designated_Generic_Decl));

                  Gen_Body : constant LK.Lk_Node := LKI.As_Node
                    (LKI.Eval_Node_Member
                      (Gen_Decl,
                       Body_Part_For_Decl,
                       [LKI.From_Bool (Ada_Lang_Id, False)]));
               begin
                  LK.Traverse (Gen_Decl, Visit'Access);

                  --  Also traverse the body of the generic, if there is one
                  if not Gen_Body.Is_Null then
                     LK.Traverse (Gen_Body, Visit'Access);
                  end if;
               end;

               --  Restore old value
               In_Generic_Instantiation := In_Generic_Instantiation_Old_Val;

            --  Also traverse stub bodies if already part of an instantiation

            elsif In_Generic_Instantiation
              and then LKI.Type_Matches
                         (LKI.From_Node (LK.Language (Node), Node), Body_Stub)
            then
               declare
                  Separate_Body : constant LK.Lk_Node := LKI.As_Node
                    (LKI.Eval_Node_Member
                      (Node,
                       Next_Part_For_Decl,
                       [LKI.From_Bool (Ada_Lang_Id, False)]));
               begin
                  LK.Traverse (Separate_Body, Visit'Access);
               end;
            end if;
         end if;

         Mark (Ctx.Eval_Ctx.Pools);

         Ctx.Eval_Ctx.Add_Binding
           ("node", To_Primitive (Node, Ctx.Eval_Ctx.Pool));

         for Rule of Ctx.Cached_Rules (LKI.To_Index (LKI.Type_Of (Node))) loop

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
               Result_Node : LK.Lk_Node;
            begin
               Rule.Eval_Ctx.Add_Binding
                 ("node", To_Primitive (Node, Rule.Eval_Ctx.Pool));

               --  The check is a "bool check", ie. a check that returns a
               --  boolean.  Eval the call to the check function

               if Bool_Val
                 (Eval (Rule.Eval_Ctx, Rule.Function_Expr, Kind_Bool))
               then

                  --  The result node is the current node

                  Result_Node := Node;

                  --  If the result node is a decl, grab its defining
                  --  identifier, so that the diagnostic spans only one line.
                  --  TODO: this logic could somehow be hoisted directly into
                  --  langkit diagnostics.

                  if LKI.Type_Matches
                    (LKI.From_Node (Result_Node.Language, Result_Node),
                     Basic_Decl)
                  then
                     declare
                        DN : constant LK.Lk_Node := LKI.As_Node
                          (LKI.Eval_Node_Member (Result_Node, Defining_Name));
                     begin
                        --  Some basic decls don't have a defining name,
                        --  e.g. Anonymous_Type_Decl.
                        if not DN.Is_Null then
                           Result_Node := DN;
                        end if;
                     end;
                  end if;

                  if Emit_Message /= null then
                     if not Rule.Follow_Instantiations then
                        Emit_Message
                          (Rule.Message, Result_Node.Unit, Rule.Name,
                           Rule_Violation, Result_Node.Sloc_Range);
                     else
                        declare
                           Insts : constant LKI.Value_Ref_Array :=
                             LKI.As_Array
                               (LKI.Eval_Node_Member
                                 (Result_Node, Generic_Instantiations));

                           Msg   : Unbounded_Text_Type := Rule.Message;

                        begin
                           --  For generic instantiations, append
                           --  [instance at file:line [file:line [...]]]

                           for J in Insts'Range loop
                              declare
                                 N : constant LK.Lk_Node :=
                                   LKI.As_Node (Insts (J));
                              begin
                                 if J = Insts'First then
                                    Append (Msg, " [instance at ");
                                 else
                                    Append (Msg, " [");
                                 end if;

                                 Append (Msg,
                                         To_Text (Simple_Name
                                           (N.Unit.Filename)));
                                 Append (Msg, ":");
                                 Append
                                   (Msg,
                                    To_Text
                                      (Stripped_Image
                                        (Integer (N.Sloc_Range.Start_Line))));
                              end;
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
                           Simple_Name (Diag.Unit.Filename),
                           Style => Output.Diagnostic_Style'
                             (Label => To_Unbounded_Text ("rule violation"),
                              Color => Yellow));
                     end;
                  end if;
               end if;

            exception
               when E : LKQL.Errors.Stop_Evaluation_Error =>
                  Handle_Error (Rule, Node, E, Severe => False);
               when E : others =>
                  if Emit_Message /= null then
                     Emit_Message
                       (To_Unbounded_Wide_Wide_String
                          ("internal error on rule " &
                           To_Text (Rule.Name) & ": " &
                           To_Text (Strip_LF (Exception_Information (E)))),
                        Node.Unit, Rule.Name,
                        Severe_Internal_Error, Node.Sloc_Range);

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
         return LK.Into;
      end Visit;

      List : Primitive_List_Access;

   begin
      --  Run node checks
      LK.Traverse (Lk_Unit.Root, Visit'Access);

      --  Run unit checks
      for Rule of Ctx.Cached_Rules (LKI.To_Index (LKI.Type_Of (Lk_Unit.Root)))
      loop
         begin
            Mark (Rule.Eval_Ctx.Pools);

            if Rule.Is_Unit_Check then
               declare
                  Result : Primitive;
               begin
                  Rule.Eval_Ctx.Add_Binding
                    ("unit", To_Primitive (Lk_Unit, Rule.Eval_Ctx.Pool));

                  Result := Eval (Rule.Eval_Ctx, Rule.Code);

                  if Result.Kind = Kind_Iterator then
                     Consume (Result);
                     List := Result.Iter_Cache;
                  else
                     Check_Kind
                       (Rule.Eval_Ctx, Rule.Lkql_Root, Kind_List, Result);

                     List := Result.List_Val;
                  end if;

                  for El of List.Elements loop
                     Check_Kind
                       (Rule.Eval_Ctx, Rule.Lkql_Root, Kind_Object, El);

                     declare
                        Loc_Val : constant Primitive :=
                          Extract_Value (El, "loc", Rule.Eval_Ctx, No_Kind,
                                         Location => Rule.Lkql_Root);

                        Loc : Source_Location_Range;

                        Message : constant Unbounded_Text_Type :=
                          To_Unbounded_Text
                            (Extract_Value
                               (El, "message", Rule.Eval_Ctx, Kind_Str,
                                Location => Rule.Lkql_Root).Str_Val.all);

                        Diag     : Diagnostic;
                        Loc_Unit : LK.Lk_Unit;

                     begin
                        --  Loc can be either a token value or a node value. In
                        --  both cases we'll extract the source location and
                        --  the unit from it.

                        if Loc_Val.Kind = Kind_Node then
                           declare
                              Node : constant LK.Lk_Node := Loc_Val.Node_Val;
                           begin
                              Loc := Node.Sloc_Range;
                              Loc_Unit := Node.Unit;
                           end;

                        elsif Loc_Val.Kind = Kind_Token then
                           declare
                              Token : constant LK.Lk_Token :=
                                Loc_Val.Token_Val;
                           begin
                              Loc := Token.Sloc_Range;
                              Loc_Unit := Token.Unit;
                           end;
                        end if;

                        if Emit_Message /= null then
                           if Loc_Val.Kind = Kind_Node then
                              declare
                                 Insts : constant LKI.Value_Ref_Array :=
                                   LKI.As_Array
                                     (LKI.Eval_Node_Member
                                       (Loc_Val.Node_Val,
                                        Generic_Instantiations));
                                 Msg   : Unbounded_Text_Type := Message;

                              begin
                                 --  For generic instantiations, append
                                 --  [instance at file:line [file:line [...]]]

                                 for J in Insts'Range loop
                                    declare
                                       N : constant LK.Lk_Node :=
                                         LKI.As_Node (Insts (J));
                                    begin
                                       if J = Insts'First then
                                          Append (Msg, " [instance at ");
                                       else
                                          Append (Msg, " [");
                                       end if;

                                       Append (Msg,
                                               To_Text (Simple_Name
                                                 (N.Unit.Filename)));
                                       Append (Msg, ":");
                                       Append
                                         (Msg,
                                          To_Text
                                            (Stripped_Image
                                              (Integer
                                                (N.Sloc_Range.Start_Line))));
                                    end;
                                 end loop;

                                 for J in Insts'Range loop
                                    Append (Msg, "]");
                                 end loop;

                                 Emit_Message
                                   (Msg, Loc_Unit, Rule.Name,
                                    Rule_Violation, Loc);
                              end;
                           else
                              Emit_Message
                                (Message, Loc_Unit, Rule.Name,
                                 Rule_Violation, Loc);
                           end if;
                        else
                           Diag := (Message => Message, Sloc_Range => Loc);

                           Output.Print_Diagnostic
                             (Self        => Diag,
                              Buffer      => Loc_Unit,
                              Path        =>
                                Simple_Name (Loc_Unit.Filename),
                              Style       => Output.Diagnostic_Style'
                               (Label => To_Unbounded_Text ("rule violation"),
                                Color => Yellow));
                        end if;
                     end;
                  end loop;

               exception
                  when E : LKQL.Errors.Stop_Evaluation_Error =>
                     Handle_Error (Rule, Lk_Unit.Root, E, Severe => True);
               end;
            end if;

            Release (Rule.Eval_Ctx.Pools);
         exception
            when E : LKQL.Errors.Stop_Evaluation_Error | Assertion_Error =>
               Release (Rule.Eval_Ctx.Pools);
               Handle_Error (Rule, Lk_Unit.Root, E, Severe => True);
         end;
      end loop;
   end Process_Unit;

   procedure Process_Unit (Context : App_Job_Context; Unit : Analysis_Unit) is
      procedure No_Message
        (Message    : Unbounded_Text_Type;
         Unit       : LK.Lk_Unit;
         Rule       : Unbounded_Text_Type;
         Kind       : Message_Kinds;
         Sloc_Range : Source_Location_Range) is null;

      procedure Emit_Message
        (Message    : Unbounded_Text_Type;
         Unit       : LK.Lk_Unit;
         Rule       : Unbounded_Text_Type;
         Kind       : Message_Kinds;
         Sloc_Range : Source_Location_Range);
      --  Callback to emit a gnatcheck-like message on stdout

      ------------------
      -- Emit_Message --
      ------------------

      procedure Emit_Message
        (Message    : Unbounded_Text_Type;
         Unit       : LK.Lk_Unit;
         Rule       : Unbounded_Text_Type;
         Kind       : Message_Kinds;
         Sloc_Range : Source_Location_Range)
      is
         pragma Unreferenced (Rule, Kind);
      begin
         Ada.Text_IO.Put
           (Simple_Name (Unit.Filename) & ":"
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

   -----------------
   -- Append_Rule --
   -----------------

   procedure Append_Rule (Ctx : in out Lkql_Context; Rule : Rule_Command) is
      use Liblkqllang.Analysis;
   begin
      if Rule.Kind_Pattern /= No_Node_Kind_Pattern then
         declare
            Type_Ref : constant LKI.Type_Ref :=
              Ctx.Eval_Ctx.Get_Name_Map.Lookup_Type
               (Ctx.Eval_Ctx.Symbol (Rule.Kind_Pattern.F_Kind_Name.Text));

            Type_Id : constant LKI.Type_Index := LKI.To_Index (Type_Ref);
         begin
            for I in Type_Id .. LKI.Last_Derived_Type (Type_Ref) loop
               Ctx.Cached_Rules (I).Append (Rule);
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

   -----------
   -- Rules --
   -----------

   procedure Process_Rules (Ctx : in out Lkql_Context) is

      Explicit_Rules_Names : constant Args.Rules.Result_Array :=
        Args.Rules.Get;

      Additional_Rules_Dirs : Path_Vector;

      Rules_Args_Map : Rules_Args_Maps.Map;
      --  Map from argument names to argument values.

      function Add_Rule_Dir (Path : String) return Boolean;
      --  Add the given path to the list of directories in which to look for
      --  LKQL rules.

      function Add_Rule_Dir (Path : String) return Boolean is
      begin
         Additional_Rules_Dirs.Append (Path);
         return True;
      end Add_Rule_Dir;

      use Rule_Vectors;
   begin
      if not Ctx.All_Rules.Is_Empty then
         return;
      end if;

      GNATCOLL.Utils.Split
        (To_String (Args.Rules_Dirs.Get),
         GNAT.OS_Lib.Path_Separator & "",
         Add_Rule_Dir'Access);

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
            Append_Rule (Ctx, Rule);
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
                     Append_Rule (Ctx, R);
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
      Lkql_Contexts := new Lkql_Context_Array (Jobs'Range);
      for I in Lkql_Contexts'Range loop
         Lkql_Contexts (I) := new Lkql_Context;
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
         Ctx : Lkql_Context_Access renames Get_Context (Context.ID);
      begin

         case Context.App_Ctx.Provider.Kind is
            when Project_File =>
               Files := Source_Files (Context.App_Ctx.Provider.Project.all);

               for F of Files loop
                  Units.Append
                    (Context.Analysis_Ctx.Get_From_File (To_String (F)));
               end loop;

               --  Setup the configuration pragma mapping by reading the
               --  configuration file given by the project.
               Libadalang.Config_Pragmas.Import_From_Project
                 (Context.Analysis_Ctx, Context.App_Ctx.Provider.Project.all);

            when Default =>
               for F of App.Args.Files.Get loop
                  Units.Append
                    (Context.Analysis_Ctx.Get_From_File (To_String (F)));
               end loop;

            when others =>
               --  ??? Should we worry about this case and fill Units
               null;
         end case;

         declare
            Roots : LK.Lk_Node_Array (Units.First_Index .. Units.Last_Index);
         begin

            for I in Roots'Range loop
               Roots (I) :=
                 To_Generic_Unit (Units (I)).Root;
            end loop;

            Ctx.Eval_Ctx := Make_Eval_Context
              (Roots, Ada_Lang_Id);
            Ctx.Analysis_Ctx := Context.Analysis_Ctx;

            --  Initialize the cached rules array, with an array that goes from
            --  the index of the first root node type, to the index of the last
            --  derived type. This array will have too many slots since is has
            --  slots for abstract types, but we don't really care.
            declare
               Root_Node_Type : LKI.Type_Ref
                 renames LKI.Root_Node_Type (Ada_Lang_Id);
               subtype Rules_By_Kind_Array_Subt is
                 Rules_By_Kind_Array
                   (LKI.To_Index (Root_Node_Type)
                    .. LKI.Last_Derived_Type (Root_Node_Type));

            begin
               Ctx.Cached_Rules := new Rules_By_Kind_Array_Subt;
            end;

            Process_Rules (Get_Context (Context.ID).all);

            for Rule of Ctx.All_Rules loop
               --  Eval the rule's code (which should contain only
               --  definitions).  TODO this should be encapsulated.
               begin
                  Dummy := Eval (Rule.Eval_Ctx, Rule.Lkql_Root);
               exception
                  when others =>
                     Put ("internal error loading rule ");
                     Put (To_Text (Rule.Name));
                     Put_Line (":");
                     raise;
               end;
            end loop;

            --  Set property error recovery with the value of the command line
            --  flag.
            LKQL.Errors.Property_Error_Recovery
              := Args.Property_Error_Recovery.Get;
         end;
      end;
   end Job_Setup;

   procedure Job_Post_Process (Context : App_Job_Context) is
      Ctx : Lkql_Context renames Get_Context (Context.ID).all;
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

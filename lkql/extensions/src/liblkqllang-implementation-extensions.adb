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
with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;

with GNATCOLL.Projects; use GNATCOLL.Projects;

with Langkit_Support.Diagnostics.Output;
with Langkit_Support.Text; use Langkit_Support.Text;
with Langkit_Support.Symbols; use Langkit_Support.Symbols;

with Liblkqllang.Analysis;
with Liblkqllang.Public_Converters;

with Libadalang.Project_Provider; use Libadalang.Project_Provider;
with Libadalang.Helpers; use Libadalang.Helpers;
with Libadalang.Analysis; use Libadalang.Analysis;
with Libadalang.Introspection; use Libadalang.Introspection;
with Libadalang.Common;

with Ada_AST_Nodes; use Ada_AST_Nodes;
with LKQL.Evaluation;
with LKQL.Primitives; use LKQL.Primitives;
with LKQL.Errors;
with LKQL.Eval_Contexts; use LKQL.Eval_Contexts;
with LKQL.AST_Nodes; use LKQL.AST_Nodes;
with LKQL.Unit_Utils;

package body Liblkqllang.Implementation.Extensions is

   package LALCO renames Libadalang.Common;

   function Get_All_Completions_For_Id
     (Id : Ada_AST_Nodes.Node_Type_Id;
      In_Pattern : Boolean := False) return Unbounded_Text_Array;
   --  Get all valid completions (fields & properties) for a given
   --  Node_Type_Id.

   function Units return Unit_Vectors.Vector;
   --  Return all the units for the LKQL context.

   function Eval
     (Ctx : Eval_Context; Node : Analysis.LKQL_Node'Class)
      return Primitive;
   --  Evaluate the given node in the given context. Also ensures that
   --  the unit in which ``Node`` belongs has been pre-processed.

   --  TODO: for the moment the state is global, we need to store it in the
   --  LKQL context.
   Ctx      : Libadalang.Analysis.Analysis_Context;
   Files    : String_Vectors.Vector;
   LKQL_Ctx : Eval_Context;
   Project  : Project_Tree_Access;
   Env      : Project_Environment_Access;
   Init     : Boolean := False;

   -----------
   -- Units --
   -----------

   function Units return Unit_Vectors.Vector is
      Ret : Unit_Vectors.Vector;
   begin
      for F of Files loop
         Ret.Append (Ctx.Get_From_File (To_String (F)));
      end loop;
      return Ret;
   end Units;

   ----------
   -- Eval --
   ----------

   function Eval
     (Ctx : Eval_Context; Node : Analysis.LKQL_Node'Class)
      return Primitive
   is
   begin
      LKQL.Unit_Utils.Run_Preprocessor (Node.Unit);
      return LKQL.Evaluation.Eval (Ctx, Node);
   end Eval;

   ------------------------------------------
   -- LKQL_Node_P_Interp_Init_From_Project --
   ------------------------------------------

   function LKQL_Node_P_Interp_Init_From_Project
     (Node : Bare_LKQL_Node; Project_File : String_Type) return Boolean
   is
      UFP : Unit_Provider_Reference;
   begin
      --  If already init, it means this is called for a second time: In that
      --  case we want to reinitialize.
      if Init then
         --  No need to explicitly finalize Analysis context, since it's a
         --  controlled type.

         --  Free the LKQL eval context
         Free_Eval_Context (LKQL_Ctx);

         Free (Project);
         Free (Env);
      end if;

      Libadalang.Helpers.Load_Project
        (Image (Project_File.Content), Project => Project, Env => Env);

      Files := Source_Files (Project.all);

      UFP := Project_To_Provider (Project);
      Ctx := Create_Context (Charset => "utf-8", Unit_Provider => UFP);

      --  Use the context from this node to create the LKQL context.
      LKQL_Ctx := Make_Eval_Context
        (Units, Public_Converters.Wrap_Context (Node.Unit.Context));

      Init := True;
      return True;
   end LKQL_Node_P_Interp_Init_From_Project;

   -----------------------------
   -- LKQL_Node_P_Interp_Eval --
   -----------------------------

   function LKQL_Node_P_Interp_Eval (Node : Bare_LKQL_Node) return Symbol_Type
   is
      Public_Unit : constant Analysis.Analysis_Unit
        := Public_Converters.Wrap_Unit (Node.Unit);
   begin
      if Node.Unit.Diagnostics.Length > 0 then
         for Diag of Node.Unit.Diagnostics loop
            Langkit_Support.Diagnostics.Output.Print_Diagnostic
              (Diag,
               Public_Unit,
               Simple_Name (Public_Unit.Get_Filename));
         end loop;
         return null;
      end if;

      return Find
           (Node.Unit.Context.Symbols,
            To_Text
              (To_Unbounded_Text
                   (Eval (LKQL_Ctx, Public_Converters.Wrap_Node (Node)))));
   exception
      when E : LKQL.Errors.Stop_Evaluation_Error =>
         return Find (Node.Unit.Context.Symbols,
                      To_Text ("<ERROR>: " & Exception_Message (E)));
   end LKQL_Node_P_Interp_Eval;

   --------------------------------
   -- Get_All_Completions_For_Id --
   --------------------------------

   function Get_All_Completions_For_Id
     (Id : Ada_AST_Nodes.Node_Type_Id;
      In_Pattern : Boolean := False) return Unbounded_Text_Array
   is
      Props  : constant LALCO.Property_Reference_Array := Properties (Id);
      Fields : constant LALCO.Syntax_Field_Reference_Array
        := Syntax_Fields (Id);
      Ret    : Unbounded_Text_Array (1 .. Props'Length + Fields'Length);
      Idx    : Positive := 1;
   begin
      for Field of Fields loop
         declare
            Val : Unbounded_Text_Type :=
              To_Unbounded_Text (Member_Name (Field));
         begin
            if In_Pattern then
               Append (Val, "=");
            end if;

            Ret (Idx) := Val;
            Idx := Idx + 1;
         end;
      end loop;

      for Prop of Props loop
         declare
            Val : Unbounded_Text_Type :=
              To_Unbounded_Text (Member_Name (Prop));
         begin
            if In_Pattern then
               if Property_Argument_Types (Prop)'Length > 0 then
                  Append (Val, "(");
               else
                  Append (Val, "() is");
               end if;
            end if;

            Ret (Idx) := Val;
            Idx := Idx + 1;
         end;
      end loop;

      return Ret;
   end Get_All_Completions_For_Id;

   ---------------------------------
   -- LKQL_Node_P_Interp_Complete --
   ---------------------------------

   function LKQL_Node_P_Interp_Complete
     (Node : Bare_LKQL_Node) return Symbol_Type_Array_Access
   is

      function Make_Sym_Array
        (Strings : Unbounded_Text_Array) return Symbol_Type_Array_Access;

      --------------------
      -- Make_Sym_Array --
      --------------------

      function Make_Sym_Array
        (Strings : Unbounded_Text_Array) return Symbol_Type_Array_Access
      is
         Ret : constant Symbol_Type_Array_Access :=
           Create_Symbol_Type_Array (Strings'Length);

         Idx : Positive := 1;
      begin
         for S of Strings loop
            Ret.Items (Idx)
              := Find (Node.Unit.Context.Symbols, To_Text (S));
            Idx := Idx + 1;
         end loop;

         return Ret;
      end Make_Sym_Array;

      use Liblkqllang.Analysis;

      PNode      : constant LKQL_Node := Public_Converters.Wrap_Node (Node);
      Last_Token : constant Token_Kind := Kind (Data (PNode.Token_End));

   begin

      --  Implement runtime based completion (a la IPython/etc) based on the
      --  syntactic content of the string.

      case Node.Kind is

      when LKQL_Query =>

         --  select |

         if Last_Token not in LKQL_Select_Tok then
            return No_Symbol_Type_Array_Type;
         end if;

         return Make_Sym_Array (Kind_Names);

      when LKQL_Node_Pattern_Property
         | LKQL_Node_Pattern_Field =>

         --  Node(a=|
         --  Node(a() is |

         if Last_Token not in LKQL_Eq | LKQL_Is then
            return No_Symbol_Type_Array_Type;
         end if;

         return Make_Sym_Array (Kind_Names);

      when LKQL_Extended_Node_Pattern =>

         --  Node(|

         if Last_Token not in LKQL_L_Par | LKQL_Coma then
            return No_Symbol_Type_Array_Type;
         end if;

         declare
            VP : constant Value_Pattern :=
              PNode.As_Extended_Node_Pattern.F_Node_Pattern;
         begin
            if VP.Kind = LKQL_Node_Kind_Pattern then
               return Make_Sym_Array
                 (Get_All_Completions_For_Id
                    (Kind (VP.As_Node_Kind_Pattern.F_Kind_Name.Text)));
            else
               return No_Symbol_Type_Array_Type;
            end if;
         end;

      when LKQL_Dot_Access =>
         declare
            LHS : constant Analysis.Expr := PNode.As_Dot_Access.F_Receiver;
         begin

            --  a.|
            --  where a is a dot access/simple identifier, not a complex expr

            if Last_Token /= LKQL_Dot
              or else LHS.Kind not in
                LKQL_Dot_Access | LKQL_Identifier | LKQL_Safe_Access
            then
               return No_Symbol_Type_Array_Type;
            end if;

            declare
               Val : constant Primitive := Eval (LKQL_Ctx, LHS);
            begin
               if Val.Kind = Kind_Node then
                  return Make_Sym_Array
                    (Get_All_Completions_For_Id
                       (Get_Node_Type_Id
                            (Ada_AST_Node
                                 (Val.Node_Val.Unchecked_Get.all))));
               end if;
            end;

            return No_Symbol_Type_Array_Type;

         end;
      when others =>
         return No_Symbol_Type_Array_Type;
      end case;
   end LKQL_Node_P_Interp_Complete;

end Liblkqllang.Implementation.Extensions;

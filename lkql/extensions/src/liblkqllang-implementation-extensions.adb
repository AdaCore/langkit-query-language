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
with Libadalang.Generic_API;

with LKQL.Evaluation;
with LKQL.Primitives; use LKQL.Primitives;
with LKQL.Errors;
with LKQL.Eval_Contexts; use LKQL.Eval_Contexts;
with LKQL.Unit_Utils;

package body Liblkqllang.Implementation.Extensions is

   use LKQL;

   function Get_All_Completions_For_Id
     (Id : LKI.Type_Ref;
      In_Pattern : Boolean := False) return Unbounded_Text_Array;
   --  Get all valid completions (fields & properties) for a given
   --  Node_Type_Id.

   function Roots return LK.Lk_Node_Array;
   --  Return all the units for the LKQL context.

   function Eval
     (Ctx : Eval_Context; Node : Analysis.Lkql_Node'Class)
      return Primitive;
   --  Evaluate the given node in the given context. Also ensures that
   --  the unit in which ``Node`` belongs has been pre-processed.

   function Node_Kind_Names return Unbounded_Text_Array;

   function Make_Sym_Array
     (Strings : Unbounded_Text_Array) return Symbol_Type_Array_Access;

   --  TODO: for the moment the state is global, we need to store it in the
   --  LKQL context.
   Ctx      : LK.Lk_Context;
   Files    : String_Vectors.Vector;
   Lkql_Ctx : Eval_Context;
   Project  : Project_Tree_Access;
   Env      : Project_Environment_Access;
   Init     : Boolean := False;

   ---------------------
   -- Node_Kind_Names --
   ---------------------

   function Node_Kind_Names return Unbounded_Text_Array is
      use LKI;

      Root_Node_Type : constant Type_Ref :=
        LKI.Root_Node_Type (Ctx.Language);
      First_Index : constant Type_Index := To_Index (Root_Node_Type);
      Last_Derived_Type_Index : constant Type_Index :=
        Last_Derived_Type (Root_Node_Type);
   begin
      return Ret : Unbounded_Text_Array
        (Positive (First_Index) .. Positive (Last_Derived_Type_Index))
      do
         for I in First_Index .. Last_Derived_Type_Index loop
            Ret (Positive (I)) := To_Unbounded_Text
              (LKN.Format_Name
                (Node_Type_Name
                  (From_Index (Ctx.Language, I)), LKN.Camel));
         end loop;
      end return;
   end Node_Kind_Names;

   -----------
   -- Roots --
   -----------

   function Roots return LK.Lk_Node_Array is
      Ret : LK.Lk_Node_Array (1 .. Natural (Files.Length));
   begin
      for J in Ret'First .. Ret'Last loop
         Ret (J) :=
           Ctx .Get_From_File (To_String (Files (J))).Root;
      end loop;

      return Ret;
   end Roots;

   ----------
   -- Eval --
   ----------

   function Eval
     (Ctx : Eval_Context; Node : Analysis.Lkql_Node'Class)
      return Primitive
   is
   begin
      LKQL.Unit_Utils.Run_Preprocessor (Ctx, Node.Unit);
      return LKQL.Evaluation.Eval (Ctx, Node);
   end Eval;

   ------------------------------------------
   -- Lkql_Node_P_Interp_Init_From_Project --
   ------------------------------------------

   function Lkql_Node_P_Interp_Init_From_Project
     (Node : Bare_Lkql_Node; Project_File : String_Type) return Boolean
   is
      UFP : Unit_Provider_Reference;
   begin
      --  If already init, it means this is called for a second time: In that
      --  case we want to reinitialize.
      if Init then
         --  No need to explicitly finalize Analysis context, since it's a
         --  controlled type.

         --  Free the LKQL eval context
         Free_Eval_Context (Lkql_Ctx);

         Free (Project);
         Free (Env);
      end if;

      Libadalang.Helpers.Load_Project
        (Image (Project_File.Content), Project => Project, Env => Env);

      Files := Source_Files (Project.all);

      UFP := Project_To_Provider (Project);
      Ctx := Libadalang.Generic_API.To_Generic_Context
        (Create_Context (Charset => "utf-8", Unit_Provider => UFP));

      --  Use the context from this node to create the LKQL context.
      Lkql_Ctx := Make_Eval_Context
        (Roots,
         Libadalang.Generic_API.Ada_Lang_Id,
         Public_Converters.Wrap_Context (Node.Unit.Context));

      Init := True;
      return True;
   end Lkql_Node_P_Interp_Init_From_Project;

   -----------------------------
   -- Lkql_Node_P_Interp_Eval --
   -----------------------------

   function Lkql_Node_P_Interp_Eval (Node : Bare_Lkql_Node) return Symbol_Type
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
                   (Eval (Lkql_Ctx, Public_Converters.Wrap_Node (Node)))));
   exception
      when E : LKQL.Errors.Stop_Evaluation_Error =>
         return Find (Node.Unit.Context.Symbols,
                      To_Text ("<ERROR>: " & Exception_Message (E)));
   end Lkql_Node_P_Interp_Eval;

   --------------------------------
   -- Get_All_Completions_For_Id --
   --------------------------------

   function Get_All_Completions_For_Id
     (Id : LKI.Type_Ref;
      In_Pattern : Boolean := False) return Unbounded_Text_Array
   is
      Members : constant LKI.Struct_Member_Ref_Array := LKI.Members (Id);
      Ret     : Unbounded_Text_Array (Members'Range);
   begin
      for J in Members'Range loop
         declare
            Val : Unbounded_Text_Type :=
              To_Unbounded_Text
                (LKN.Format_Name
                  (LKI.Member_Name (Members (J)), LKN.Lower));
         begin
            if In_Pattern then
               Append (Val, "=");
            end if;
         end;
      end loop;

      return Ret;
   end Get_All_Completions_For_Id;

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
         Ret.Items (Idx) := Symbol (Lkql_Ctx, To_Text (S));
         Idx := Idx + 1;
      end loop;

      return Ret;
   end Make_Sym_Array;

   ---------------------------------
   -- Lkql_Node_P_Interp_Complete --
   ---------------------------------

   function Lkql_Node_P_Interp_Complete
     (Node : Bare_Lkql_Node) return Symbol_Type_Array_Access
   is

      use Liblkqllang.Analysis;

      PNode      : constant Lkql_Node := Public_Converters.Wrap_Node (Node);
      Last_Token : constant Token_Kind := Kind (Data (PNode.Token_End));

   begin

      --  Implement runtime based completion (a la IPython/etc) based on the
      --  syntactic content of the string.

      case Node.Kind is

      when Lkql_Query =>

         --  select |

         if Last_Token not in Lkql_Select_Tok then
            return No_Symbol_Type_Array_Type;
         end if;

         return Make_Sym_Array (Node_Kind_Names);

      when Lkql_Node_Pattern_Property
         | Lkql_Node_Pattern_Field =>

         --  Node(a=|
         --  Node(a() is |

         if Last_Token not in Lkql_Eq | Lkql_Is then
            return No_Symbol_Type_Array_Type;
         end if;

         return Make_Sym_Array (Node_Kind_Names);

      when Lkql_Extended_Node_Pattern =>

         --  Node(|

         if Last_Token not in Lkql_L_Par | Lkql_Coma then
            return No_Symbol_Type_Array_Type;
         end if;

         declare
            VP : constant Value_Pattern :=
              PNode.As_Extended_Node_Pattern.F_Node_Pattern;
         begin
            if VP.Kind = Lkql_Node_Kind_Pattern then
               return Make_Sym_Array
                 (Get_All_Completions_For_Id
                    (Lkql_Ctx.Get_Name_Map.Lookup_Type
                      (Lkql_Ctx.Symbol
                        (VP.As_Node_Kind_Pattern.F_Kind_Name.Text))));
            else
               return No_Symbol_Type_Array_Type;
            end if;
         end;

      when Lkql_Dot_Access =>
         declare
            LHS : constant Analysis.Expr := PNode.As_Dot_Access.F_Receiver;
         begin

            --  a.|
            --  where a is a dot access/simple identifier, not a complex expr

            if Last_Token /= Lkql_Dot
              or else LHS.Kind not in
                Lkql_Dot_Access | Lkql_Identifier | Lkql_Safe_Access
            then
               return No_Symbol_Type_Array_Type;
            end if;

            declare
               Val : constant Primitive := Eval (Lkql_Ctx, LHS);
            begin
               if Val.Kind = Kind_Node then
                  return Make_Sym_Array
                    (Get_All_Completions_For_Id (LKI.Type_Of (Val.Node_Val)));
               end if;
            end;

            return No_Symbol_Type_Array_Type;

         end;
      when others =>
         return No_Symbol_Type_Array_Type;
      end case;
   end Lkql_Node_P_Interp_Complete;

end Liblkqllang.Implementation.Extensions;

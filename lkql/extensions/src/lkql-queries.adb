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

with GNAT.Traceback.Symbolic;

with Ada.Text_IO; use Ada.Text_IO;

with Langkit_Support.Text; use Langkit_Support.Text;
with Langkit_Support.Errors; use Langkit_Support.Errors;

with LKQL.Patterns;       use LKQL.Patterns;
with LKQL.Primitives;     use LKQL.Primitives;
with LKQL.Evaluation;     use LKQL.Evaluation;
with LKQL.Patterns.Match; use LKQL.Patterns.Match;
with LKQL.Error_Handling; use LKQL.Error_Handling;
with LKQL.Errors;         use LKQL.Errors;
with Ada.Exceptions; use Ada.Exceptions;

package body LKQL.Queries is

   -------------------------
   -- Make_Query_Iterator --
   -------------------------

   function Make_Query_Iterator (Ctx  : Eval_Context;
                                 Node : L.Query)
                                 return AST_Node_Iterator'Class
   is

      function Roots return AST_Node_Iterator_Access;

      -----------
      -- Roots --
      -----------

      function Roots return AST_Node_Iterator_Access is
      begin
         if Node.F_From_Expr.Is_Null then

            --  First case, there is no "from" in the query. In that case, the
            --  implicit roots of the query are the roots of the LKQL eval
            --  context.

            return new AST_Node_Iterator'Class'
              (AST_Node_Iterator'Class
                 (Make_Child_Iterator (Ctx.AST_Roots.all)));
         else

            --  Second case, there is a "from" clause in the query.

            declare
               --  First, eval the expression.
               Eval_From_Expr : constant Primitive :=
                 Eval (Ctx, Node.F_From_Expr);

               Vec : AST_Node_Vector;
            begin
               case Eval_From_Expr.Get.Kind is

                  --  If it's a single node, create an array with just this
                  --  element.
                  when Kind_Node =>
                     Vec.Append (Eval_From_Expr.Get.Node_Val);

                  --  If it's a list, it needs to be a list of nodes. Create a
                  --  vector from it to create the iterator from.
                  when Kind_List =>
                     for El of Eval_From_Expr.Get.List_Val.Elements loop
                        if El.Get.Kind /= Kind_Node then
                           --  TODO: For the moment it's impossible to exert
                           --  this check in queries, because only queries
                           --  return lists (comprehensions return iterators
                           --  and selectors selector lists). We need to unify
                           --  the sequence types somehow, because having a
                           --  list comprehension in a "from" appears
                           --  potentially useful, and is not possible yet.
                           Raise_And_Record_Error
                             (Ctx,
                              Make_Eval_Error
                                (Node.F_From_Expr.As_LKQL_Node,
                                 "Wrong kind of element in list for "
                                 & "`from clause`"));
                        end if;
                        Vec.Append (El.Get.Node_Val);
                     end loop;

                  --  If it's any other kind of node, then it's an error
                  when others =>
                     Raise_And_Record_Error
                       (Ctx,
                        Make_Eval_Error
                          (Node.F_From_Expr.As_LKQL_Node,
                           "Wrong kind of element in `from clause`"));
               end case;

               return new AST_Node_Iterator'Class'
                 (AST_Node_Iterator'Class
                    (Make_Child_Iterator (Vec)));
            end;
         end if;
      end Roots;
   begin
      case Node.F_Pattern.Kind is
      when LCO.LKQL_Chained_Node_Pattern_Range =>
         declare
            Chained : constant Chained_Pattern_Iterator :=
              Make_Chained_Pattern_Iterator
                (Ctx,
                 Roots,
                 Node.F_Pattern.P_Value_Part.As_Chained_Node_Pattern);
         begin
            return Chained_Pattern_Query_Iter'
              (Ctx       => Ctx.Ref_Frame,
               Iter      => Chained);
         end;
      when others =>
         declare
            Predicate : constant AST_Node_Predicate_Access :=
              AST_Node_Predicate_Access
                (Make_Query_Predicate (Ctx, Node.F_Pattern));
         begin
            return AST_Node_Iterators.Filter (Roots, Predicate);
         end;
      end case;
   end Make_Query_Iterator;

   --------------------------
   -- Make_Query_Predicate --
   --------------------------

   function Make_Query_Predicate
     (Ctx : Eval_Context; Pattern : L.Base_Pattern)
      return Query_Predicate_Access
   is
   begin
      return new Query_Predicate'(Ctx.Ref_Frame, Pattern);
   end Make_Query_Predicate;

   --------------
   -- Evaluate --
   --------------

   overriding function Evaluate
     (Self : in out Query_Predicate; Node : AST_Node_Rc) return Boolean
   is
   begin
      declare
         Match : constant Match_Result :=
           Match_Pattern (Self.Ctx,
                          Self.Pattern,
                          To_Primitive (Node));
      begin
         return Match.Is_Success;
      end;

   exception
      when E : Property_Error =>
         case Property_Error_Recovery is
            when Continue_And_Log =>
               Eval_Trace.Trace ("Evaluating query predicate failed");
               Eval_Trace.Trace ("pattern => " & Self.Pattern.Image);
               Eval_Trace.Trace ("ada node => " & Image (Node.Get.Text_Image));

               Eval_Trace.Trace (Exception_Information (E));
               Eval_Trace.Trace
                 (GNAT.Traceback.Symbolic.Symbolic_Traceback (E));

            when Continue_And_Warn =>
               --  TODO: Use Langkit_Support.Diagnostics.Output
               Put_Line (Standard_Error, "Evaluating query predicate failed");
               Put_Line (Standard_Error, "pattern => " & Self.Pattern.Image);
               Put_Line
                 (Standard_Error, "ada node => "
                  & Image (Node.Get.Text_Image));
            when Raise_Error =>
               raise;
         end case;

         return False;
   end Evaluate;

   -----------
   -- Clone --
   -----------

   overriding function Clone
     (Self : Query_Predicate) return Query_Predicate
   is
   begin
      return Query_Predicate'(Self.Ctx, Self.Pattern);
   end Clone;

   -------------
   -- Release --
   -------------

   overriding procedure Release (Self : in out Query_Predicate) is
   begin
      Self.Ctx.Release_Current_Frame;
   end Release;

   ----------
   -- Next --
   ----------

   overriding function Next (Iter   : in out Chained_Pattern_Query_Iter;
                             Result : out AST_Node_Rc) return Boolean
   is
      Match            : Match_Result;
   begin
      --  The inner iterator is empty: return false
      if not Iter.Iter.Next (Match) then
         return False;
      end if;

      Result := Node_Val (Match.Get_Matched_Value);
      return True;
   end Next;

   -----------
   -- Clone --
   -----------

   overriding function Clone (Iter : Chained_Pattern_Query_Iter)
                              return Chained_Pattern_Query_Iter
   is
     (Iter.Ctx.Ref_Frame, Iter.Iter.Clone);

   -------------
   -- Release --
   -------------

   overriding procedure Release (Iter : in out Chained_Pattern_Query_Iter) is
   begin
      Iter.Ctx.Release_Current_Frame;
      Iter.Iter.Release;
   end Release;

end LKQL.Queries;

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

with LKQL.Eval_Contexts;      use LKQL.Eval_Contexts;
with LKQL.Chained_Pattern;    use LKQL.Chained_Pattern;
with LKQL.Lk_Nodes_Iterators; use LKQL.Lk_Nodes_Iterators;

private package LKQL.Queries is

   function Make_Query_Iterator
     (Ctx  : Eval_Context;
      Node : L.Query) return Lk_Node_Iterator'Class;
   --  Returns an iterator over the AST nodes, yielding only the elements that
   --  belong to the result of the given query.

   ---------------------
   -- Query_Predicate --
   ---------------------

   type Query_Predicate is new Lk_Node_Iterator_Predicate with record
      Ctx     : Eval_Context;
      Pattern : L.Base_Pattern;
   end record;
   --  Predicate that returns true for every node that belongs to the
   --  result of the given query.

   type Query_Predicate_Access is access all Query_Predicate;
   --  Pointer to a Query_predicate

   function Make_Query_Predicate
     (Ctx : Eval_Context; Pattern : L.Base_Pattern)
      return Query_Predicate_Access;
   --  Return a pointer to a Query_Predicate that returns true for every node
   --  that belongs to the result set of the given query.

   overriding function Evaluate
     (Self : in out Query_Predicate; Node : Lk_Node) return Boolean;
   --  Evaluate the given predicate against 'Node'

   overriding function Clone
     (Self : Query_Predicate) return Query_Predicate;
   --  Return a copy of the given Query_Predicate

   overriding procedure Release (Self : in out Query_Predicate);

   --------------------------------
   -- Chained_Pattern_Query_Iter --
   --------------------------------

   type Chained_Pattern_Query_Iter is new Lk_Node_Iterator with record
      Ctx       : Eval_Context;
      Iter      : Chained_Pattern_Iterator;
   end record;

   overriding function Next (Iter   : in out Chained_Pattern_Query_Iter;
                             Result : out Lk_Node) return Boolean;

   overriding function Clone (Iter : Chained_Pattern_Query_Iter)
                              return Chained_Pattern_Query_Iter;

   overriding procedure Release (Iter : in out Chained_Pattern_Query_Iter);

end LKQL.Queries;

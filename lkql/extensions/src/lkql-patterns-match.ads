------------------------------------------------------------------------------
--                                                                          --
--                                   LKQL                                   --
--                                                                          --
--                     Copyright (C) 2019-2022, AdaCore                     --
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

with LKQL.Eval_Contexts; use LKQL.Eval_Contexts;

package LKQL.Patterns.Match is

   function Match_Pattern_Array (Ctx      : Eval_Context;
                                 Patterns : L.Base_Pattern_Array;
                                 Value    : Primitive)
                                 return Match_Array_Result;
   --  Match a value against an array of pattern.
   --  Return the index of the first successful match, along with the
   --  associated bindings, if any.

   function Match_Pattern (Ctx     : Eval_Context;
                           Pattern : L.Base_Pattern;
                           Value   : Primitive) return Match_Result;
   --  Match a Primitive value against the given pattern

   function Match_Unfiltered (Ctx     : Eval_Context;
                              Pattern : L.Unfiltered_Pattern;
                              Value   : Primitive) return Match_Result;
   --  Match a Primitive value against a pattern that doesn't contain a
   --  filtering predicate.

   function Match_Filtered (Ctx     : Eval_Context;
                            Pattern : L.Filtered_Pattern;
                            Value   : Primitive) return Match_Result;
   --  Match a Primitive value against a pattern that contains a filtering
   --  predicate.

   function Match_Value (Ctx     : Eval_Context;
                         Pattern : L.Value_Pattern;
                         Value   : Primitive) return Match_Result;
   --  Match a Primitive value against a value pattern, i.e a pattern that
   --  doesn't contain a binding name.

   function Match_Binding (Ctx     : Eval_Context;
                           Pattern : L.Binding_Pattern;
                           Value   : Primitive) return Match_Result;
   --  Match a Primitive value against a pattern that contains both a binding
   --  name and a value pattern.

   function Match_Regex (Ctx     : Eval_Context;
                         Pattern : L.Regex_Pattern;
                         Value   : Primitive) return Match_Result;
   --  Match a Primitive value against a regular expression.

end LKQL.Patterns.Match;

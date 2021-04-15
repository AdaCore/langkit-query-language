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
with LKQL.Unit_Utils;

package body Liblkqllang.Prelude is

   Prelude_Content : constant String :=
        "selector next_siblings" & ASCII.LF &
        "   | AdaNode => rec this.next_sibling" & ASCII.LF &
        "   | *       => ()" & ASCII.LF &
        ""                   & ASCII.LF &
        "selector prev_siblings" & ASCII.LF &
        "   | AdaNode => rec this.previous_sibling" & ASCII.LF &
        "   | *       => ()" & ASCII.LF &
        ""                   & ASCII.LF &
        "selector parent" & ASCII.LF &
        "   | AdaNode => rec *this.parent" & ASCII.LF &
        "   | *       => ()" & ASCII.LF &
        ""                   & ASCII.LF &
        "selector children" & ASCII.LF &
        "   | AdaNode => rec *this.children" & ASCII.LF &
        "   | *       => ()" & ASCII.LF &
        ""                   & ASCII.LF &
        "selector super_types" & ASCII.LF &
        "    | BaseTypeDecl      => rec *this.p_base_types()" & ASCII.LF &
        "    | *                 => ()" & ASCII.LF;

   ------------------
   -- Prelude_Unit --
   ------------------

   function Prelude_Unit (Context : Analysis_Context) return Analysis_Unit is
   begin
      return LKQL.Unit_Utils.Make_LKQL_Unit_From_Code
        (Context, Prelude_Content, "prelude");
   end Prelude_Unit;

end Liblkqllang.Prelude;

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

with Ada.Unchecked_Deallocation;

package body LKQL.Node_Extensions is
   -------------
   -- Destroy --
   -------------

   procedure Destroy (Self : in out Ext) is
      procedure Free_Pattern is new Ada.Unchecked_Deallocation
        (GNAT.Regpat.Pattern_Matcher, Regex_Matcher_Access);

      procedure Free_Node_Kind is new Ada.Unchecked_Deallocation
        (AST_Nodes.AST_Node_Kind'Class, AST_Node_Kind_Access);

      procedure Free is new Ada.Unchecked_Deallocation
        (LKQL_Node_Extension, Ext);
   begin
      case Self.Content.Kind is
         when LCO.LKQL_Regex_Pattern =>
            Free_Pattern (Self.Content.Compiled_Pattern);
         when LCO.LKQL_Node_Kind_Pattern =>
            Free_Node_Kind (Self.Content.Expected_Kind);
         when LCO.LKQL_Base_String_Literal =>
            Free (Self.Content.Denoted_Value);
         when others =>
            null;
      end case;
      Free (Self);
   end Destroy;
end LKQL.Node_Extensions;

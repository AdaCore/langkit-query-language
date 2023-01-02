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

with Liblkqllang.Analysis;
with Liblkqllang.Common;
with Langkit_Support.Symbols; use Langkit_Support.Symbols;
with Langkit_Support.Generic_API.Analysis;
with Langkit_Support.Generic_API.Introspection;
use Langkit_Support.Generic_API.Analysis;
with Langkit_Support.Names;
with Langkit_Support.Errors;

with Ada.Containers.Vectors;

package LKQL is
   package L renames Liblkqllang.Analysis;
   package LCO renames Liblkqllang.Common;
   package LK renames Langkit_Support.Generic_API.Analysis;
   package LKI renames Langkit_Support.Generic_API.Introspection;
   package LKN renames Langkit_Support.Names;
   package LKE renames Langkit_Support.Errors;

   function Node_Text (Self : L.Lkql_Node'Class) return String;
   --  Helper debug function. Return the text of a node as a string.

   function Symbol (Node : L.Identifier) return Symbol_Type;

   package Lk_Node_Vectors is new Ada.Containers.Vectors
     (Positive, Langkit_Support.Generic_API.Analysis.Lk_Node);

   subtype Lk_Node_Vector is Lk_Node_Vectors.Vector;

end LKQL;

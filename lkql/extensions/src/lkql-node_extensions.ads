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

with Ada.Containers.Hashed_Maps;
with GNAT.Regpat;

with Langkit_Support.Text; use Langkit_Support.Text;

with LKQL.AST_Nodes;

--  This package is the user facing parts of the LKQL node extension mechanism.
--  It allows us to extend nodes with pre-computed information stored in
--  ``Node_Ext`` records that are allocated and attached to LKQL nodes.

package LKQL.Node_Extensions is

   ------------------
   -- Custom data  --
   ------------------

   --  This section contains all the custom data types used to store
   --  information on nodes in different cases.

   type Formal_Param_Info is record
      Param : L.Parameter_Decl;
      --  Referenced parameter decl

      Pos   : Positive;
      --  Position of the parameter in the function profile
   end record;
   --  Store information about a formal parameter

   package Params_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Symbol_Type,
      Element_Type    => Formal_Param_Info,
      Hash            => Hash,
      Equivalent_Keys => "=");
   --  Mapping of name to formal parameter information. Used to speed up lookup
   --  of parameters in function calls.

   type Regex_Matcher_Access is access GNAT.Regpat.Pattern_Matcher;
   --  Store a compiled regular expression pattern. Used by LKQL's pattern
   --  matching routine to avoid having to recompile patterns for each query.

   type AST_Node_Kind_Access is access AST_Nodes.AST_Node_Kind'Class;
   --  Reference to a node kind descriptor, that allows querying efficiently
   --  whether a given node is of this kind or not. used by LKQL's node kind
   --  pattern matcher to avoid doing too much string manipulations to answer
   --  this query.

   ------------------------
   -- Node extension API --
   ------------------------

   --  This section contains the main types and subprograms used to interact
   --  with node extensions. At a high level, the most important entry point is
   --  the ``Get_Ext`` function.

   type Node_Ext (Kind : LCO.LKQL_Node_Kind_Type := LCO.LKQL_Expr_Arg)
   is record
      case Kind is
         when LCO.LKQL_Base_Function =>
            Params : Params_Maps.Map;
            --  Param_Map for the function, used to speedup lookup of
            --  parameters in calls.
         when LCO.LKQL_Regex_Pattern =>
            Compiled_Pattern : Regex_Matcher_Access;

         when LCO.LKQL_Node_Kind_Pattern =>
            Expected_Kind : AST_Node_Kind_Access;

         when LCO.LKQL_Base_String_Literal =>
            Denoted_Value : Text_Access;

         when others => null;
      end case;
   end record;
   --  Discriminated record containing data depending on the type of the LKQL
   --  node. This type is wrapped because we cannot have a default value
   --  discriminated record inheriting from L.Extension_Base.

   type LKQL_Node_Extension is new L.Extension_Base with record
      Content : Node_Ext;
   end record;
   --  Wrapper containing the ``Node_Ext`` instance that actually contains the
   --  info.

   type Ext is access all LKQL_Node_Extension;
   --  Access to an extension record.

   procedure Destroy (Self : in out Ext);
   --  Destroy an extension through its pointer.

   function Get_Ext
     is new L.Get_Extension
     (LKQL_Node_Extension, Ext);
   --  Main entry point. Returns the extension for a given node.

end LKQL.Node_Extensions;

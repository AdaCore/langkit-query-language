--  This package is the user facing parts of the LKQL node extension mechanism.
--  It allows us to extend nodes with pre-computed information stored in
--  ``Node_Ext`` records that are allocated and attached to LKQL nodes.

package LKQL.Node_Extensions is

   ------------------------
   -- Node extension API --
   ------------------------

   --  This section contains the main types and subprograms used to interact
   --  with node extensions. At a high level, the most important entry point is
   --  the ``Get_Ext`` function.

   type Node_Ext (Kind : LCO.LKQL_Node_Kind_Type := LCO.LKQL_Expr_Arg)
   is record
      case Kind is
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

   function Get_Ext is new L.Get_Extension
     (LKQL_Node_Extension, Ext);
   --  Main entry point. Returns the extension for a given node.

end LKQL.Node_Extensions;

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

with Langkit_Support.Text; use Langkit_Support.Text;
with Langkit_Support.Diagnostics; use Langkit_Support.Diagnostics;
with Langkit_Support.Diagnostics.Output;

with LKQL.Node_Extensions; use LKQL.Node_Extensions;

package body LKQL.Unit_Utils is

   procedure Output_Error (Node : L.LKQL_Node; Error_Msg : Text_Type);

   function Preprocess_Visitor
     (Node : L.LKQL_Node'Class) return LCO.Visit_Status;
   --  Visitor for the preprocessing pass of LKQL, where we will do some
   --  preprocessing/compilation like computations. TODO: For the moment this
   --  is in unit utils, but clearly this should have its own dedicated module
   --  eventually.

   ------------------
   -- Output_Error --
   ------------------

   procedure Output_Error (Node : L.LKQL_Node; Error_Msg : Text_Type) is
      D : constant Diagnostic := Langkit_Support.Diagnostics.Create
        (Sloc_Range => Node.Sloc_Range,
         Message    => Error_Msg);
   begin
      Output.Print_Diagnostic
        (D, Node.Unit, Simple_Name (Node.Unit.Get_Filename));
      raise Unit_Creation_Error;
   end Output_Error;

   ------------------------
   -- Preprocess_Visitor --
   ------------------------

   function Preprocess_Visitor
     (Node : L.LKQL_Node'Class) return LCO.Visit_Status
   is
   begin
      L.Init_Extension (Node);

      declare
         Ext_Val : constant Ext := Get_Ext (Node);
      begin
         case Node.Kind is

         when LCO.LKQL_Base_Function =>

            --  Base function case: Pre process function parameters, put them
            --  in a name -> info map so that we can speed up function calls.

            Ext_Val.Content :=
              Node_Ext'(Kind => LCO.LKQL_Anonymous_Function, Params => <>);
            declare
               Fun : constant L.Parameter_Decl_List
                 := Node.As_Base_Function.F_Parameters;
            begin
               for I in Fun.First_Child_Index .. Fun.Last_Child_Index loop
                  declare
                     Param : constant L.Parameter_Decl :=
                       Fun.Child (I).As_Parameter_Decl;
                  begin
                     Ext_Val.Content.Params.Include
                       (Symbol (Param.F_Param_Identifier),
                        (Fun.Child (I).As_Parameter_Decl, I));
                  end;
               end loop;
            end;

         when LCO.LKQL_Fun_Call =>

            --  Function calls: Check that positional arguments are always
            --  before named arguments.
            declare
               Has_Seen_Named : Boolean := False;
            begin
               for Arg of Node.As_Fun_Call.F_Arguments loop
                  case Arg.Kind is
                     when LCO.LKQL_Named_Arg =>
                        Has_Seen_Named := True;
                     when LCO.LKQL_Expr_Arg =>
                        if Has_Seen_Named then
                           Output_Error
                             (Arg.As_LKQL_Node,
                              "positional argument after named argument");
                        end if;
                     when others => null;
                  end case;
               end loop;
            end;

         when others => null;
         end case;
      end;

      return LCO.Into;
   end Preprocess_Visitor;

   --------------------
   -- Make_LKQL_Unit --
   --------------------

   function Make_LKQL_Unit
     (Path : String; Context : out L.Analysis_Context) return L.Analysis_Unit
   is
   begin
      Context := L.Create_Context;
      return Make_LKQL_Unit (Context, Path);
   end Make_LKQL_Unit;

   --------------------
   -- Make_LKQL_Unit --
   --------------------

   function Make_LKQL_Unit
     (Context : L.Analysis_Context; Path : String) return L.Analysis_Unit
   is

      Ret : constant L.Analysis_Unit :=
        Unit_Or_Error (Context.Get_From_File (Path));
   begin
      Ret.Root.Traverse (Preprocess_Visitor'Access);
      return Ret;
   end Make_LKQL_Unit;

   ------------------------------
   -- Make_LKQL_Unit_From_Code --
   ------------------------------

   function Make_LKQL_Unit_From_Code
     (LKQL_Code : String) return L.Analysis_Unit
   is (Make_LKQL_Unit_From_Code (L.Create_Context, LKQL_Code));

   ------------------------------
   -- Make_LKQL_Unit_From_Code --
   ------------------------------

   function Make_LKQL_Unit_From_Code (Context   : L.Analysis_Context;
                                      LKQL_Code : String;
                                      Unit_Name : String := "[inline code]")
                                      return L.Analysis_Unit
   is
      Ret : constant L.Analysis_Unit := Unit_Or_Error
        (Context.Get_From_Buffer
             (Filename => Unit_Name, Buffer => LKQL_Code));

   begin
      Ret.Root.Traverse (Preprocess_Visitor'Access);
      return Ret;
   end Make_LKQL_Unit_From_Code;

   -------------------
   -- Unit_Or_Error --
   -------------------

   function Unit_Or_Error (Unit : L.Analysis_Unit) return L.Analysis_Unit is
   begin
      if Unit.Has_Diagnostics then
         for D of Unit.Diagnostics loop
            Output.Print_Diagnostic
              (D, Unit, Simple_Name (Unit.Get_Filename));
         end loop;

         raise Unit_Creation_Error;
      end if;

      Unit.Populate_Lexical_Env;
      return Unit;
   end Unit_Or_Error;

end LKQL.Unit_Utils;

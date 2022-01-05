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

with Ada.Directories; use Ada.Directories;
with Ada.Strings.Wide_Wide_Unbounded;
with Ada.Strings.Wide_Wide_Fixed; use Ada.Strings.Wide_Wide_Fixed;

with Langkit_Support.Text; use Langkit_Support.Text;
with Langkit_Support.Diagnostics; use Langkit_Support.Diagnostics;
with Langkit_Support.Diagnostics.Output;

with LKQL.Node_Extensions; use LKQL.Node_Extensions;
with LKQL.Primitives;
with Ada_AST_Nodes;
with GNAT.Regpat;

package body LKQL.Unit_Utils is

   procedure Output_Error (Node : L.LKQL_Node; Error_Msg : Text_Type);

   function Preprocess_String (S : Text_Type) return Text_Type;
   --  Preprocess the given text, which is the content of a string literal.
   --  Today, this is solely processing escape sequences.

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

   -----------------------
   -- Preprocess_String --
   -----------------------

   function Preprocess_String (S : Text_Type) return Text_Type is
      Ret : Unbounded_Text_Type;
      Idx : Positive := S'First;

      function Decode_Hex (C : Wide_Wide_Character) return Natural
      is (case C is
          when '0' .. '9' =>
             Wide_Wide_Character'Pos (C) - Wide_Wide_Character'Pos ('0'),
          when 'a' .. 'f' =>
             10 + Wide_Wide_Character'Pos (C) - Wide_Wide_Character'Pos ('a'),
          when 'A' .. 'F' =>
             10 + Wide_Wide_Character'Pos (C) - Wide_Wide_Character'Pos ('A'),
          when others     =>
             raise Unit_Creation_Error with "Invalid escape character");
      --  Return the integer value corresponding to the given heaxdecimal
      --  character.

      use Ada.Strings.Wide_Wide_Unbounded;
   begin
      --  Process escape sequences
      loop
         exit when Idx > S'Last;
         if S (Idx) = '\' then
            case S (Idx  + 1) is

               --  Just append the escaped character
               when '\' | '"' => Append (Ret, S (Idx + 1));

               --  Newline \n: append a LF char
               when 'n' => Append (Ret, To_Text ((1 => ASCII.LF)));

               --  Hex code
               when 'x' =>
                  Idx := Idx + 2;
                  Append (Ret,
                          Wide_Wide_Character'Val
                            (Decode_Hex (S (Idx)) * 16 +
                             Decode_Hex (S (Idx + 1))));

               --  Unsupported cases
               when others =>
                  raise Unit_Creation_Error
                    with "Unsupported escape character";
            end case;

            Idx := Idx + 2;

         else
            Append (Ret, S (Idx));
            Idx := Idx + 1;
         end if;
      end loop;

      return To_Text (Ret);
   end Preprocess_String;

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
                       (Symbol (Param.F_Param_Identifier), (Param, I));
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

         when LCO.LKQL_Regex_Pattern =>

            --  Regular expressions: precompile regex patterns to speed up
            --  matching at runtime.
            declare
               use GNAT.Regpat;
               use Ada.Strings.Wide_Wide_Unbounded;

               Regex_Node : constant L.Regex_Pattern := Node.As_Regex_Pattern;

               Quoted_Pattern : constant Unbounded_Text_Type :=
                  To_Unbounded_Text (Regex_Node.Text);

               Pattern_Str : constant Unbounded_Text_Type :=
                  Unbounded_Slice
                    (Quoted_Pattern, 2, Length (Quoted_Pattern) - 1);

               Pattern_Utf8 : constant String :=
                  To_UTF8 (To_Text (Pattern_Str));
            begin
               Ext_Val.Content := Node_Ext'
                 (Kind => LCO.LKQL_Regex_Pattern,
                  Compiled_Pattern =>
                     new Pattern_Matcher'(Compile (Pattern_Utf8)));
            exception
               when Expression_Error =>
                  Output_Error
                    (Node.As_LKQL_Node,
                     "Failed to compile regular expression.");
            end;

         when LCO.LKQL_Node_Kind_Pattern =>
            declare
               Pattern : constant L.Node_Kind_Pattern :=
                  Node.As_Node_Kind_Pattern;

               Kind    : Ada_AST_Nodes.Ada_AST_Node_Kind;
               --  TODO: Here we explicitly reference Ada_AST_Nodes, because
               --  we have no runtime information at this point indicating
               --  which language LKQL is targetting. This should be solved
               --  once we have the generic introspection API in langkit,
               --  for example by having a global variable "Target_Language"
               --  of type "Langkit.Language_Descriptor" (names are fictional)
               --  from which we could easily lookup node kind names.
            begin
               Kind := Ada_AST_Nodes.Get_Kind_From_Name
                 (Pattern.F_Kind_Name.Text);
               Ext_Val.Content := Node_Ext'
                 (Kind          => LCO.LKQL_Node_Kind_Pattern,
                  Expected_Kind => new Ada_AST_Nodes.Ada_AST_Node_Kind'(Kind));
            exception
               when Primitives.Unsupported_Error =>
                  Output_Error (Node.As_LKQL_Node, "Invalid kind name");
            end;

         when LCO.LKQL_String_Literal =>
            declare
               T : Text_Type renames Node.Text;
               No_Quotes : constant Text_Type := T (T'First + 1 .. T'Last - 1);
            begin
               Ext_Val.Content :=
                 Node_Ext'(Kind => LCO.LKQL_String_Literal,
                           Denoted_Value =>
                              new Text_Type'(Preprocess_String (No_Quotes)));
            end;
         when LCO.LKQL_Block_String_Literal =>
            declare
               Ret      : Unbounded_Text_Type;
               package W renames Ada.Strings.Wide_Wide_Unbounded;
            begin
               if Node.Children_Count > 0 then
                  --  The block string will be aligned on the start of the
                  --  first non blank char of the first block literal
                  declare
                     First_Sub_Block : constant L.Sub_Block_Literal :=
                       Node.As_Block_String_Literal
                         .F_Docs.Child (1).As_Sub_Block_Literal;
                     Text            : constant Text_Type :=
                       First_Sub_Block.Text;

                     Stripped        : constant Text_Type :=
                       Text (Text'First + 2 .. Text'Last);
                     --  Strip the |" prefix

                     Non_Blank_Index : constant Positive :=
                       Index_Non_Blank (Stripped) - Text'First;
                     --  Get the first non blank index
                  begin
                     for Doc_Lit of
                       Node.As_Block_String_Literal.F_Docs.Children
                     loop
                        declare
                           T : constant Text_Type := Doc_Lit.Text;
                        begin
                           W.Append
                             (Ret,
                              To_Unbounded_Text
                                (T (T'First + Non_Blank_Index .. T'Last)));
                           W.Append (Ret, To_Text ("" & ASCII.LF));
                        end;
                     end loop;
                  end;
               end if;
               Ext_Val.Content :=
                 Node_Ext'
                   (Kind          => LCO.LKQL_Block_String_Literal,
                    Denoted_Value =>
                       new Text_Type'(Preprocess_String (To_Text (Ret))));
            end;
         when others => null;
         end case;
      end;

      return LCO.Into;
   end Preprocess_Visitor;

   ----------------------
   -- Run_Preprocessor --
   ----------------------

   procedure Run_Preprocessor (Unit : L.Analysis_Unit) is
   begin
      --  TODO: Ideally we would check here that the unit has not already
      --  been preprocessed.
      Unit.Root.Traverse (Preprocess_Visitor'Access);
   end Run_Preprocessor;

   --------------------
   -- Make_LKQL_Unit --
   --------------------

   function Make_LKQL_Unit
     (Context : L.Analysis_Context; Path : String) return L.Analysis_Unit
   is
      Ret : constant L.Analysis_Unit :=
        Unit_Or_Error (Context.Get_From_File (Path));
   begin
      Run_Preprocessor (Ret);
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
      Run_Preprocessor (Ret);
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

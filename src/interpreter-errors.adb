with String_Utils; use String_Utils;

with Langkit_Support.Slocs; use Langkit_Support.Slocs;

with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;

package body Interpreter.Errors is

   function Underline_Error (Lines    : String_Vectors.Vector;
                             Location : Source_Location_Range)
                             return Unbounded_Text_Type;
   --  Return a String representing the source code containing the error, where
   --  the location of the error has been underlined.

   ----------------------
   -- Underline_Error --
   ----------------------

   function Underline_Error (Lines    : String_Vectors.Vector;
                             Location : Source_Location_Range)
                             return Unbounded_Text_Type
   is
      Result     : Unbounded_Text_Type;
      Start_Col  : constant Integer := Integer (Location.Start_Column);
      End_Col    : constant Integer := Integer (Location.End_Column);
      Start_Line : constant Integer := Integer (Location.Start_Line);
      End_Line   : constant Integer := Integer (Location.End_Line);
   begin
      if Start_Line = End_Line then
         return Underline_Range (Lines (Start_Line), Start_Col, End_Col);
      end if;

      Append (Result, Underline_From (Lines (Start_Line), Start_Col));

      for I in Start_Line + 1 .. End_Line - 1 loop
         Append (Result, Underline (Lines (I)));
      end loop;

      if Location.Start_Line /= Location.End_Line then
         Append (Result, Underline_To (Lines (End_Line), End_Col));
      end if;

      return Result;
   end Underline_Error;

   ------------------------
   --  Error_Description --
   ------------------------

   function Error_Description (Error : Error_Data) return Unbounded_Text_Type
   is
      use Langkit_Support.Text.Chars;
      Error_Unit : constant LEL.Analysis_Unit := Error.AST_Node.Unit;
      Error_Msg  : constant Unbounded_Text_Type :=
        "Error: " & Error.Short_Message;
      Unit_Lines : constant String_Vectors.Vector :=
        Split_Lines (Error_Unit.Text);
      Underlined : constant Unbounded_Text_Type :=
        Underline_Error (Unit_Lines, Error.AST_Node.Sloc_Range);
   begin
      return LF & Error_Msg & LF & LF & Underlined;
   end Error_Description;

   --------------
   -- Is_Error --
   --------------

   function Is_Error (Err : Error_Data) return Boolean is
   begin
      return Err.Kind /= Kind_Empty_Error;
   end Is_Error;

   ----------------------
   -- Make_Empty_Error --
   ----------------------

   function Make_Empty_Error return Error_Data is
   begin
      return (Kind => Kind_Empty_Error);
   end Make_Empty_Error;

   ---------------------
   -- Make_Eval_Error --
   ---------------------

   function Make_Eval_Error (AST_Node      : LEL.LKQL_Node;
                             Short_Message : Unbounded_Text_Type)
                             return Error_Data
   is
   begin
      return (Kind => Kind_Eval_Error,
              AST_Node => AST_Node,
              Short_Message => Short_Message);
   end Make_Eval_Error;

end Interpreter.Errors;

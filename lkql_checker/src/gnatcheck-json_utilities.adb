--
--  Copyright (C) 2005-2025, AdaCore
--  SPDX-License-Identifier: GPL-3.0-or-later
--

with Ada.Strings.Unbounded;

package body Gnatcheck.JSON_Utilities is

   ------------
   -- Expect --
   ------------

   function Expect (Object : JSON_Value; Field : String) return JSON_Value is
   begin
      if not Object.Has_Field (Field) then
         raise Field_Not_Found with Field;
      end if;
      return Object.Get (Field);
   end Expect;

   function Expect (Object : JSON_Value; Field : String) return String is
      Field_Value : constant JSON_Value := Expect (Object, Field);
   begin
      if Field_Value.Kind /= JSON_String_Type then
         raise Invalid_Type
           with "'" & Field & "'' JSON value should be a string";
      end if;
      return Field_Value.Get;
   end Expect;

   --------------------
   -- Expect_Literal --
   --------------------

   function Expect_Literal (Object : JSON_Value; Field : String) return Boolean
   is
      Field_Value : constant String := Expect (Object, Field);
   begin
      return Boolean'Value (Field_Value);
   exception
      when Constraint_Error =>
         raise Invalid_Type with "'" & Field & "' value should be a boolean";
   end Expect_Literal;

   function Expect_Literal (Object : JSON_Value; Field : String) return Integer
   is
      Field_Value : constant String := Expect (Object, Field);
   begin
      return Integer'Value (Field_Value);
   exception
      when Constraint_Error =>
         raise Invalid_Type with "'" & Field & "' value should be an integer";
   end Expect_Literal;

   function Expect_Literal (Object : JSON_Value; Field : String) return String
   is
      Field_Value : constant String := Expect (Object, Field);
   begin
      if Field_Value'Length > 1
        and then Field_Value (Field_Value'First) = '"'
        and then Field_Value (Field_Value'Last) = '"'
      then
         return Remove_Quotes (Field_Value);
      end if;
      raise Invalid_Type with "'" & Field & "' value should be a string";
   end Expect_Literal;

   function Expect_Literal
     (Object : JSON_Value; Field : String) return String_Vector
   is
      use Ada.Strings.Unbounded;

      Field_Value : constant String := Expect (Object, Field);
      Res         : String_Vector;
      Acc         : Unbounded_String;
      Tuple_Level : Integer := 0;
      In_Item     : Boolean := False;
   begin
      if Field'Length > 1
        and then Field_Value (Field_Value'First) = '['
        and then Field_Value (Field_Value'Last) = ']'
      then
         for C of
           Field_Value ((Field_Value'First + 1) .. (Field_Value'Last - 1))
         loop
            case C is
               when ','    =>
                  if Tuple_Level = 0 then
                     Res.Append (Remove_Quotes (To_String (Acc)));
                     Set_Unbounded_String (Acc, "");
                     In_Item := False;
                  else
                     Append (Acc, C);
                  end if;

               when ' '    =>
                  if In_Item then
                     Append (Acc, C);
                  end if;

               when others =>
                  Append (Acc, C);
                  In_Item := True;
                  if C = '(' then
                     Tuple_Level := @ + 1;
                  elsif C = ')' then
                     Tuple_Level := @ - 1;
                  end if;
            end case;
         end loop;
         Res.Append (Remove_Quotes (To_String (Acc)));
         return Res;
      end if;
      raise Invalid_Type with "'" & Field & "' value should be a list";
   end Expect_Literal;

   ---------------------
   -- Parse_LKQL_List --
   ---------------------

   function Parse_LKQL_List (List_Literal : String) return String_Vector is
   begin
      if List_Literal'Length > 1
        and then List_Literal (List_Literal'First) = '['
        and then List_Literal (List_Literal'Last) = ']'
      then
         return
           Split
             (List_Literal
                ((List_Literal'First + 1) .. (List_Literal'Last - 1)),
              Sep        => ',',
              Trim_Elems => True);
      end if;
      raise Invalid_Type with "expecting a list literal";
   end Parse_LKQL_List;

   ------------------------
   -- Parse_String_Tuple --
   ------------------------

   function Parse_String_Tuple (Tuple_Literal : String) return String_Vector is
      Res : String_Vector;
   begin
      if Tuple_Literal'Length > 2
        and then Tuple_Literal (Tuple_Literal'First) = '('
        and then Tuple_Literal (Tuple_Literal'Last) = ')'
      then
         Res :=
           Split
             (Tuple_Literal
                ((Tuple_Literal'First + 1) .. (Tuple_Literal'Last - 1)),
              Sep        => ',',
              Trim_Elems => True);
         for C in Res.Iterate loop
            Res.Replace_Element (C, Remove_Quotes (Res (C)));
         end loop;
         return Res;
      end if;
      raise Invalid_Type with "expecting a tuple of strings";
   end Parse_String_Tuple;

end Gnatcheck.JSON_Utilities;

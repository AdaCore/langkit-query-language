--
--  Copyright (C) 2005-2024, AdaCore
--  SPDX-License-Identifier: GPL-3.0-or-later
--

--  This package offer high level abstractions for manipulating JSON data
--  during the GNATcheck process.

with Gnatcheck.String_Utilities; use Gnatcheck.String_Utilities;

with GNATCOLL.JSON; use GNATCOLL.JSON;

package Gnatcheck.JSON_Utilities is

   -----------------------
   -- Exception objects --
   -----------------------

   Field_Not_Found : exception;
   Invalid_Type : exception;

   --------------------
   -- Util functions --
   --------------------

   function Expect (Object : JSON_Value; Field : String) return JSON_Value
     with Pre => Object.Kind = JSON_Object_Type;
   --  Get the `Field` in `Object` as a JSON value.
   --  If `Object` doesn't contain the required `Field`, this function will
   --  raise a `Field_Not_Found`.

   function Expect (Object : JSON_Value; Field : String) return String;
   --  Same as other `Expect` functions, but raise an `Invalid_Type` if the
   --  value of `Field` is not JSON string.

   --------------------------
   -- LKQL literal helpers --
   --------------------------

   function Expect_Literal
     (Object : JSON_Value; Field : String) return Boolean;
   --  Call `Expect` function to get the given `Field` in `Object` as a string
   --  and ensure that it is representing a LKQL boolean literal.
   --  If the `Field` does not represents an LKQL boolean literal then raise an
   --  `Invalid_Type`.
   --  This function will unset the `Field` from the `Object` if it is present
   --  and is a boolean literal.

   function Expect_Literal
     (Object : JSON_Value; Field : String) return Integer;
   --  Same as other `Consume_Literal` functions for LKQL integer literals.

   function Expect_Literal
     (Object : JSON_Value; Field : String) return String;
   --  Same as other `Consume_Literal` functions for LKQL string literals. This
   --  function also parses regex LKQL patterns, returning the regex content
   --  surrounded by double quotes.

   function Expect_Literal
     (Object : JSON_Value; Field : String) return String_Vector;
   --  Same as other `Consume_Literal` functions for LKQL list literals. This
   --  function return a string vector containing all elements in the list.
   --  This function does not parse nested list, but handles nested tuples.

   function Parse_LKQL_List
     (List_Literal : String) return String_Vector;
   --  Parse the given string as a LKQL list literal. Return its content as
   --  a string vector. This function only handles list of non-composite types.
   --  Raise `Invalid_Type` if the given literal is not a list.

   function Parse_String_Tuple
     (Tuple_Literal : String) return String_Vector;
   --  Parse the given string as a LKQL tuple literal composed of strings.
   --  Return its content as a string vector.
   --  Raise `Invalid_Type` if the given literal is not a tuple.

end Gnatcheck.JSON_Utilities;

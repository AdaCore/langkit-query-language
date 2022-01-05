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

with Ada.Exceptions; use Ada.Exceptions;

with Langkit_Support.Text; use Langkit_Support.Text;

package LKQL.Errors is

   type Property_Error_Recovery_Kind is
     (Continue_And_Warn, Continue_And_Log, Raise_Error);
   --  Type to describe the behavior of LKQL when a property error happens
   --  inside a query.
   --
   --  * Continue_And_Warn will emit a warning/diagnostic on stderr and
   --    continue.
   --
   --  * Continue_And_Log will just log the error on a trace and continue. Use
   --  it in cases where LKQL is embedded and you don't want to emit anything
   --  on stderr.
   --
   --  * Raise_Error will bubble up the error.

   Property_Error_Recovery : Property_Error_Recovery_Kind := Continue_And_Log;

   Stop_Evaluation_Error : exception;
   --  This type of exception is used to signal that the execution should not
   --  be resumed. WARNING: THIS SHOULD NEVER BE RAISED MANUALLY but instead
   --  be raised via ``Raise_And_Record_Error``.

   type Error_Kind is
     (No_Error,
      --  Absence of error
      Eval_Error
      --  Error originating from the execution of the LKQL program
     );
   --  Denotes the kind of an error value.

   type Error_Data (Kind : Error_Kind := No_Error) is record
      case Kind is
         when No_Error =>
            null;
            --  Represents the absence of error
         when Eval_Error =>
            AST_Node     : L.LKQL_Node;
            --  Node whose evaluation triggered this error

            Short_Message : Unbounded_Text_Type;
            --  A short description of the error

            Property_Error_Info : Exception_Occurrence_Access := null;
            --  If the raised error encapsulates a property error, this will
            --  contain an access to the property error exception occurence.
            --  Else, will be null.
      end case;
   end record;
   --  Store an error value.

   function Error_Description (Error : Error_Data) return Unbounded_Text_Type;
   --  Return a detailed description of the given error.

   function Is_Error (Err : Error_Data) return Boolean;
   --  Return whether Err contains an error

   function Make_Empty_Error return Error_Data;

   function Make_Eval_Error
     (AST_Node            : L.LKQL_Node'Class;
      Short_Message       : Text_Type;
      Property_Error_Info : Exception_Occurrence_Access := null)
      return Error_Data;
   --  Create an error value of kind Eval_Error

end LKQL.Errors;

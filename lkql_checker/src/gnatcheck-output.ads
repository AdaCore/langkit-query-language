------------------------------------------------------------------------------
--                                                                          --
--                                 GNATCHECK                                --
--                                                                          --
--                     Copyright (C) 2004-2021, AdaCore                     --
--                                                                          --
-- GNATCHECK  is free software;  you can redistribute it and/or modify  it  --
-- under terms of the GNU General Public License  as published by the Free  --
-- Software Foundation;  either version 3,  or (at your option)  any later  --
-- version.   This  software  is distributed in the hope that it  will  be  --
-- useful but  WITHOUT ANY WARRANTY;  without even the implied warranty of  --
-- MERCHANTABILITY  or  FITNESS  FOR  A PARTICULAR PURPOSE.                 --
--                                                                          --
-- You should have received a copy of the GNU General Public License and a  --
-- copy of the GCC Runtime Library Exception along with this program;  see  --
-- the files COPYING3 and COPYING.RUNTIME respectively.  If not, see        --
-- <http://www.gnu.org/licenses/>.                                          --
------------------------------------------------------------------------------

--  This package contains various output routines.

with Ada.Exceptions; use Ada.Exceptions;

package Gnatcheck.Output is

   Text_Report_ON : Boolean := True;
   XML_Report_ON  : Boolean := False;
   --  Indicate if the tool should generate a global report file in text or XML
   --  format;

   Custom_Text_Report_File : Boolean := False;
   Custom_XML_Report_File  : Boolean := False;
   --  Undicate if custom name is specified for text or XML output file

   procedure Print_Version_Info (Released_At : Positive);
   --  Prints into Stderr the tool version information in the following format:
   --
   --  <toolname>
   --  Copyright <Released_At>-<current year>, AdaCore.
   --
   --  Released_At is supposed to be the year when the tool is first released.

   procedure Print_Tool_Version (Released_At : Positive);
   --  Similar to Print_Version_Info, but sends the output into Stdout and
   --  the format of the information printed is similar to what is printed
   --  for '--version' option by all the other GNAT tools.

   procedure Report_Unhandled_Exception (Ex : Exception_Occurrence);
   --  Reports an unhandled exception into Standard_Error

   procedure Error (Message : String);
   --  Sends into Stderr the error message in the form 'Tool_Name: Message'

   procedure Error_No_Tool_Name (Message : String);
   --  Sends into Stderr the error message with no tool name prefix

   procedure SLOC_Error
     (Message : String;
      SLOC    : String);
   --  Sends to Stderr the error message in the following format:
   --  'SLOC:Tool_Name:Message', where SLOC is the GNAT-style source location.

   procedure Warning (Message : String);
   --  Same as Error, but do nothing if Warning_Mode = Quiet.

   procedure Info
     (Message  : String;
      Line_Len : Natural := 0;
      Spacing  : Natural := 0);
   --  Sends Message as a separate line(s) into Stderr (with no tool name
   --  prefix). If Line_Len is set to some positive value, it is treated as a
   --  maximal length of the text to be placed into one output line, and if the
   --  length of Message exceeds Line_Len, this procedure tries to split
   --  Message treating spaces as word separators and prints the rest of the
   --  Message on the next line(s). Each continuation line starts from Spacing
   --  number of space characters. Message can be split only on borders of
   --  words.

   procedure Info_No_EOL
     (Message  : String;
      Line_Len : Natural := 0;
      Spacing  : Natural := 0);
   --  The same as Info, but does not "close" the last line being printed out,
   --  that is, the last line does not contain a (platform-specific) EOL
   --  character(s).

   Indent_String : constant String := "   ";
   --  Used as indentation element in various output

   ----------------------
   -- Tool report file --
   ----------------------

   procedure Set_Report_File_Name (Fname : String);
   procedure Set_XML_Report_File_Name (Fname : String);
   --  Sets the name of the tool (XML) report file. If this procedure has not
   --  been called before creating the output file, the default name of the
   --  form 'tool_name.out' ('tool_name.xml') is used.

   function Get_Report_File_Name return String;
   function Get_XML_Report_File_Name return String;
   --  Returns the full normalized name of the tool (XML) report file in
   --  absolute form. Return empty string if the tool report file name is
   --  not set.

   function Get_Number return String;
   --  Gets numeric index from the report file name.

   procedure Set_Report_Files;
   --  Creates and/or opens the tool report files, according to the values of
   --  Text_Report_ON and XML_Report_ON. If the file with the same name as the
   --  name of the report file already exists, it is silently and
   --  unconditionally overridden. In Mimic_gcc mode, the file should have
   --  already been created by the outer invocation, and we don't overwrite it,
   --  we APPEND to it. We also lock the file in this mode in case there are
   --  parallel inner invocations.
   --  Note, that this procedure is supposed to be called *before* the tool
   --  creates the temporary directory and gets into it.

   procedure Close_Report_Files;
   --  Closes the report files (according to the values of Text_Report_ON and
   --  XML_Report_ON). In Mimic_gcc mode, also unlock them.

   --  The following routines should be called only after the call to
   --  Set_Report_File. They should not be called after the call to
   --  Close_Report_File

   procedure Report
     (Message      : String;
      Indent_Level : Natural := 0);
   procedure XML_Report
     (Message      : String;
      Indent_Level : Natural := 0);
   --  Sends the Message into the tool (XML) report file. The line is then
   --  closed (by appending the EOL character(s)). If Indent_Level is not zero,
   --  Message is prepended by Indent_Level indentation string (currently the
   --  indentation string consists of three space characters).

   procedure Report_No_EOL
     (Message      : String;
      Indent_Level : Natural := 0);
   procedure XML_Report_No_EOL
     (Message      : String;
      Indent_Level : Natural := 0);
   --  Similar to the previous routine, but it does not close the output line.

   procedure Report_EOL;
   procedure XML_Report_EOL;
   --  Closes the line  in the (XML) output file.

   function Get_Indent_String return String;
   --  Returns the strung constant consisting on space characters and used to
   --  indicate one level of indentation

   -------------------
   -- Tool log file --
   -------------------

   --  The log file is the file to copy all the information sent into Stderr.

   procedure Set_Log_File_Name (Fname : String);
   --  Sets the name of the tool log file. If this procedure has not been
   --  called before creating the log file, the default name of the form
   --  'tool_name.log' is used.

   procedure Set_Log_File;
   --  Creates and/or opens the tool log file. If the file with the same name
   --  as the name of the log file already exists, it is silently and
   --  unconditionally overridden.
   --  Note, that this procedure is supposed to be called *before* the tool
   --  creates the temporary directory and gets into it.
   --
   --  At the moment there is no possibility to specify the name for the log
   --  file, the name used for it is always tool_name.log
   --
   --  As soon as the procedure is called, all the messages that are sent by
   --  the routines defined in this package into Stderr, are copied into the
   --  log file.

   procedure Close_Log_File;
   --  Closes the report file (and stops copying the messages into it)

   procedure Brief_Help;
   --  Prints the brief gnatcheck help info into Stderr

   procedure Print_Gnatcheck_Usage;
   --  Similar to Brief_Help, but corresponds to the general format generated
   --  by other GNAT tools for '--help' option, and sends the output into
   --  Stdout

end Gnatcheck.Output;

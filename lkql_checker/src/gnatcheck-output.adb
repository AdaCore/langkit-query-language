------------------------------------------------------------------------------
--                                                                          --
--                                 GNATCHECK                                --
--                                                                          --
--                     Copyright (C) 2004-2023, AdaCore                     --
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

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Finalization;
with Ada.Strings;             use Ada.Strings;
with Ada.Strings.Fixed;       use Ada.Strings.Fixed;
with Ada.Text_IO;             use Ada.Text_IO;

with GNAT.Directory_Operations;   use GNAT.Directory_Operations;
with GNAT.OS_Lib;                 use GNAT.OS_Lib;

with Gnatcheck.Options;          use Gnatcheck.Options;
with Gnatcheck.String_Utilities; use Gnatcheck.String_Utilities;

package body Gnatcheck.Output is

   Report_File_Name     : String_Access;
   XML_Report_File_Name : String_Access;
   Log_File_Name        : String_Access;
   --  Variables that set the properties of the tool report and log files

   XML_Report_File : File_Type;
   Report_File     : File_Type;
   Log_File        : File_Type;

   procedure Set_Report_File;
   procedure Set_XML_Report_File;
   --  Creates and/or opens the tool text/XML report file

   procedure Close_Report_File;
   procedure Close_XML_Report_File;
   --  Closes text/XML report file.

   --------------------
   -- Close_Log_File --
   --------------------

   procedure Close_Log_File is
   begin
      if Log_Mode then
         Close (Log_File);
         Log_Mode := False;
         Free (Log_File_Name);
      end if;
   end Close_Log_File;

   -----------------------
   -- Close_Report_File --
   -----------------------

   procedure Close_Report_File is
   begin
      --  This can be called on unhandled exceptions when we don't know the
      --  state of the Report_File, so we take care not to blow up.

      if Is_Open (Report_File) then
         Close (Report_File);
      end if;
   end Close_Report_File;

   ---------------------------
   -- Close_XML_Report_File --
   ---------------------------

   procedure Close_XML_Report_File is
   begin
      --  This can be called on unhandled exceptions when we don't know the
      --  state of the Report_File, so we take care not to blow up.

      if Is_Open (XML_Report_File) then
         Close (XML_Report_File);
      end if;
   end Close_XML_Report_File;

   ------------------------
   -- Close_Report_Files --
   ------------------------

   procedure Close_Report_Files is
   begin
      pragma Assert (Text_Report_ON or else XML_Report_ON);

      if Text_Report_ON then
         Close_Report_File;
      end if;

      if XML_Report_ON then
         Close_XML_Report_File;
      end if;
   end Close_Report_Files;

   -----------
   -- Error --
   -----------

   procedure Error (Message : String) is
   begin
      Put (Standard_Error, Executable & ": ");

      if Log_Mode and then Is_Open (Log_File) then
         Put (Log_File, Executable & ": ");
      end if;

      Error_No_Tool_Name (Message);
   end Error;

   ------------------------
   -- Error_No_Tool_Name --
   ------------------------

   procedure Error_No_Tool_Name (Message : String) is
   begin
      Put_Line (Standard_Error, Message);

      if Log_Mode and then Is_Open (Log_File) then
         Put_Line (Log_File, Message);
      end if;
   end Error_No_Tool_Name;

   -----------------------
   -- Get_Indent_String --
   -----------------------

   function Get_Indent_String return String is
   begin
      return Indent_String;
   end Get_Indent_String;

   ----------------
   -- Get_Number --
   ----------------

   function Get_Number return String is
      Report_File_Name : constant String := (if Text_Report_ON then
                                                Get_Report_File_Name
                                             else
                                                Get_XML_Report_File_Name);

      Idx_1, Idx_2 : Natural;
   begin
      if not Aggregated_Project then
         return "";
      end if;

      Idx_2 := Index (Report_File_Name, ".", Backward);

      if Idx_2 = 0 then
         Idx_2 := Report_File_Name'Last;
      else
         Idx_2 := Idx_2 - 1;
      end if;

      Idx_1 := Index (Report_File_Name (Report_File_Name'First .. Idx_2),
                      "_",
                      Backward);

      pragma Assert (Idx_1 > 0);
      pragma Assert (Idx_1 < Idx_2);

      return Report_File_Name (Idx_1 .. Idx_2);
   end Get_Number;

   --------------------------
   -- Get_Report_File_Name --
   --------------------------

   function Get_Report_File_Name return String is
   begin
      if Report_File_Name = null then
         return "";
      end if;

      pragma Assert
        (Report_File_Name.all = Normalize_Pathname (Report_File_Name.all));

      return Report_File_Name.all;
   end Get_Report_File_Name;

   --------------------------
   -- Get_XML_Report_File_Name --
   --------------------------

   function Get_XML_Report_File_Name return String is
   begin
      if XML_Report_File_Name = null then
         return "";
      end if;

      pragma Assert
        (XML_Report_File_Name.all =
           Normalize_Pathname (XML_Report_File_Name.all));
      return XML_Report_File_Name.all;
   end Get_XML_Report_File_Name;

   ----------
   -- Info --
   ----------

   procedure Info
     (Message  : String;
      Line_Len : Natural := 0;
      Spacing  : Natural := 0) is
   begin
      Info_No_EOL (Message, Line_Len, Spacing);
      New_Line (Current_Error);

      if Log_Mode and then Is_Open (Log_File) then
         New_Line (Log_File);
      end if;
   end Info;

   -----------------
   -- Info_No_EOL --
   -----------------

   procedure Info_No_EOL
     (Message  : String;
      Line_Len : Natural := 0;
      Spacing  : Natural := 0)
   is
      Start_Idx   : constant Natural := Message'First;
      End_Idx     : Natural := Message'Last;
      Start_From  : Positive;

   begin
      if Line_Len = 0
        or else
         End_Idx - Start_Idx + 1 <= Line_Len
      then
         Put (Current_Error, Message);

         if Log_Mode and then Is_Open (Log_File) then
            Put (Log_File, Message);
         end if;

      else
         --  Define which part of the Message can be placed into one line:
         while End_Idx >= Start_Idx
             and then
               not (Message (End_Idx) = ' '
                  and then
                    End_Idx - Start_Idx + 1 <= Line_Len)
         loop
            End_Idx := End_Idx - 1;
         end loop;

         if End_Idx < Start_Idx then
            --  Cannot split Message, so:
            Put (Current_Error, Message);

            if Log_Mode and then Is_Open (Log_File) then
               Put (Log_File, Message);
            end if;

         else
            --  Index of the beginning of the remaining part of Message
            Start_From := End_Idx + 1;

            --  Now move End_Idx to the left to skip spaces:

            while End_Idx >= Start_Idx
                 and then
                  Message (End_Idx) = ' '
            loop
               End_Idx := End_Idx - 1;
            end loop;

            Put (Current_Error, Message (Start_Idx .. End_Idx));

            if Log_Mode and then Is_Open (Log_File) then
               Put (Log_File, Message (Start_Idx .. End_Idx));
            end if;

            --  Skip spaces in the remaining part of the message, if any:
            End_Idx := Message'Last;

            while Start_From <= End_Idx
                 and then
                  Message (Start_From) = ' '
            loop
               Start_From := Start_From + 1;
            end loop;

            if Start_From <= End_Idx then
               New_Line (Current_Error);

               if Log_Mode and then Is_Open (Log_File) then
                  New_Line (Log_File);
               end if;

               Info_No_EOL
                 (Message  => Spacing * ' ' & Message (Start_From .. End_Idx),
                  Line_Len => Line_Len,
                  Spacing  => Spacing);
            end if;
         end if;
      end if;
   end Info_No_EOL;

   ------------------------
   -- Print_Tool_Version --
   ------------------------

   procedure Print_Tool_Version (Released_At : Positive) is
   begin
      Put ("GNATCHECK ");
      Put (Version_String);
      New_Line;

      Put ("Copyright (C) ");
      Put (Image (Released_At));
      Put ('-');
      Put (Current_Year);
      Put (", AdaCore.");
      New_Line;
   end Print_Tool_Version;

   ------------------------
   -- Print_Version_Info --
   ------------------------

   procedure Print_Version_Info (Released_At : Positive) is
   begin
      Info (Executable & " " & Version_String);
      Info_No_EOL ("Copyright ");
      Info_No_EOL (Image (Released_At));
      Info_No_EOL ("-");
      Info_No_EOL (Current_Year);
      Info        (", AdaCore.");
   end Print_Version_Info;

   ------------
   -- Report --
   ------------

   procedure Report
     (Message      : String;
      Indent_Level : Natural := 0) is
   begin
      Report_No_EOL (Message, Indent_Level);
      Report_EOL;
   end Report;

   ----------------
   -- XML_Report --
   ----------------

   procedure XML_Report
     (Message      : String;
      Indent_Level : Natural := 0) is
   begin
      XML_Report_No_EOL (Message, Indent_Level);
      XML_Report_EOL;
   end XML_Report;

   ----------------
   -- Report_EOL --
   ----------------

   procedure Report_EOL is
   begin
      New_Line (Report_File);
   end Report_EOL;

   --------------------
   -- XML_Report_EOL --
   --------------------

   procedure XML_Report_EOL is
   begin
      New_Line (XML_Report_File);
   end XML_Report_EOL;

   -------------------
   -- Report_No_EOL --
   -------------------

   procedure Report_No_EOL
     (Message      : String;
      Indent_Level : Natural := 0) is
   begin
      for J in 1 .. Indent_Level loop
         Put (Report_File, Indent_String);
      end loop;

      Put (Report_File, Message);
   end Report_No_EOL;

   -----------------------
   -- XML_Report_No_EOL --
   -----------------------

   procedure XML_Report_No_EOL
     (Message      : String;
      Indent_Level : Natural := 0) is
   begin
      for J in 1 .. Indent_Level loop
         Put (XML_Report_File, Indent_String);
      end loop;

      Put (XML_Report_File, Message);
   end XML_Report_No_EOL;

   --------------------------------
   -- Report_Unhandled_Exception --
   --------------------------------

   procedure Report_Unhandled_Exception (Ex : Exception_Occurrence) is
   begin
      Error (Exception_Information (Ex));
   end Report_Unhandled_Exception;

   -------------------------
   -- Report_Missing_File --
   -------------------------

   procedure Report_Missing_File (From_File, Missing_File : String) is
      function Format_Filename (F : String) return String is
        (if Full_Source_Locations then F else Base_Name (F));
      --  Formats filename
   begin
      Warning
        (Format_Filename (From_File)
         & ": cannot find "
         & Format_Filename (Missing_File));

      Missing_File_Detected := True;
   end Report_Missing_File;

   ------------------
   -- Set_Log_File --
   ------------------

   procedure Set_Log_File is
   begin
      if Log_Mode then
         if Log_File_Name = null then
            Log_File_Name :=
              new String'(Global_Report_Dir.all & Executable & ".log");
         end if;

         if Is_Regular_File (Log_File_Name.all) then
            Open (Log_File, Out_File, Log_File_Name.all);
         else
            Create (Log_File, Out_File, Log_File_Name.all);
         end if;
      end if;
   end Set_Log_File;

   -----------------------
   -- Set_Log_File_Name --
   -----------------------

   procedure Set_Log_File_Name (Fname : String) is
   begin
      Free (Log_File_Name);

      if Fname /= "" then
         Log_File_Name := new String'(Fname);
      end if;
   end Set_Log_File_Name;

   ---------------------
   -- Set_Report_File --
   ---------------------

   procedure Set_Report_File is
      Mode    : constant File_Mode := Out_File;
      Ignored : Boolean;
   begin
      if not Aggregated_Project then
         if Report_File_Name /= null
           and then
            Is_Absolute_Path (Report_File_Name.all)
         then
            Report_File_Name :=
              new String'(Normalize_Pathname (Report_File_Name.all));
         else
            Report_File_Name := new String'(Normalize_Pathname
              (Global_Report_Dir.all &
                 (if Report_File_Name = null then Executable & ".out"
                  else Report_File_Name.all)));
         end if;

         --  And in case of Aggregated_Project we already have in
         --  Report_File_Name the needed name with full path in absolute
         --  form
      end if;

      if Is_Regular_File (Report_File_Name.all) then
         Open (Report_File, Mode, Report_File_Name.all);
      else
         Create (Report_File, Out_File, Report_File_Name.all);
      end if;

   exception
      when Status_Error =>
         Error ("can not open the report file, the file may be in use");
         raise Fatal_Error;
      when Fatal_Error =>
         null;
      when others =>
         Error ("can not open the report file: " & Report_File_Name.all);
         raise Fatal_Error;
   end Set_Report_File;

   -------------------------
   -- Set_XML_Report_File --
   -------------------------

   procedure Set_XML_Report_File is
      Mode    : constant File_Mode := Out_File;
      Ignored : Boolean;
   begin
      if not Aggregated_Project then
         if XML_Report_File_Name /= null
           and then Is_Absolute_Path (XML_Report_File_Name.all)
         then
            XML_Report_File_Name :=
              new String'(Normalize_Pathname (XML_Report_File_Name.all));
         else
            XML_Report_File_Name := new String'(Normalize_Pathname
              (Global_Report_Dir.all &
                 (if XML_Report_File_Name = null then Executable & ".xml"
                  else XML_Report_File_Name.all)));
         end if;

         --  And in case of Aggregated_Project we already have in
         --  Report_File_Name the needed name with full path in absolute
         --  form
      end if;

      if Is_Regular_File (XML_Report_File_Name.all) then
         Open (XML_Report_File, Mode, XML_Report_File_Name.all);
      else
         Create (XML_Report_File, Out_File, XML_Report_File_Name.all);
      end if;

   exception
      when Status_Error =>
         Error ("can not open the report file, the file may be in use");
         raise Fatal_Error;
      when Fatal_Error =>
         null;
      when others =>
         Error ("can not open the report file: " & XML_Report_File_Name.all);
         raise Fatal_Error;
   end Set_XML_Report_File;

   ----------------------
   -- Set_Report_Files --
   ----------------------

   procedure Set_Report_Files is
   begin
      pragma Assert (Text_Report_ON or else XML_Report_ON);

      if Text_Report_ON then
         Set_Report_File;
      end if;

      if XML_Report_ON then
         Set_XML_Report_File;
      end if;

   end Set_Report_Files;

   --------------------------
   -- Set_Report_File_Name --
   --------------------------

   procedure Set_Report_File_Name (Fname : String) is
   begin
      Free (Report_File_Name);

      if Fname /= "" then
         Report_File_Name := new String'(Fname);
      end if;
   end Set_Report_File_Name;

   --------------------------
   -- Set_XML_Report_File_Name --
   --------------------------

   procedure Set_XML_Report_File_Name (Fname : String) is
   begin
      Free (XML_Report_File_Name);

      if Fname /= "" then
         XML_Report_File_Name := new String'(Fname);
      end if;
   end Set_XML_Report_File_Name;

   ----------------
   -- SLOC_Error --
   ----------------

   procedure SLOC_Error
     (Message : String;
      SLOC    : String) is
   begin
      Put (Standard_Error, SLOC & ": ");
      if Log_Mode and then Is_Open (Log_File) then
         Put (Log_File, SLOC & ": ");
      end if;

      Put (Standard_Error, Executable & ": ");

      if Log_Mode and then Is_Open (Log_File) then
         Put (Log_File, Executable & ": ");
      end if;

      Error_No_Tool_Name (Message);
   end SLOC_Error;

   -------------
   -- Warning --
   -------------

   procedure Warning (Message : String) is
   begin
      if Warning_Mode /= Quiet then
         Error (Message);
      end if;
   end Warning;

   ----------------

   --  We create a dummy object whose finalization calls Close_Report_File, so
   --  we don't leave stale lock files around even in case of unhandled
   --  exceptions.

   use Ada.Finalization;

   type Dummy_Type is new Limited_Controlled with null record;
   procedure Finalize (Ignore : in out Dummy_Type);
   procedure Finalize (Ignore : in out Dummy_Type) is
   begin
      Close_Report_File;
   end Finalize;

   Dummy : Dummy_Type;

   ----------------
   -- Brief_Help --
   ----------------

   procedure Brief_Help is
   begin
      pragma Style_Checks ("M200"); -- Allow long lines

      if Gnatkp_Mode then
         Info ("gnatkp: the GNAT known problem detector");
         Info ("usage: gnatkp -Pproject [options] [-rules [-from=file] {+Rkp_id[:param]}]");
         Info ("options:");
         Info (" --version - Display version and exit");
         Info (" --help    - Display usage and exit");
         Info ("");
         Info (" -Pproject        - Use project file project. Only one such switch can be used");
         Info (" -U               - check all sources of the argument project");
         Info (" -U main          - check the closure of units rooted at unit main");
         Info (" --no-subprojects - process only sources of root project");
         Info (" -Xname=value     - specify an external reference for argument project file");
         Info (" --subdirs=dir    - specify subdirectory to place the result files into");
         Info (" -eL              - follow all symbolic links when processing project files");
         Info (" -o filename      - specify the name of the report file");
         Info ("");
         Info (" --target=targetname - specify a target for cross platforms");
         Info (" --RTS=<runtime>     - use runtime <runtime>");
         Info ("");
         Info (" -h   - print out the list of the available kp detectors");
         Info (" -jn  - n is the maximal number of processes");
         Info (" -q   - quiet mode (do not report detections in Stderr)");
         Info (" -v   - verbose mode");
         Info (" -l   - full pathname for file locations");
         Info ("");
         Info (" --brief                - brief mode, only report detections in Stderr");
         Info (" --check-semantic       - check semantic validity of the source files");
         Info (" --charset=<charset>    - specify the charset of the source files");
         Info (" --kp-version=<version> - enable all KP detectors matching GNAT <version>");
         Info ("");

         Info (" -from=filename    - read kp options from filename");
         Info (" +R<kp_id>[:param] - turn ON a given detector [with given parameter]");
         Info ("   where <kp_id>   - ID of one of the currently implemented");
         Info ("                     detectors, use '-h' for the full list");
         Info ("");
         Info ("KP detectors must be specified either implicitly via --kp-version ");
         Info ("(and optionally --target), or explicitly via -rules");
         return;
      end if;

      Info ("gnatcheck: the GNAT rule checking tool");
      Info ("usage: gnatcheck [options] {filename} {-files=filename} -rules rule_switches [-cargs gcc_switches]");
      Info ("options:");
      Info (" --version - Display version and exit");
      Info (" --help    - Display usage and exit");
      Info ("");
      Info (" -Pproject        - Use project file project. Only one such switch can be used");
      Info (" -U               - check all sources of the argument project");
      Info (" -U main          - check the closure of units rooted at unit main");
      Info (" -Xname=value     - specify an external reference for argument project file");
      Info (" --subdirs=dir    - specify subdirectory to place the result files into");
      Info (" --simple-project - simple project set up");
      Info (" --no_objects_dir - place results into current dir instead of project dir");
      Info (" -eL              - follow all symbolic links when processing project files");
      Info ("");
      Info (" --ignore-project-switches - ignore switches specified in the project file");
      Info (" --target=targetname       - specify a target for cross platforms");
      Info (" --RTS=<runtime>           - use runtime <runtime>");
      Info (" --config=<cgpr>           - use configuration project <cgpr>");
      Info ("");
      Info (" -h   - print out the list of the currently implemented rules");
      Info (" -mn  - n is the maximal number of diagnoses in Stderr");

      Info ("        (n in 0 .. 1000, 0 means no limit)");
      Info (" -jn  - n is the maximal number of processes");
      Info (" -q   - quiet mode (do not report detections in Stderr)");
      Info (" -t   - report execution time in Stderr");
      Info (" -v   - verbose mode");
      Info (" -l   - full pathname for file locations");
      Info (" -log - duplicate all the messages sent to Stderr in gnatcheck.log");
      Info (" -s   - short form of the report file");
      Info (" -xml - generate report in XML format");
      Info (" -nt  - do not generate text report (enforces '-xml')");
      Info ("");

      Info (" --show-rule - append rule names to diagnoses generated");
      Info ("");

      Info (" --brief              - brief mode, only report detections in Stderr");
      Info (" --check-redefinition - issue warning if a rule parameter is redefined");
      Info (" --check-semantic     - check semantic validity of the source files");
      Info (" --charset=<charset>  - specify the charset of the source files");

      if not Legacy then
         Info (" --rules-dir=<dir>    - specify an alternate directory containing rule files");
      end if;

      Info ("");
      Info (" --include-file=filename - add the content of filename into generated report");

      Info ("");

      Info (" -o filename   - specify the name of the text report file");
      Info (" -ox filename  - specify the name of the XML report file (enforces '-xml')");
      Info ("");

      Info ("filename                 - the name of the Ada source file to be analyzed.");
      Info ("                           Wildcards are allowed");
      Info ("-files=filename          - the name of the text file containing a list of Ada");
      Info ("                           source files to analyze");
      Info ("--ignore=filename        - do not process sources listed in filename");
      Info ("");

      Info ("rule_switches          - a list of the following switches");
      Info ("   -from=filename      - read rule options from filename");
      Info ("   -from-lkql=filename - read rule options from the given LKQL file");
      Info ("   +R<rule_id>[:param] - turn ON a given rule [with given parameter]");
      Info ("   -R<rule_id>         - turn OFF a given rule");
      Info ("   -R<rule_id>:param   - turn OFF some of the checks for a given  rule,");
      Info ("                         depending on the specified parameter");
      Info ("where <rule_id> - ID of one of the currently implemented");
      Info ("                  rules, use '-h' for the full list");
      Info ("      param     - string representing parameter(s) of a given rule, more than ");
      Info ("                  one parameter can be set separated by ','");

      pragma Style_Checks ("M79");
   end Brief_Help;

   ---------------------------
   -- Print_Gnatcheck_Usage --
   ---------------------------

   procedure Print_Gnatcheck_Usage is
   begin
      Set_Error (Standard_Output);
      Brief_Help;
      New_Line;
      Put_Line ("Report bugs to report@adacore.com");
   end Print_Gnatcheck_Usage;

end Gnatcheck.Output;

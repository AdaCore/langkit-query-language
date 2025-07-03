--
--  Copyright (C) 2005-2025, AdaCore
--  SPDX-License-Identifier: GPL-3.0-or-later
--

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Finalization;
with Ada.Strings;             use Ada.Strings;
with Ada.Strings.Fixed;       use Ada.Strings.Fixed;
with Ada.Text_IO;             use Ada.Text_IO;

with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib;               use GNAT.OS_Lib;
with GNAT.Traceback.Symbolic;

with Gnatcheck.Options;          use Gnatcheck.Options;
with Gnatcheck.String_Utilities; use Gnatcheck.String_Utilities;

with Interfaces.C_Streams; use Interfaces.C_Streams;

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

   ------------------
   -- Emit_Message --
   ------------------

   procedure Emit_Message
     (Message     : String;
      Tag         : Message_Tags := None;
      Tool_Name   : Boolean := False;
      Location    : String := "";
      New_Line    : Boolean := False;
      Log_Message : Boolean := False)
   is
      Final_Message : constant String :=
        (if Tool_Name then Executable & ": " else "")
        & (if Location /= "" then Location & ": " else "")
        & (case Tag is
             when Info => "info: ",
             when Warning => "warning: ",
             when Error => "error: ",
             when None => "")
        & Message;
   begin
      --  Display the message in the standard error
      Put (Standard_Error, Final_Message);
      if New_Line then
         Ada.Text_IO.New_Line (Standard_Error);
      end if;

      --  If required, log the message
      if Log_Message and then Log_Mode and then Is_Open (Log_File) then
         Put (Log_File, Final_Message);
         if New_Line then
            Ada.Text_IO.New_Line (Log_File);
         end if;
      end if;
   end Emit_Message;

   -----------
   -- Error --
   -----------

   procedure Error (Message : String; Location : String := "") is
   begin
      Emit_Message
        (Message,
         Tag         => Error,
         Tool_Name   => Location = "",
         Location    => Location,
         New_Line    => True,
         Log_Message => True);
   end Error;

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
      Report_File_Name : constant String :=
        (if Text_Report_ON
         then Get_Report_File_Name
         else Get_XML_Report_File_Name);

      Idx_1, Idx_2 : Natural;
   begin
      if not Arg.Aggregated_Project then
         return "";
      end if;

      Idx_2 := Index (Report_File_Name, ".", Backward);

      if Idx_2 = 0 then
         Idx_2 := Report_File_Name'Last;
      else
         Idx_2 := Idx_2 - 1;
      end if;

      Idx_1 :=
        Index
          (Report_File_Name (Report_File_Name'First .. Idx_2), "_", Backward);

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

      pragma
        Assert
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

      pragma
        Assert
          (XML_Report_File_Name.all
             = Normalize_Pathname (XML_Report_File_Name.all));
      return XML_Report_File_Name.all;
   end Get_XML_Report_File_Name;

   ----------
   -- Info --
   ----------

   procedure Info (Message : String) is
   begin
      Emit_Message
        (Message,
         Tag         => Info,
         Tool_Name   => True,
         New_Line    => True,
         Log_Message => True);
   end Info;

   -----------------
   -- Info_No_EOL --
   -----------------

   -----------------
   -- Info_In_Tty --
   -----------------

   procedure Info_In_Tty (Message : String) is
   begin
      if isatty (fileno (stderr)) /= 0 then
         Emit_Message
           (Message,
            Tag         => Info,
            Tool_Name   => True,
            New_Line    => True,
            Log_Message => False);
      end if;
   end Info_In_Tty;

   -----------
   -- Print --
   -----------

   procedure Print (Message : String; New_Line, Log_Message : Boolean := True)
   is
   begin
      Emit_Message (Message, New_Line => New_Line, Log_Message => Log_Message);
   end Print;

   ------------------------
   -- Print_Tool_Version --
   ------------------------

   procedure Print_Tool_Version (Released_At : Positive) is
   begin
      if Gnatkp_Mode then
         Put_Line ("GNATKP " & Date);
      else
         Put_Line ("GNATCHECK " & Version_String);
      end if;

      Put_Line
        ("Copyright (C) "
         & Image (Released_At)
         & '-'
         & Current_Year
         & ", AdaCore.");
   end Print_Tool_Version;

   ------------------------
   -- Print_Version_Info --
   ------------------------

   procedure Print_Version_Info (Released_At : Positive) is
   begin
      Print (Executable & " " & Version_String, Log_Message => False);
      Print
        ("Copyright "
         & Image (Released_At)
         & '-'
         & Current_Year
         & ", AdaCore.",
         Log_Message => False);
   end Print_Version_Info;

   ------------
   -- Report --
   ------------

   procedure Report (Message : String; Indent_Level : Natural := 0) is
   begin
      Report_No_EOL (Message, Indent_Level);
      Report_EOL;
   end Report;

   ----------------
   -- XML_Report --
   ----------------

   procedure XML_Report (Message : String; Indent_Level : Natural := 0) is
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

   procedure Report_No_EOL (Message : String; Indent_Level : Natural := 0) is
   begin
      for J in 1 .. Indent_Level loop
         Put (Report_File, Indent_String);
      end loop;

      Put (Report_File, Message);
   end Report_No_EOL;

   -----------------------
   -- XML_Report_No_EOL --
   -----------------------

   procedure XML_Report_No_EOL (Message : String; Indent_Level : Natural := 0)
   is
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
      Error (Exception_Message (Ex));
      if Arg.Debug_Mode.Get then
         Print (GNAT.Traceback.Symbolic.Symbolic_Traceback_No_Hex (Ex));
      end if;
   end Report_Unhandled_Exception;

   -------------------------
   -- Report_Missing_File --
   -------------------------

   procedure Report_Missing_File (From_File, Missing_File : String) is
      function Format_Filename (F : String) return String
      is (if Arg.Full_Source_Locations.Get then F else Base_Name (F));
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
      if not Arg.Aggregated_Project then
         if Report_File_Name /= null
           and then Is_Absolute_Path (Report_File_Name.all)
         then
            Report_File_Name :=
              new String'(Normalize_Pathname (Report_File_Name.all));
         else
            Report_File_Name :=
              new String'
                (Normalize_Pathname
                   (Global_Report_Dir.all
                    & (if Report_File_Name = null
                       then Executable & ".out"
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
      if not Arg.Aggregated_Project then
         if XML_Report_File_Name /= null
           and then Is_Absolute_Path (XML_Report_File_Name.all)
         then
            XML_Report_File_Name :=
              new String'(Normalize_Pathname (XML_Report_File_Name.all));
         else
            XML_Report_File_Name :=
              new String'
                (Normalize_Pathname
                   (Global_Report_Dir.all
                    & (if XML_Report_File_Name = null
                       then Executable & ".xml"
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

   -------------
   -- Warning --
   -------------

   procedure Warning (Message : String; Location : String := "") is
   begin
      if Arg.Warnings_As_Errors.Get then
         Error (Message, Location);
         Error_From_Warning := True;
      else
         Emit_Message
           (Message,
            Tag         => Warning,
            Tool_Name   => Location = "",
            Location    => Location,
            New_Line    => True,
            Log_Message => True);
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

   --  TODO: Transition this help message to Opt_Parse's one
   procedure Brief_Help is
   begin
      pragma Style_Checks ("M200"); -- Allow long lines

      if Gnatkp_Mode then
         Put_Line ("gnatkp: the GNAT known problem detector");
         Put_Line
           ("usage: gnatkp -Pproject [options] [-rules [-from=file] {+Rkp_id[:param]}]");
         Put_Line ("options:");
         Put_Line (" --version - Display version and exit");
         Put_Line (" --help    - Display usage and exit");
         Put_Line ("");
         Put_Line
           (" -Pproject        - Use project file project. Only one such switch can be used");
         Put_Line
           (" -U               - check all sources of the argument project");
         Put_Line
           (" -U main          - check the closure of units rooted at unit main");
         Put_Line (" --no-subprojects - process only sources of root project");
         Put_Line
           (" -Xname=value     - specify an external reference for argument project file");
         Put_Line
           (" --subdirs=dir    - specify subdirectory to place the result files into");
         Put_Line
           (" -eL              - follow all symbolic links when processing project files");
         Put_Line (" -o filename      - specify the name of the report file");
         Put_Line ("");
         Put_Line
           (" --target=targetname - specify a target for cross platforms");
         Put_Line (" --RTS=<runtime>     - use runtime <runtime>");
         Put_Line ("");
         Put_Line
           (" -h                       - print out the list of the available kp detectors");
         Put_Line
           (" -jn                      - n is the maximal number of processes");
         Put_Line
           (" -q                       - quiet mode (do not report detections in Stderr)");
         Put_Line (" -v                       - verbose mode");
         Put_Line
           (" -W, --warnings-as-errors - treat warning messages as errors");
         Put_Line
           (" -l                       - full pathname for file locations");
         Put_Line ("");
         Put_Line
           (" --brief                - brief mode, only report detections in Stderr");
         Put_Line
           (" --check-semantic       - check semantic validity of the source files");
         Put_Line
           (" --charset=<charset>    - specify the charset of the source files");
         Put_Line
           (" --kp-version=<version> - enable all KP detectors matching GNAT <version>");
         Put_Line
           (" --rule-file=filename   - read kp configuration from the given LKQL file");
         Put_Line
           (" -r, --rule [kp_id]     - enable the given kp detector during the GNATKP run (this option is cumulative)");
         Put_Line ("");
         Put_Line (" -from=filename    - read kp options from filename");
         Put_Line
           (" +R<kp_id>[:param] - turn ON a given detector [with given parameter]");
         Put_Line
           ("   where <kp_id>   - ID of one of the currently implemented");
         Put_Line
           ("                     detectors, use '-h' for the full list");
         Put_Line ("");
         Put_Line
           ("KP detectors must be specified either implicitly via --kp-version ");
         Put_Line ("(and optionally --target), or explicitly via -rules");
         return;
      end if;

      Put_Line ("gnatcheck: the GNAT rule checking tool");
      Put_Line
        ("usage: gnatcheck [options] {filename} {-files=filename} -rules rule_switches [-cargs gcc_switches]");
      Put_Line ("options:");
      Put_Line (" --version - Display version and exit");
      Put_Line (" --help    - Display usage and exit");
      Put_Line ("");
      Put_Line
        (" -Pproject        - Use project file project. Only one such switch can be used");
      Put_Line
        (" -U               - check all sources of the argument project");
      Put_Line
        (" -U main          - check the closure of units rooted at unit main");
      Put_Line (" --no-subprojects - process only sources of root project");
      Put_Line
        (" -Xname=value     - specify an external reference for argument project file");
      Put_Line
        (" --subdirs=dir    - specify subdirectory to place the result files into");
      Put_Line
        (" --no_objects_dir - place results into current dir instead of project dir");
      Put_Line
        (" -eL              - follow all symbolic links when processing project files");
      Put_Line ("");
      Put_Line
        (" --ignore-project-switches - ignore switches specified in the project file");
      Put_Line
        (" --target=targetname       - specify a target for cross platforms");
      Put_Line (" --RTS=<runtime>           - use runtime <runtime>");
      Put_Line
        (" --config=<cgpr>           - use configuration project <cgpr>");
      Put_Line ("");
      Put_Line
        (" -h                       - print out the list of the currently implemented rules");
      Put_Line
        (" -mn                      - n is the maximal number of diagnoses in Stderr");
      Put_Line
        ("                            (n in 0 .. 1000, 0 means no limit); default is 0");
      Put_Line
        (" -jn                      - n is the maximal number of processes");
      Put_Line
        (" -q                       - quiet mode (do not report detections in Stderr)");
      Put_Line (" -t                       - report execution time in Stderr");
      Put_Line (" -v                       - verbose mode");
      Put_Line
        (" -W, --warnings-as-errors - treat warning messages as errors");
      Put_Line
        (" -l                       - full pathname for file locations");
      Put_Line
        (" -log                     - duplicate all the messages sent to Stderr in gnatcheck.log");
      Put_Line (" -s                       - short form of the report file");
      Put_Line (" -xml                     - generate report in XML format");
      Put_Line
        (" -nt                      - do not generate text report (enforces '-xml')");
      Put_Line ("");
      Put_Line
        (" --show-rule                - append rule names to diagnoses generated");
      Put_Line
        (" --show-instantiation-chain - show instantiation chain for reported generic construct");
      Put_Line ("");
      Put_Line
        (" --brief              - brief mode, only report detections in Stderr");
      Put_Line
        (" --check-redefinition - issue warning if a rule parameter is redefined");
      Put_Line
        (" --check-semantic     - check semantic validity of the source files");
      Put_Line
        (" --charset=<charset>  - specify the charset of the source files");

      if not Legacy then
         Put_Line
           (" --rules-dir=<dir>    - specify an alternate directory containing rule files");
      end if;

      Put_Line ("");
      Put_Line
        (" --include-file=filename - add the content of filename into generated report");
      Put_Line ("");
      Put_Line (" -o filename   - specify the name of the text report file");
      Put_Line
        (" -ox filename  - specify the name of the XML report file (enforces '-xml')");
      Put_Line ("");
      Put_Line
        (" filename                 - the name of the Ada source file to be analyzed.");
      Put_Line ("                            Wildcards are allowed");
      Put_Line
        (" -files=filename          - the name of the text file containing a list of Ada");
      Put_Line ("                            source files to analyze");
      Put_Line
        (" --ignore=filename        - do not process sources listed in filename");
      Put_Line
        (" --rule-file=filename     - read rule configuration from the given LKQL file");
      Put_Line
        (" -r, --rule [rule_name]   - enable the given rule during the GNATcheck run (this option is cumulative)");
      Put_Line
        (" --emit-lkql-rule-file    - emit a 'rules.lkql' file containing the rules configuration");
      Put_Line ("");
      Put_Line ("rule_switches          - a list of the following switches");
      Put_Line ("   -from=filename      - read rule options from filename");
      Put_Line
        ("   +R<rule_id>[:param] - turn ON a given rule [with given parameter]");
      Put_Line ("   -R<rule_id>         - turn OFF a given rule");
      Put_Line
        ("   -R<rule_id>:param   - turn OFF some of the checks for a given  rule,");
      Put_Line
        ("                         depending on the specified parameter");
      Put_Line ("where <rule_id> - ID of one of the currently implemented");
      Put_Line ("                  rules, use '-h' for the full list");
      Put_Line
        ("      param     - string representing parameter(s) of a given rule, more than ");
      Put_Line ("                  one parameter can be set separated by ','");

      pragma Style_Checks ("M79");
   end Brief_Help;

   ---------------------------
   -- Print_Gnatcheck_Usage --
   ---------------------------

   procedure Print_Gnatcheck_Usage is
   begin
      Brief_Help;
      New_Line;
      Put_Line ("Report bugs to support@adacore.com");
   end Print_Gnatcheck_Usage;

end Gnatcheck.Output;

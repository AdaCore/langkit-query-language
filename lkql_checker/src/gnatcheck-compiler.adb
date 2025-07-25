--
--  Copyright (C) 2005-2025, AdaCore
--  SPDX-License-Identifier: GPL-3.0-or-later
--

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Characters.Latin_1;
with Ada.Containers;
with Ada.Directories;         use Ada.Directories;
with Ada.Strings;             use Ada.Strings;
with Ada.Strings.Fixed;       use Ada.Strings.Fixed;
with Ada.Strings.Unbounded;

with GNAT.Case_Util;
with GNAT.Regpat; use GNAT.Regpat;
with GNAT.Strings;
with GNAT.String_Split;

with Gnatcheck.Diagnoses;          use Gnatcheck.Diagnoses;
with Gnatcheck.Ids;                use Gnatcheck.Ids;
with Gnatcheck.Options;            use Gnatcheck.Options;
with Gnatcheck.Output;             use Gnatcheck.Output;
with Gnatcheck.Projects;           use Gnatcheck.Projects;
with Gnatcheck.Projects.Aggregate; use Gnatcheck.Projects.Aggregate;
with Gnatcheck.Rules.Rule_Table;   use Gnatcheck.Rules.Rule_Table;
with Gnatcheck.Source_Table;       use Gnatcheck.Source_Table;
with Gnatcheck.String_Utilities;   use Gnatcheck.String_Utilities;

with GNATCOLL.VFS; use GNATCOLL.VFS;

with Langkit_Support.Slocs; use Langkit_Support.Slocs;

package body Gnatcheck.Compiler is

   use Rident;

   Style_Options_String : String_Access := new String'("-gnaty");
   --  Stores parameters of the Style_Checks

   Warning_Options_String : String_Access := new String'("-gnatw");
   --  Stores parameters of the Warnings rule

   type Message_Kinds is (Not_A_Message, Warning, Style, Restriction, Error);

   function To_Mixed (A : String) return String
   renames GNAT.Case_Util.To_Mixed;

   procedure Process_Style_Options (Param : String);
   --  Stores Param as parameter of the compiler -gnaty... option as is,
   --  (if some -gnaty... parameter has already been stored, appends Param to
   --  it.)

   function Adjust_Message
     (Diag : String; Message_Kind : Message_Kinds) return String;
   --  Does the following adjustments:
   --
   --  * Remove from the diagnostic message the reference to the configuration
   --    file with restriction pragmas that is created by gnatcheck.
   --
   --  * Remove warning and style markers for Warning, Style, Restriction
   --    messages.
   --
   --  * If Gnatcheck.Options.Mapping_Mode is ON, annotates the message by
   --    adding the compiler check (if for a warning message '.d' is specified,
   --    the trailing part that indicates the warning message that causes this
   --    warning is removed from the diagnosis, and the corresponding warning
   --    parameter is added to the annotation, as well as the user synonym, if
   --    any.

   function Annotation
     (Message_Kind : Message_Kinds; Parameter : String) return String;
   --  Returns annotation to be added to the compiler diagnostic message if
   --  Gnatcheck.Options.Mapping_Mode is ON. Parameter, if non-empty, is the
   --  parameter of '-gnatw/y' option that causes the diagnosis

   function Get_Rule_Id (Check : Message_Kinds) return Rule_Id;
   --  Returns the Id corresponding to the given compiler check

   function Path_Index (Source, Pattern : String) return Integer;
   --  Returns the index of the first occurence of the path represented by
   --  ``Pattern`` inside the ``Source`` string.
   --  This function treats the provided path as case-insensitive on Windows
   --  systems.

   ---------------------------------
   -- Target information fetching --
   ---------------------------------

   function Get_Available_Targets return String_Sets.Set;
   --  Call the ``gprconfig`` tool to list all currently available targets. If
   --  you want the list of available targets when GNATcheck has been started,
   --  use the ``Available_Targets`` global to avoid useless calls.

   function Get_Available_Targets return String_Sets.Set is
      Res                 : String_Sets.Set;
      GPRConfig_Exec      : String_Access := Locate_Exec_On_Path ("gprconfig");
      Args                : Argument_List (1 .. 2);
      Return_Code         : Integer := -1;
      Output_File         : File_Descriptor;
      Output_File_Name    : String_Access;
      Output_File_Content : String_Access;
      Split_Content       : String_Vector;
   begin
      --  If no regular "gprconfig" has been found, look for the gnatsas one
      if GPRConfig_Exec = null then
         GPRConfig_Exec := Locate_Exec_On_Path ("gnatsas-gprconfig");

         --  If the gnatsas "gprconfig" is not available, look for the codepeer
         --  one (for retro-compatibility purposes).
         if GPRConfig_Exec = null then
            GPRConfig_Exec := Locate_Exec_On_Path ("codepeer-gprconfig");

            --  If the result is still null, raise a fatal error. We cannot
            --  continue the analysis execution.
            if GPRConfig_Exec = null then
               Error ("cannot locate gprconfig executable");
               raise Fatal_Error;
            end if;
         end if;
      end if;

      --  Create the temporary file to get the "gprbuild" output
      Create_Temp_Output_File (Output_File, Output_File_Name);

      --  Prepare the argument list
      Args (1) := new String'("--show-targets");
      Args (2) := new String'("--config=Ada");

      --  Spawn "gprconfig" to fetch the list of available targets
      Spawn
        (Program_Name           => GPRConfig_Exec.all,
         Args                   => Args,
         Output_File_Descriptor => Output_File,
         Return_Code            => Return_Code,
         Err_To_Out             => False);

      --  Parse the output to fill the result
      Output_File_Content := Read_File (Output_File_Name.all);
      Split_Content :=
        Split
          (Output_File_Content.all,
           Ada.Characters.Latin_1.LF,
           Trim_Elems => True);
      for I in Split_Content.First_Index + 1 .. Split_Content.Last_Index loop
         if Split_Content (I) /= "" then
            Res.Include (Split_Content (I));
         end if;
      end loop;

      --  Release allocated resources and delete the temporary file
      Close (Output_File);
      Delete_File (Output_File_Name.all);
      Free (GPRConfig_Exec);
      Free (Output_File_Name);
      Free (Output_File_Content);
      for I in Args'Range loop
         Free (Args (I));
      end loop;

      --  Finally return the set of available targets
      return Res;
   end Get_Available_Targets;

   Available_Targets : constant String_Sets.Set := Get_Available_Targets;
   --  Cache containing all available targets when GNATcheck has been started.

   ---------------------------------------------------------
   -- Data structures and routines for restriction checks --
   ---------------------------------------------------------

   subtype Option_Parameter is Natural;

   type Restriction_State is record
      Active : Boolean;
      Param  : String_Vector;
   end record;
   --  We can not use Option_Parameter here, because some restrictions (e.g.
   --  Max_Task_Entries) may be active and may have zero parameter

   Restriction_Setting : array (All_Restrictions) of Restriction_State :=
     [others => (False, String_Vectors.Empty_Vector)];
   --  This array represents only restrictions that are values of
   --  System.Rident.Restriction_Id. But we need to process restrictions that
   --  are not included in values of this type.

   type Special_Restriction_Id is
     (Not_A_Special_Restriction_Id,
      No_Dependence,
      No_Specification_Of_Aspect,
      No_Use_Of_Entity);

   subtype All_Special_Restrictions is
     Special_Restriction_Id range No_Dependence .. No_Use_Of_Entity;
   --  All special restrictions, excluding Not_A_Special_Restriction_Id.

   subtype All_Special_Parameter_Restrictions is
     Special_Restriction_Id range No_Dependence .. No_Use_Of_Entity;
   --  All special restrictions that have a parameter

   procedure Get_Restriction_Id
     (Restriction_Name : String;
      R_Id             : in out Restriction_Id;
      Special_R_Id     : in out Special_Restriction_Id);
   --  Get the identifier of the restriction correspinding to the given
   --  ``Restriction_Name``. This restriction may be a "special" restriction.

   function Needs_Parameter_In_Exemption
     (R : Restriction_Id; SR : Special_Restriction_Id) return Boolean;
   --  Checks if R or SR denotes a restriction that needs a restriction
   --  parameter if used in parametric rule exemption (such as
   --  'No_Dependence => Foo).

   Special_Restriction_Setting : array (All_Special_Restrictions) of Boolean :=
     [others => False];
   --  This array only indicates if a given special restriction is ON or OFF,
   --  we cannot store any restriction parameter information, because
   --  parameter format is restriction-specific

   package Forbidden_Units_Dictionary is new
     Simple_String_Dictionary
       (Dictionary_Name => "Forbidden units dictionary");

   package Forbidden_Entities_Dictionary is new
     Simple_String_Dictionary
       (Dictionary_Name => "Forbidden entities dictionary");

   package Forbidden_Aspects_Dictionary is new
     Simple_String_Dictionary
       (Dictionary_Name => "Forbidden aspects dictionary");

   --------------------
   -- Adjust_Message --
   --------------------

   function Adjust_Message
     (Diag : String; Message_Kind : Message_Kinds) return String
   is
      Result : constant String :=
        (case Message_Kind is
           when Warning | Restriction =>
             (declare
                Idx : constant Natural := Index (Diag, "warning: ");
              begin
                (if Idx = 0
                 then Diag
                 else
                   Diag (Diag'First .. Idx - 1)
                   & Diag (Idx + 9 .. Diag'Last))),
           when Style =>
             (declare
                Idx : constant Natural := Index (Diag, "(style) ");
              begin
                (if Idx = 0
                 then Diag
                 else
                   Diag (Diag'First .. Idx - 1)
                   & Diag (Idx + 8 .. Diag'Last))),
           when others => Diag);

      Last_Idx  : Natural;
      Diag_End  : Natural;
      Par_Start : Natural := Result'First;
      Par_End   : Natural := 0;
      Sep_Idx   : Natural := 0;

   begin
      Last_Idx := Path_Index (Result, Gnatcheck_Config_File.all);

      if Last_Idx = 0 then
         Last_Idx := Result'Last;
      else
         Last_Idx := Last_Idx - 5;
      end if;

      if Arg.Show_Rule.Get then
         if Message_Kind in Warning | Style then
            Diag_End :=
              Index
                (Source  => Result (Result'First .. Last_Idx),
                 Pattern =>
                   (if Message_Kind = Warning then "[-gnatw" else "[-gnaty"),
                 Going   => Backward);

            if Diag_End = 0 then
               Diag_End := Last_Idx;
            else
               Par_Start := Diag_End + 7;
               Par_End := Par_Start;

               if Result (Par_End) in '.' | '_' then
                  Par_End := Par_End + 1;
               end if;

               Diag_End := Diag_End - 2;
            end if;

            return
              Result (Result'First .. Diag_End)
              & Annotation (Message_Kind, Result (Par_Start .. Par_End));
         else
            Diag_End := Last_Idx;
            if Index (Result, "of restriction ") /= 0 then
               Par_Start := Index (Result, "of restriction ") + 16;
               Par_End := Index (Result (Par_Start .. Diag_End), """") - 1;
               Sep_Idx := Index (Result (Par_Start .. Diag_End), "=");

               if Sep_Idx /= 0 then
                  Par_End := Sep_Idx - 1;
                  while Par_End > Par_Start and then Result (Par_End) = ' '
                  loop
                     Par_End := @ - 1;
                  end loop;
               end if;

            elsif Index (Result, "violates restriction ") /= 0 then
               Par_Start := Index (Result, "violates restriction ") + 21;
               Par_End := Index (Result (Par_Start .. Result'Last), " at") - 1;
            end if;

            return
              Result (Result'First .. Diag_End)
              & Annotation
                  (Message_Kind, To_Lower (Result (Par_Start .. Par_End)));
         end if;
      else
         return Result (Result'First .. Last_Idx);
      end if;
   end Adjust_Message;

   ----------------------------
   -- Analyze_Builder_Output --
   ----------------------------

   procedure Analyze_Output (File_Name : String; Errors : out Boolean) is
      Out_File : constant String := File_Name & ".out";
      Line     : String (1 .. 1024);
      Line_Len : Natural;
      File     : File_Type;

      procedure Analyze_Line (Msg : String);
      --  Analyze one line containing a builder output. Insert the relevant
      --  messages into gnatcheck diagnoses table.

      ------------------
      -- Analyze_Line --
      ------------------

      procedure Analyze_Line (Msg : String) is
         Matches      : Match_Array (0 .. 5);
         Sloc         : Source_Location;
         SF           : SF_Id;
         Kind         : Diagnosis_Kinds := Rule_Violation;
         Message_Kind : Message_Kinds := Not_A_Message;

         Msg_Start : Natural;
         Msg_End   : Natural;

         procedure Format_Error;
         --  Emit an error about an unexpected format encountered and set
         --  Errors to True.

         ------------------
         -- Format_Error --
         ------------------

         procedure Format_Error is
         begin
            Error ("unparsable worker output: """ & Msg & """");
            Errors := True;
         end Format_Error;

      begin
         --  Skip all empty lines
         if Msg'Last = 0 then
            return;
         end if;

         --  We assume the following format of the message:
         --    filename:line:column: <message body>
         --
         --  If this format is violated we display the line as unparsable.

         --  Try to match the diagnostic to extract information
         Match (Match_Diagnosis, Msg, Matches);
         if Matches (0) = No_Match then
            Format_Error;
            return;
         end if;

         Msg_Start := Matches (5).First;
         Msg_End := Matches (5).Last;

         SF :=
           File_Find
             (Msg (Matches (1).First .. Matches (1).Last),
              Use_Short_Name => True);
         Sloc.Line :=
           Line_Number'Value (Msg (Matches (3).First .. Matches (3).Last));
         Sloc.Column :=
           Column_Number'Value (Msg (Matches (4).First .. Matches (4).Last));

         --  Handle internal warnings before any other kind of error, because
         --  they don't have an Ada source location
         --
         --  TODO: We want to refactor this logic and make the logic of
         --  handling internal errors more general.
         if Msg_End - Msg_Start > 22
           and then Msg (Msg_Start .. Msg_Start + 22)
                    = "warning: internal issue"
         then
            Warning (Msg (Msg_Start + 27 .. Msg_End));
            return;
         end if;

         --  Test if the provided sources is present and is not ignored
         if not Present (SF) or else Source_Info (SF) = Ignore_Unit then
            --  This diagnostic should be ignored
            return;
         end if;

         --  A gnatcheck message emitted by the worker
         if Msg (Msg_Start .. Msg_Start + 6) = "check: " then
            if Msg (Msg_End) /= ']' then
               Format_Error;
               return;
            end if;

            declare
               Last       : constant Natural :=
                 Index
                   (Source  => Msg (Msg_Start .. Msg_End),
                    Pattern => "[",
                    Going   => Backward);
               Name_Split : constant Natural :=
                 Index (Source => Msg (Last + 1 .. Msg_End), Pattern => "|");

               Rule_Name     : constant String :=
                 (if Name_Split /= 0
                  then Msg (Name_Split + 1 .. Msg_End - 1)
                  elsif Last /= 0
                  then Msg (Last + 1 .. Msg_End - 1)
                  else "");
               Instance_Name : constant String :=
                 (if Name_Split /= 0
                  then Msg (Last + 1 .. Name_Split - 1)
                  else "");

               Instance : Rule_Instance_Access := null;
            begin
               if Last = 0 then
                  Format_Error;
                  return;
               end if;
               Instance :=
                 Get_Instance
                   (if Instance_Name = ""
                    then To_Lower (Rule_Name)
                    else To_Lower (Instance_Name));
               Store_Diagnosis
                 (Full_File_Name => Gnatcheck.Source_Table.File_Name (SF),
                  Message        =>
                    Msg (Msg_Start + 7 .. Last - 2) & Instance.Annotate_Diag,
                  Sloc           => Sloc,
                  Diagnosis_Kind => Rule_Violation,
                  SF             => SF,
                  Rule           => Instance.Rule,
                  Instance       => Instance);
               return;
            end;

         --  An error message has been emitted
         elsif Msg (Msg_Start .. Msg_Start + 6) = "error: " then
            Message_Kind := Error;

            if Msg_End - Msg_Start > 21
              and then Msg (Msg_Start + 7 .. Msg_Start + 20) = "internal issue"
            then
               Kind := Internal_Error;
            else
               Kind := Compiler_Error;
            end if;

            Errors := True;

         --  A warning has been emitted by gprbuild
         elsif Msg (Msg_Start .. Msg_Start + 8) = "warning: " then
            if Index (Msg (Msg_Start .. Msg_End), ": violation of restriction")
              /= 0
              or else Index
                        (Msg (Msg_Start .. Msg_End), "violates restriction")
                      /= 0
            then
               Message_Kind := Restriction;
            elsif Msg (Msg_Start + 9 .. Msg_Start + 19) = "cannot find" then
               Report_Missing_File
                 (Gnatcheck.Source_Table.File_Name (SF),
                  Msg (Msg_Start + 21 .. Msg_End));
               return;
            else
               Message_Kind := Warning;
            end if;

         --  A style check violation has been emitted
         elsif Msg (Msg_Start .. Msg_Start + 6) = "(style)" then
            Message_Kind := Style;

         --  Ignore anything else
         else
            return;
         end if;

         --  Skip restriction message not coming from the GNATcheck config file
         if Message_Kind = Restriction
           and then Path_Index (Msg, Gnatcheck_Config_File.all) = 0
         then
            return;
         end if;

         --  Use File_Name to always use the same filename (including proper
         --  casing for case insensitive systems).
         Store_Diagnosis
           (Full_File_Name => Gnatcheck.Source_Table.File_Name (SF),
            Sloc           => Sloc,
            Message        =>
              Adjust_Message (Msg (Msg_Start .. Msg_End), Message_Kind),
            Diagnosis_Kind => Kind,
            SF             => SF,
            Rule           =>
              (if Message_Kind = Error
               then No_Rule_Id
               else Get_Rule_Id (Message_Kind)));
      end Analyze_Line;

      --  Start of processing for Analyze_Output

   begin
      Errors := False;

      --  If the .out file is not empty it means we got some errors, so display
      --  them.

      if Is_Regular_File (Out_File) and then Size (Out_File) /= 0 then
         Error ("error when calling gprbuild, raw output:");

         declare
            Str : String_Access := Read_File (Out_File);
         begin
            Print (Str (Str'First .. Str'Last - 1));
            Free (Str);
         end;

         Errors := True;
      end if;

      Open (File => File, Mode => In_File, Name => File_Name);

      while not End_Of_File (File) loop
         Get_Line (File, Line, Line_Len);

         if (Line_Len >= 24
             and then Line (1 .. 24) = "gnat1: bad -gnaty switch")
           or else (Line_Len > 29
                    and then Line (1 .. 29) = "gnat1: invalid switch: -gnatw")
         then
            Error ("wrong parameter specified for compiler-related rule:");
            Print (Line (1 .. Line_Len));
            Errors := True;

         elsif Index (Line (1 .. Line_Len), "BUG DETECTED") /= 0 then
            --  If there is a bug box, we should skip the rest of
            --  processing to avoid storing some completely unmanageable
            --  (for diagnoses storage) diagnoses

            --  Skip the next two lines of the bug box

            Get_Line (File, Line, Line_Len);
            Get_Line (File, Line, Line_Len);

            --  Parse the "| Compiling <file>   |" line, if present

            Get_Line (File, Line, Line_Len);

            if Line (3 .. 12) = "Compiling " then
               declare
                  SF : constant SF_Id :=
                    File_Find
                      (Line
                         (13
                          .. Index_Non_Blank (Line, Line_Len - 1, Backward)),
                       Use_Short_Name => True);

               begin
                  if Is_Argument_Source (SF) then
                     Errors := True;
                     Store_Diagnosis
                       (Full_File_Name => Source_Table.File_Name (SF),
                        Sloc           => (1, 1),
                        Message        =>
                          Adjust_Message ("fatal compiler error", Error),
                        Diagnosis_Kind => Compiler_Error,
                        SF             => SF);
                  end if;
               end;
            end if;
         elsif Line_Len >= 16 and then Line (1 .. 16) = "WORKER_WARNING: " then
            Warning (Line (17 .. Line_Len));
         elsif Line_Len >= 14 and then Line (1 .. 14) = "WORKER_ERROR: " then
            Error (Line (15 .. Line_Len));
            Detected_Internal_Error := @ + 1;
            Errors := True;
         else
            Analyze_Line (Line (1 .. Line_Len));
         end if;
      end loop;

      Close (File);

   exception
      when Ex : others =>
         if Is_Open (File) then
            Close (File);
         end if;

         Error ("unknown bug detected when analyzing tool output:");
         Report_Unhandled_Exception (Ex);
         Errors := True;
   end Analyze_Output;

   -----------------
   -- Annotation --
   ----------------

   function Annotation
     (Message_Kind : Message_Kinds; Parameter : String) return String is
   begin
      case Message_Kind is
         when Not_A_Message =>
            pragma Assert (False);
            return "";

         when Warning =>
            if Parameter = "" then
               return " [warnings]";
            elsif Warning_To_Instance.Contains (Parameter)
              and then Warning_To_Instance (Parameter) /= "warnings"
            then
               return
                 " ["
                 & Warning_To_Instance (Parameter)
                 & "|warnings:"
                 & Parameter
                 & "]";
            else
               return " [warnings:" & Parameter & "]";
            end if;

         when Style =>
            if Parameter = "" then
               return " [style_checks]";
            elsif Style_To_Instance.Contains (Parameter)
              and then Style_To_Instance (Parameter) /= "style_checks"
            then
               return
                 " ["
                 & Style_To_Instance (Parameter)
                 & "|style_checks:"
                 & Parameter
                 & "]";
            else
               return " [style_checks:" & Parameter & "]";
            end if;

         when Restriction =>
            if Restriction_To_Instance.Contains (Parameter)
              and then Restriction_To_Instance (Parameter) /= "restrictions"
            then
               return
                 " [" & Restriction_To_Instance (Parameter) & "|restrictions]";
            else
               return " [restrictions]";
            end if;

         when Error =>
            return " [errors]";
      end case;
   end Annotation;

   -------------------------------------
   -- Create_Restriction_Pragmas_File --
   -------------------------------------

   procedure Create_Restriction_Pragmas_File is
      use Ada.Strings.Unbounded;

      Contents : Unbounded_String;

      procedure Add_Line (Line : String);
      --  Add Line to Contents, and append a LF character

      --------------
      -- Add_Line --
      --------------

      procedure Add_Line (Line : String) is
      begin
         Append (Contents, Line);
         Append (Contents, [ASCII.LF]);
      end Add_Line;

   begin
      --  Create the contents of the file in memory first and then if the file
      --  already exists, compare its contents and do nothing if the contents
      --  are the same to avoid changing the file's timestamp and trigger
      --  spurious recompilations.

      Add_Line ("pragma Warnings (Off, ""[enabled by default]"");");

      for R in All_Restrictions loop
         if Restriction_Setting (R).Active then
            if R in All_Boolean_Restrictions then
               Add_Line ("pragma Restriction_Warnings (" & R'Img & ");");
            else
               for J in Restriction_Setting (R).Param.Iterate loop
                  Append (Contents, "pragma Restriction_Warnings (");
                  Append (Contents, R'Img);
                  Append (Contents, " =>" & Restriction_Setting (R).Param (J));
                  Add_Line (");");
               end loop;
            end if;
         end if;
      end loop;

      for R in Special_Restriction_Setting'Range loop
         if Special_Restriction_Setting (R) then
            case R is
               when No_Dependence =>
                  Forbidden_Units_Dictionary.Reset_Iterator;

                  while not Forbidden_Units_Dictionary.Done loop
                     Append
                       (Contents,
                        "pragma Restriction_Warnings (No_Dependence => ");
                     Add_Line (Forbidden_Units_Dictionary.Next_Entry & ");");
                  end loop;

               when No_Use_Of_Entity =>
                  Forbidden_Entities_Dictionary.Reset_Iterator;

                  while not Forbidden_Entities_Dictionary.Done loop
                     Append
                       (Contents,
                        "pragma Restriction_Warnings (No_Use_Of_Entity => ");
                     Add_Line
                       (Forbidden_Entities_Dictionary.Next_Entry & ");");
                  end loop;

               when No_Specification_Of_Aspect =>
                  Forbidden_Aspects_Dictionary.Reset_Iterator;

                  while not Forbidden_Aspects_Dictionary.Done loop
                     Append
                       (Contents,
                        "pragma Restriction_Warnings "
                        & "(No_Specification_Of_Aspect => ");
                     Add_Line (Forbidden_Aspects_Dictionary.Next_Entry & ");");
                  end loop;
            end case;
         end if;
      end loop;

      declare
         Config_File   : constant Virtual_File :=
           Create (+Gnatcheck_Config_File.all);
         File          : Writable_File;
         Old_Contents  : GNAT.Strings.String_Access;
         New_Contents  : constant String := To_String (Contents);
         Same_Contents : Boolean;

      begin
         if Is_Regular_File (Config_File) then
            Old_Contents := Read_File (Config_File);
            Same_Contents := Old_Contents.all = New_Contents;
            Free (Old_Contents);

            if Same_Contents then
               --  Nothing more to do: we don't want to change the timestamp
               --  of the configuration file.

               return;
            end if;
         end if;

         File := Write_File (Config_File);
         Write (File, New_Contents);
         Close (File);
      end;
   end Create_Restriction_Pragmas_File;

   -----------------
   -- Get_Rule_Id --
   -----------------

   function Get_Rule_Id (Check : Message_Kinds) return Rule_Id is
   begin
      case Check is
         when Not_A_Message | Error =>
            pragma Assert (False);
            return No_Rule_Id;

         when Warning =>
            return Warnings_Id;

         when Style =>
            return Style_Checks_Id;

         when Restriction =>
            return Restrictions_Id;
      end case;
   end Get_Rule_Id;

   ----------------
   -- Path_Index --
   ----------------

   function Path_Index (Source, Pattern : String) return Integer is
      Case_Sensitive : constant Boolean :=
        GNAT.OS_Lib.Directory_Separator /= '\';
   begin
      if Case_Sensitive then
         return Index (Source, Pattern);
      else
         declare
            Offset : constant Integer :=
              Index (To_Lower (Source), To_Lower (Pattern));
         begin
            if Offset /= 0 then
               return Source'First + Offset - 1;
            else
               return 0;
            end if;
         end;
      end if;
   end Path_Index;

   ----------------------
   -- Get_Style_Option --
   ----------------------

   function Get_Style_Option return String is
   begin
      return Style_Options_String.all;
   end Get_Style_Option;

   ------------------------
   -- Get_Warning_Option --
   ------------------------

   function Get_Warning_Option return String is
   begin
      return Warning_Options_String.all;
   end Get_Warning_Option;

   ------------------------
   -- Get_Restriction_Id --
   ------------------------

   procedure Get_Restriction_Id
     (Restriction_Name : String;
      R_Id             : in out Restriction_Id;
      Special_R_Id     : in out Special_Restriction_Id) is
   begin
      begin
         R_Id := Restriction_Id'Value (Restriction_Name);
      exception
         when Constraint_Error =>
            R_Id := Not_A_Restriction_Id;
      end;

      if R_Id = Not_A_Restriction_Id then
         begin
            Special_R_Id := Special_Restriction_Id'Value (Restriction_Name);
         exception
            when Constraint_Error =>
               Special_R_Id := Not_A_Special_Restriction_Id;
         end;
      end if;
   end Get_Restriction_Id;

   ----------------------------------
   -- Needs_Parameter_In_Exemption --
   ----------------------------------

   function Needs_Parameter_In_Exemption
     (R : Restriction_Id; SR : Special_Restriction_Id) return Boolean is
   begin
      if SR in All_Special_Parameter_Restrictions then
         return True;
      elsif R in All_Parameter_Restrictions then

         --  Not all the restrictions from All_Parameter_Restrictions require
         --  restriction parameter in parametric exemptions

         return R not in Integer_Parameter_Restrictions;

      else
         return False;
      end if;
   end Needs_Parameter_In_Exemption;

   ---------------------------
   -- Is_Restriction_Active --
   ---------------------------

   function Is_Restriction_Active (Restriction_Name : String) return Boolean is
      R_Id         : Restriction_Id := Not_A_Restriction_Id;
      Special_R_Id : Special_Restriction_Id := Not_A_Special_Restriction_Id;
   begin
      Get_Restriction_Id (Restriction_Name, R_Id, Special_R_Id);
      if R_Id /= Not_A_Restriction_Id then
         return Restriction_Setting (R_Id).Active;
      elsif Special_R_Id /= Not_A_Special_Restriction_Id then
         return Special_Restriction_Setting (Special_R_Id);
      end if;
      return False;
   end Is_Restriction_Active;

   ----------------------------------
   -- Is_Restriction_Exemption_Par --
   ----------------------------------

   function Is_Restriction_Exemption_Par (Par : String) return Boolean is
      Result        : Boolean := False;
      Arrow_Idx     : constant Natural := Index (Par, "=>");
      Rest_Name_End : Natural := Par'Last;
      Par_Start     : constant Positive := Par'First;
      R_Id          : Restriction_Id := Not_A_Restriction_Id;
      Special_R_Id  : Special_Restriction_Id := Not_A_Special_Restriction_Id;

   begin
      if Arrow_Idx /= 0 then
         Rest_Name_End := Arrow_Idx - 1;
      end if;

      Get_Restriction_Id
        (Par (Par_Start .. Rest_Name_End), R_Id, Special_R_Id);

      if R_Id /= Not_A_Restriction_Id
        or else Special_R_Id /= Not_A_Special_Restriction_Id
      then
         if Arrow_Idx /= 0 then
            Result := Needs_Parameter_In_Exemption (R_Id, Special_R_Id);
         else
            Result := not Needs_Parameter_In_Exemption (R_Id, Special_R_Id);
         end if;
      end if;

      return Result;
   end Is_Restriction_Exemption_Par;

   ----------------------------
   -- Is_Style_Exemption_Par --
   ----------------------------

   function Is_Style_Exemption_Par (Par : String) return Boolean is
   begin
      --  We consider any string that can be used as a parameter of '-gnaty'
      --  option as allowed exemption parameter, except for +/- which is not
      --  supported.

      for J in Par'Range loop
         if Par (J) not in 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' then
            return False;
         end if;
      end loop;

      return True;
   end Is_Style_Exemption_Par;

   ------------------------------
   -- Is_Warning_Exemption_Par --
   ------------------------------

   function Is_Warning_Exemption_Par (Par : String) return Boolean is
      Last_Idx : constant Positive := Par'Last;
      Result   : Boolean := True;
   begin
      --  We consider any string that can be used as a parameter of '-gnatw'
      --  option as allowed exemption parameter

      for J in Par'Range loop
         if Par (J) not in '.' | '_' | 'a' .. 'z' | 'A' .. 'Z' then
            Result := False;
            exit;
         end if;

         if Par (J) in '.' | '_'
           and then (J = Last_Idx or else not Is_Letter (Par (J + 1)))
         then
            Result := False;
            exit;
         end if;
      end loop;

      return Result;
   end Is_Warning_Exemption_Par;

   -----------------------------
   -- Active_Restriction_List --
   -----------------------------

   function Active_Restrictions_List
     (Separator : String; Elem_Prefix : String; Elem_Postfix : String)
      return String
   is
      use Ada.Strings.Unbounded;

      Res        : Unbounded_String;
      First_Elem : Boolean := True;

      procedure Append_Elem (Elem : String);
      --  Append an element in the result, formatted as required

      procedure Append_Elem (Elem : String) is
      begin
         if First_Elem then
            First_Elem := False;
         else
            Append (Res, Separator);
         end if;
         Append (Res, Elem_Prefix);
         Append (Res, Elem);
         Append (Res, Elem_Postfix);
      end Append_Elem;

   begin
      for R in Restriction_Setting'Range loop
         if Restriction_Setting (R).Active then
            if R in All_Boolean_Restrictions then
               Append_Elem (To_Mixed (R'Img));
            else
               for J in Restriction_Setting (R).Param.Iterate loop
                  Append_Elem
                    (To_Mixed (R'Img)
                     & " => "
                     & Restriction_Setting (R).Param (J));
               end loop;
            end if;
         end if;
      end loop;

      for R in Special_Restriction_Setting'Range loop
         if Special_Restriction_Setting (R) then
            case R is
               when No_Dependence =>
                  Forbidden_Units_Dictionary.Reset_Iterator;
                  while not Forbidden_Units_Dictionary.Done loop
                     Append_Elem
                       ("No_Dependence => "
                        & Forbidden_Units_Dictionary.Next_Entry);
                  end loop;

               when No_Use_Of_Entity =>
                  Forbidden_Entities_Dictionary.Reset_Iterator;
                  while not Forbidden_Entities_Dictionary.Done loop
                     Append_Elem
                       ("No_Use_Of_Entity => "
                        & Forbidden_Entities_Dictionary.Next_Entry);
                  end loop;

               when No_Specification_Of_Aspect =>
                  Forbidden_Aspects_Dictionary.Reset_Iterator;
                  while not Forbidden_Aspects_Dictionary.Done loop
                     Append_Elem
                       ("No_Specification_Of_Aspect => "
                        & Forbidden_Aspects_Dictionary.Next_Entry);
                  end loop;
            end case;
         end if;
      end loop;

      return To_String (Res);
   end Active_Restrictions_List;

   -------------------------------
   -- Print_Active_Restrictions --
   -------------------------------

   procedure Print_Active_Restrictions (Ident_Level : Natural := 0) is
   begin
      Report
        (Active_Restrictions_List
           (Separator    => ASCII.LF & (Ident_Level * Indent_String),
            Elem_Prefix  => "",
            Elem_Postfix => ""));
   end Print_Active_Restrictions;

   ---------------------------------------
   -- Print_Active_Restrictions_To_File --
   ---------------------------------------

   procedure Print_Active_Restrictions_To_File (Rule_File : File_Type) is
   begin
      Put_Line
        (Rule_File,
         Active_Restrictions_List
           (Separator    => [ASCII.LF],
            Elem_Prefix  => "+RRestrictions : ",
            Elem_Postfix => ""));
   end Print_Active_Restrictions_To_File;

   -------------------------------
   -- Process_Restriction_Param --
   -------------------------------
   procedure Process_Restriction_Param
     (Parameter : String; Instance : Rule_Instance_Access)
   is
      Param        : constant String := Trim (Parameter, Both);
      First_Idx    : constant Natural := Param'First;
      I_Name       : constant String := Instance_Name (Instance.all);
      Last_Idx     : Natural := Param'Last;
      Arg_Present  : Boolean := False;
      R_Id         : Restriction_Id := Not_A_Restriction_Id;
      Special_R_Id : Special_Restriction_Id := Not_A_Special_Restriction_Id;
      R_Val        : Option_Parameter;

      Success : Boolean;
      Cursor  : String_Maps.Cursor;
   begin
      --  Param should have the format
      --
      --   restriction_parameter_identifier[ => restriction_parameter_argument]
      --
      --  We assume that it can be spaces around '=>'

      --  First, try to define the restriction name.

      for J in First_Idx + 1 .. Last_Idx loop
         if Param (J) = ' ' or else Param (J) = '=' then
            Last_Idx := J - 1;
            exit;
         end if;
      end loop;

      declare
         Rest_Name       : constant String := Param (First_Idx .. Last_Idx);
         Lower_Rest_Name : constant String := To_Lower (Rest_Name);
      begin
         Get_Restriction_Id (Rest_Name, R_Id, Special_R_Id);

         if R_Id = Not_A_Restriction_Id
           and then Special_R_Id = Not_A_Special_Restriction_Id
         then
            Instance.Error
              ("wrong restriction identifier : " & Rest_Name & ", ignored");
            Bad_Rule_Detected := True;
            return;
         end if;

         --  Add the restriction lowered name as a tag associated to the
         --  current instance, if it is an alias.
         Restriction_To_Instance.Insert
           (Lower_Rest_Name, I_Name, Cursor, Success);
         if not Success and then Restriction_To_Instance (Cursor) /= I_Name
         then
            Instance.Error
              ("cannot enable the same restriction in different rule "
               & "instances: "
               & Rest_Name);
            Bad_Rule_Detected := True;
            return;
         end if;
      end;

      --  Check if we have a restriction_parameter_argument, and if we do,
      --  set First_Idx to the first character after '=>'
      for J in Last_Idx + 1 .. Param'Last - 2 loop
         if Param (J) = '=' then
            if J <= Param'Last - 2 and then Param (J + 1) = '>' then
               Arg_Present := True;
               Last_Idx := J + 2;
               exit;

            else
               Instance.Error
                 ("wrong structure of restriction rule parameter "
                  & Param
                  & ", ignored");
               Bad_Rule_Detected := True;

               return;
            end if;
         end if;
      end loop;

      if R_Id in All_Boolean_Restrictions then

         if Arg_Present then
            Instance.Error
              ("RESTRICTIONS rule parameter: "
               & Param
               & " can not contain expression, ignored");
            Bad_Rule_Detected := True;
         else
            Restriction_Setting (R_Id).Active := True;
         end if;

      elsif R_Id /= Not_A_Restriction_Id then

         if not Arg_Present then
            Instance.Error
              ("RESTRICTIONS rule parameter: "
               & Param
               & " should contain an expression, ignored");
            Bad_Rule_Detected := True;

            return;
         else
            if R_Id in Integer_Parameter_Restrictions then
               begin
                  R_Val :=
                    Option_Parameter'Value
                      (Trim (Param (Last_Idx .. Param'Last), Both));

                  if not Restriction_Setting (R_Id).Param.Is_Empty
                    and then Arg.Check_Redefinition.Get
                  then
                     Restriction_Setting (R_Id).Param.Clear;
                     Last_Idx := Index (Param, "=", Backward) - 1;

                     for J in reverse First_Idx .. Last_Idx loop
                        if Param (J) /= ' ' then
                           Last_Idx := J;
                           exit;
                        end if;
                     end loop;

                     Instance.Error
                       ("expression for RESTRICTIONS rule parameter: "
                        & Param (First_Idx .. Last_Idx)
                        & " is specified more than once");
                     Rule_Option_Problem_Detected := True;
                  end if;

                  Restriction_Setting (R_Id).Param.Append (R_Val'Img);
               exception
                  when Constraint_Error =>
                     Instance.Error
                       ("wrong restriction parameter expression in "
                        & Param
                        & ", ignored");
                     Bad_Rule_Detected := True;

                     return;
               end;
            else
               --  No check is made for the moment for non-integer restriction
               --  parameters:

               Restriction_Setting (R_Id).Param.Append
                 (Trim (Param (Last_Idx .. Param'Last), Both));
            end if;
         end if;

         Restriction_Setting (R_Id).Active := True;

      else
         --  If we are here, R_Id = Not_A_Restriction_Id, therefore
         --  Special_R_Id /= Not_A_Special_Restriction_Id

         case Special_R_Id is
            when No_Dependence =>
               if not Arg_Present then
                  Instance.Error
                    ("Restrictions rule parameter: "
                     & Param
                     & " should contain a unit name, ignored");
                  Bad_Rule_Detected := True;

                  return;
               end if;

               Special_Restriction_Setting (Special_R_Id) := True;
               Forbidden_Units_Dictionary.Add_To_Dictionary
                 (Trim (Param (Last_Idx .. Param'Last), Both));

            when No_Use_Of_Entity =>
               if not Arg_Present then
                  Instance.Error
                    ("Restrictions rule parameter: "
                     & Param
                     & " should contain an entity name, ignored");
                  Bad_Rule_Detected := True;

                  return;
               end if;

               Special_Restriction_Setting (Special_R_Id) := True;
               Forbidden_Entities_Dictionary.Add_To_Dictionary
                 (Trim (Param (Last_Idx .. Param'Last), Both));

            when No_Specification_Of_Aspect =>
               if not Arg_Present then
                  Instance.Error
                    ("Restrictions rule parameter: "
                     & Param
                     & " should contain an aspect name, ignored");
                  Bad_Rule_Detected := True;

                  return;
               end if;

               Special_Restriction_Setting (Special_R_Id) := True;
               Forbidden_Aspects_Dictionary.Add_To_Dictionary
                 (Trim (Param (Last_Idx .. Param'Last), Both));

            when Not_A_Special_Restriction_Id =>
               pragma Assert (False);
         end case;
      end if;

      --  Check if a warning about restrictions that should be checked at
      --  compile time instead of coding standard should be issued (these
      --  restrictions need information that does not exist in the trees
      --  created for ASIS)

      if R_Id
         in No_Implicit_Dynamic_Code
          | No_Elaboration_Code
          | No_Implicit_Heap_Allocations
          | No_Implicit_Task_Allocations
          | No_Implicit_Protected_Object_Allocations
          | No_Secondary_Stack
          | No_Implicit_Conditionals
          | No_Implicit_Loops
          | No_Default_Initialization
          | Static_Dispatch_Tables
          | No_Exception_Propagation
      then
         Instance.Warning
           ("restriction "
            & To_Mixed (R_Id'Img)
            & " ignored - only fully effective during code generation");

         Restriction_Setting (R_Id).Active := False;
      end if;

      --  Check if a warning about (potentially) statically uncheckable
      --  restriction should be issued

      if R_Id
         in No_Standard_Allocators_After_Elaboration
          | Max_Entry_Queue_Length
          | Max_Storage_At_Blocking
          | Max_Tasks
          | No_Task_Termination
          | No_Entry_Queue
          | No_Reentrancy
      then
         Instance.Warning
           ("restriction "
            & To_Mixed (R_Id'Img)
            & " ignored - cannot be checked statically");

         Restriction_Setting (R_Id).Active := False;

      elsif R_Id = No_Recursion then
         Instance.Warning
           ("restriction No_Recursion ignored (cannot be checked statically), "
            & "use rule Recursive_Subprograms instead");

         Restriction_Setting (R_Id).Active := False;

      elsif R_Id = Max_Asynchronous_Select_Nesting and then R_Val /= 0 then
         Instance.Warning
           ("restriction Max_Asynchronous_Select_Nesting ignored - "
            & "cannot be checked statically if parameter is not 0");
         Restriction_Setting (R_Id).Active := False;
      end if;
   end Process_Restriction_Param;

   --------------------------
   -- Disable_Restrictions --
   --------------------------

   procedure Disable_Restrictions is
   begin
      for J in Restriction_Setting'Range loop
         Restriction_Setting (J).Active := False;
         Restriction_Setting (J).Param.Clear;
      end loop;

      for J in Special_Restriction_Setting'Range loop
         Special_Restriction_Setting (J) := False;
      end loop;
   end Disable_Restrictions;

   -------------------------------
   -- Process_Style_Check_Param --
   -------------------------------

   procedure Process_Style_Check_Param
     (Param : String; Instance : Rule_Instance_Access)
   is
      Name : constant String := Instance_Name (Instance.all);
      C    : Character;
      I    : Integer;

      Success : Boolean;
      Cursor  : String_Maps.Cursor;
   begin
      --  Special arguments "all_checks" enables all style checks
      if Param = "all_checks" then
         for C of String'("0aAbcefhiklmnprst") loop
            Style_To_Instance.Include ([C], Name);
         end loop;

      --  Else, process the argument as a sequence of "gnaty" parameters.

      else
         I := Param'First;
         while I <= Param'Last loop
            C := Param (I);
            case C is
               --  -gnaty[1-9] is represented by "0"

               when '1' .. '9' =>
                  C := '0';

               --  -gnatyLxx and -gnatyMxxx are represented respectively by
               --  "L" and "M". Skip all digits directly after the flag.

               when 'L' | 'M' =>
                  while I < Param'Last and then Param (I + 1) in '0' .. '9'
                  loop
                     I := I + 1;
                  end loop;

               when others =>
                  null;
            end case;

            Style_To_Instance.Insert ([C], Name, Cursor, Success);
            if not Success and then Style_To_Instance (Cursor) /= Name then
               Instance.Error
                 ("cannot enable the same style check in different rule "
                  & "instances: "
                  & C);
               Bad_Rule_Detected := True;
               return;
            end if;

            I := I + 1;
         end loop;
      end if;

      if To_Lower (Param) = "all_checks" then
         Process_Style_Options ("y");
      else
         Process_Style_Options (Param);
      end if;
   end Process_Style_Check_Param;

   ---------------------------
   -- Process_Style_Options --
   ---------------------------

   procedure Process_Style_Options (Param : String) is
      Options : constant String := Style_Options_String.all;
   begin
      Use_gnaty_Option := True;
      Free (Style_Options_String);

      --  If the previous option is using a number and Param also starts with
      --  a number, we cannot concatenate them, so split the options.

      if Options'Length /= 0
        and then Options (Options'Last) in '0' .. '9'
        and then Param (Param'First) in '0' .. '9'
      then
         Style_Options_String := new String'(Options & " -gnaty" & Param);
      else
         Style_Options_String := new String'(Options & Param);
      end if;
   end Process_Style_Options;

   --------------------------
   -- Disable_Style_Checks --
   --------------------------

   procedure Disable_Style_Checks is
   begin
      Use_gnaty_Option := False;
      Free (Style_Options_String);
      Style_Options_String := new String'("-gnaty");
   end Disable_Style_Checks;

   ---------------------------
   -- Process_Warning_Param --
   ---------------------------

   procedure Process_Warning_Param
     (Param : String; Instance : Rule_Instance_Access)
   is
      New_Options : constant String := Warning_Options_String.all & Param;
      Name        : constant String := Instance_Name (Instance.all);
      C           : Character;
      I           : Integer;
      J           : Integer;

      Success : Boolean;
      Cursor  : String_Maps.Cursor;
   begin
      --  Checking for 'e' and 's' that should not be supplied for gnatcheck
      --  Warnings rule.

      for J in Param'Range loop
         if Param (J) in 'e' | 's'
           and then (J = Param'First or else Param (J - 1) not in '.' | '_')
         then
            Instance.Error
              ("Warnings rule cannot have "
               & Param (J)
               & " parameter, parameter string "
               & Param
               & " ignored");
            Bad_Rule_Detected := True;

            return;
         end if;
      end loop;

      --  If the current instance has an alias name, add it to the tags map
      I := Param'First;
      while I <= Param'Last loop
         C := Param (I);
         J := I;

         --  If the parameter starts by "_" or ".", it has two characters
         if C in '.' | '_' then
            J := J + 1;
         end if;

         Warning_To_Instance.Insert (Param (I .. J), Name, Cursor, Success);
         if not Success and then Warning_To_Instance (Cursor) /= Name then
            Instance.Error
              ("cannot enable the same warning in different rule instances: "
               & Param (I .. J));
            Bad_Rule_Detected := True;
            return;
         end if;

         I := J + 1;
      end loop;

      Use_gnatw_Option := True;
      Free (Warning_Options_String);
      Warning_Options_String := new String'(New_Options);
   end Process_Warning_Param;

   ----------------------
   -- Disable_Warnings --
   ----------------------

   procedure Disable_Warnings is
   begin
      Use_gnatw_Option := False;
      Free (Warning_Options_String);
      Warning_Options_String := new String'("-gnatw");
   end Disable_Warnings;

   --------------------------------
   -- Restriction_Rule_parameter --
   ---------------------------------

   function Restriction_Rule_Parameter (Diag : String) return String is
      R_Name_Start : Natural := 0;
      R_Name_End   : Natural;
      Par_End      : Natural;
      Sep_Idx      : Natural;
      R_Id         : Restriction_Id := Not_A_Restriction_Id;
      Special_R_Id : Special_Restriction_Id := Not_A_Special_Restriction_Id;

   begin
      --  Get the position of the restriction name in the diagnostic
      R_Name_Start := Index (Diag, "of restriction ");
      if R_Name_Start /= 0 then
         R_Name_Start := @ + 16;
         R_Name_End := Index (Diag (R_Name_Start .. Diag'Last), """") - 1;
         Sep_Idx := Index (Diag (R_Name_Start .. Diag'Last), "=");

         if Sep_Idx /= 0 then
            Par_End := R_Name_End;
            R_Name_End := Sep_Idx - 1;
            while R_Name_End > R_Name_Start and then Diag (R_Name_End) = ' '
            loop
               R_Name_End := @ - 1;
            end loop;
         end if;

      else
         R_Name_Start := Index (Diag, "violates restriction ");
         if R_Name_Start /= 0 then
            R_Name_Start := @ + 21;
            R_Name_End := Index (Diag (R_Name_Start .. Diag'Last), " at");
            if R_Name_End /= 0 then
               R_Name_End := @ - 1;
            else
               R_Name_End := Index (Diag (R_Name_Start .. Diag'Last), " [");
               if R_Name_End /= 0 then
                  R_Name_End := @ - 1;
               else
                  R_Name_End := Diag'Last;
               end if;
            end if;
            Par_End := R_Name_End;
         end if;

      end if;
      pragma Assert (R_Name_Start /= 0);
      pragma Assert (R_Name_End > R_Name_Start);

      Get_Restriction_Id
        (Diag (R_Name_Start .. R_Name_End), R_Id, Special_R_Id);

      if Sep_Idx /= 0
        and then Needs_Parameter_In_Exemption (R_Id, Special_R_Id)
      then
         return To_Lower (Remove_Spaces (Diag (R_Name_Start .. Par_End)));
      else
         return To_Lower (Diag (R_Name_Start .. R_Name_End));
      end if;
   end Restriction_Rule_Parameter;

   --------------------------------
   -- Should_Use_Codepeer_Target --
   --------------------------------

   function Should_Use_Codepeer_Target return Boolean is
      use Ada.Containers;
   begin
      return
        Available_Targets.Length = 1
        and then (Available_Targets.Contains ("gnatsas")
                  or else Available_Targets.Contains ("codepeer"));
   end Should_Use_Codepeer_Target;

   -------------------
   -- GPRbuild_Exec --
   -------------------

   function GPRbuild_Exec return String is
      use Ada.Strings.Unbounded;
   begin
      if Target = "gnatsas" or else Target = "codepeer" then
         return "codepeer-gprbuild";
      else
         return "gprbuild";
      end if;
   end GPRbuild_Exec;

   -------------------------
   -- Set_Compiler_Checks --
   -------------------------

   procedure Set_Compiler_Checks is
   begin
      --  Check_Restrictions

      for J in Restriction_Setting'Range loop
         if Restriction_Setting (J).Active then
            Check_Restrictions := True;
            exit;
         end if;
      end loop;

      if not Check_Restrictions then
         for J in Special_Restriction_Setting'Range loop
            if Special_Restriction_Setting (J) then
               Check_Restrictions := True;
               exit;
            end if;
         end loop;
      end if;
   end Set_Compiler_Checks;

   ----------------------------
   -- Spawn_Gnatcheck_Worker --
   ----------------------------

   function Spawn_Gnatcheck_Worker
     (Rule_File   : String;
      Msg_File    : String;
      Source_File : String;
      Log_File    : String) return Process_Id
   is
      use GNAT.String_Split;

      Pid           : Process_Id;
      Split_Command : constant Slice_Set := Create (Worker_Name, " ");
      Worker        : String_Access := null;
      Prj           : constant String := Gnatcheck_Prj.Source_Prj;
      CGPR          : constant String := Gnatcheck_Prj.Source_CGPR;
      Args          : Argument_List (1 .. 128);
      Num_Args      : Integer := 0;

      use Ada.Strings.Unbounded;
   begin
      --  Split the worker command into the name of the executable plus its
      --  arguments. We do that because the call to Non_Blocking_Spawn expects
      --  the full path to the executable and the list of arguments as separate
      --  arguments.

      for Arg of Split_Command loop
         if Worker = null then
            Worker := Locate_Exec_On_Path (Arg);
         else
            Num_Args := @ + 1;
            Args (Num_Args) := new String'(Arg);
         end if;
      end loop;

      --  Test if the worker executable exists
      if Worker = null then
         Error
           ("cannot locate the worker executable: " & Base_Name (Worker_Name));
         raise Fatal_Error;
      end if;

      if Arg.Show_Instantiation_Chain.Get then
         Num_Args := @ + 1;
         Args (Num_Args) := new String'("--show-instantiation-chain");
      end if;

      if Prj /= "" then
         Num_Args := @ + 1;
         Args (Num_Args) := new String'("-P" & Prj);

         if Arg.Ignore_Project_Switches then
            Num_Args := @ + 1;
            Args (Num_Args) := new String'("--ignore-project-switches");
         end if;
      end if;

      if Arg.Aggregated_Project then
         Num_Args := @ + 1;
         Args (Num_Args) := new String'("-A");
         Num_Args := @ + 1;
         Args (Num_Args) := new String'(Get_Aggregated_Project);
      end if;

      if CGPR = "" then
         if RTS_Path /= Null_Unbounded_String then
            Num_Args := @ + 1;
            Args (Num_Args) := new String'("--RTS=" & To_String (RTS_Path));
         end if;

         if Target /= Null_Unbounded_String then
            Num_Args := @ + 1;
            Args (Num_Args) := new String'("--target=" & To_String (Target));
         end if;
      else
         --  Target and runtime will be taken from config project anyway
         Num_Args := @ + 1;
         Args (Num_Args) := new String'("--config=" & CGPR);
      end if;

      if Arg.Debug_Mode.Get then
         Num_Args := @ + 1;
         Args (Num_Args) := new String'("-d");
      end if;

      Num_Args := @ + 1;
      Args (Num_Args) := new String'("--log-file=" & Log_File);

      if Arg.Follow_Symbolic_Links.Get then
         Num_Args := @ + 1;
         Args (Num_Args) := new String'("-eL");
      end if;

      for Dir of Arg.Rules_Dirs.Get loop
         Num_Args := @ + 1;
         Args (Num_Args) := new String'("--rules-dir=" & To_String (Dir));
      end loop;

      Num_Args := @ + 1;
      Args (Num_Args) := new String'("--files-from=" & Source_File);

      Append_Variables (Args, Num_Args);

      Num_Args := @ + 1;
      Args (Num_Args) := new String'("--rules-from=" & Rule_File);

      if Arg.Debug_Mode.Get then
         --  For debug purposes, we don't want to put the full path to the
         --  worker command, if it is a full path. We just want the base name
         Put (Base_Name (Worker.all));

         for J in 1 .. Num_Args loop
            Put (" " & Args (J).all);
         end loop;

         New_Line;
      end if;

      Pid :=
        Non_Blocking_Spawn
          (Worker.all, Args (1 .. Num_Args), Msg_File, Msg_File);
      Free (Worker);

      for J in Args'Range loop
         Free (Args (J));
      end loop;

      return Pid;
   end Spawn_Gnatcheck_Worker;

   -----------------------------------
   -- Spawn_LKQL_Rule_Config_Parser --
   -----------------------------------

   function Spawn_LKQL_Rule_File_Parser
     (LKQL_RF_Name : String; Result_File : String) return Process_Id
   is
      use GNAT.String_Split;

      Pid           : Process_Id;
      Split_Command : constant Slice_Set := Create (Worker_Name, " ");
      Worker        : String_Access := null;
      Args          : Argument_List (1 .. 128);
      Num_Args      : Integer := 0;
   begin
      --  Call the GNATcheck worker with the '--parse-lkql-config' option to
      --  get the rule configuration from the provided rule file.
      for Arg of Split_Command loop
         if Worker = null then
            Worker := Locate_Exec_On_Path (Arg);
         else
            Num_Args := @ + 1;
            Args (Num_Args) := new String'(Arg);
         end if;
      end loop;

      if Worker = null then
         Error
           ("cannot locate the worker executable: " & Base_Name (Worker_Name));
         raise Fatal_Error;
      end if;

      Num_Args := @ + 1;
      Args (Num_Args) := new String'("--parse-lkql-config");
      Num_Args := @ + 1;
      Args (Num_Args) := new String'(LKQL_RF_Name);

      --  Output the called command if in debug mode
      if Arg.Debug_Mode.Get then
         Put (Base_Name (Worker.all));
         for J in 1 .. Num_Args loop
            Put (" " & Args (J).all);
         end loop;
         New_Line;
      end if;

      --  Spawn the process and return the associated process ID
      Pid :=
        Non_Blocking_Spawn (Worker.all, Args (1 .. Num_Args), Result_File);

      for J in Args'Range loop
         Free (Args (J));
      end loop;

      return Pid;
   end Spawn_LKQL_Rule_File_Parser;

   --------------------
   -- Spawn_GPRbuild --
   --------------------

   function Spawn_GPRbuild (Output_File : String) return Process_Id is
      Pid         : Process_Id;
      GPRbuild    : String_Access := Locate_Exec_On_Path (GPRbuild_Exec);
      Prj         : constant String := Gnatcheck_Prj.Source_Prj;
      Last_Source : constant SF_Id := Last_Argument_Source;
      Args        : Argument_List (1 .. 128 + Integer (Last_Source));
      Num_Args    : Integer := 0;

      use Ada.Strings.Unbounded;
   begin
      if GPRbuild = null then
         Error ("cannot locate gprbuild executable");
         raise Fatal_Error;
      end if;

      Args (1) := new String'("-c");
      Args (2) := new String'("-s");
      Args (3) := new String'("-k");
      Args (4) := new String'("-q");
      Args (5) := new String'("--subdirs=" & Subdir_Name);

      Args (6) := new String'("--no-object-check");
      Args (7) := new String'("--complete-output");
      Args (8) := new String'("--restricted-to-languages=ada");
      Num_Args := 8;

      if Target /= Null_Unbounded_String then
         Num_Args := @ + 1;
         Args (Num_Args) := new String'("--target=" & To_String (Target));
      end if;

      if Arg.Jobs.Get > 1 then
         Num_Args := @ + 1;
         Args (Num_Args) := new String'("-j" & Image (Arg.Jobs.Get));
      end if;

      if Prj /= "" then
         Num_Args := @ + 1;
         Args (Num_Args) := new String'("-P" & Prj);
      end if;

      if Arg.Follow_Symbolic_Links.Get then
         Num_Args := @ + 1;
         Args (Num_Args) := new String'("-eL");
      end if;

      --  If files are specified explicitly, only compile these files

      if (Argument_File_Specified and then not Arg.Transitive_Closure.Get)
        or else File_List_Specified
      then
         Num_Args := @ + 1;
         Args (Num_Args) := new String'("-u");

         for SF in First_SF_Id .. Last_Source loop
            Num_Args := @ + 1;
            Args (Num_Args) := new String'(Short_Source_Name (SF));
         end loop;
      else
         if Arg.Transitive_Closure.Get then
            Num_Args := @ + 1;
            Args (Num_Args) := new String'("-U");
         end if;

         if not Main_Unit.Is_Empty then
            for MU of Main_Unit loop
               Num_Args := @ + 1;
               Args (Num_Args) := new String'(String (MU));
            end loop;
         end if;
      end if;

      Append_Variables (Args, Num_Args);

      if Arg.Debug_Mode.Get then
         Put (GPRbuild_Exec);

         for J in 1 .. Num_Args loop
            Put (" " & Args (J).all);
         end loop;

         for S of Compiler_Arg_List.all loop
            Put (" " & S.all);
         end loop;

         New_Line;
      end if;

      Pid :=
        Non_Blocking_Spawn
          (GPRbuild.all,
           Args (1 .. Num_Args) & Compiler_Arg_List.all,
           Output_File & ".out",
           Output_File);
      Free (GPRbuild);

      for J in Args'Range loop
         Free (Args (J));
      end loop;

      return Pid;
   end Spawn_GPRbuild;

   --------------------------
   -- Style_Rule_Parameter --
   --------------------------

   function Style_Rule_Parameter (Diag : String) return String is
      First_Idx        : Natural;
      String_To_Search : constant String :=
        (if Arg.Show_Rule.Get then "style_checks:" else "[-gnaty");

   begin
      --  This function returns non-empty result only if .d parameter is
      --  specified for Warnings rule or if --show-rule gnatcheck option is
      --  set (that is, if Diag ends with "[style_checks:<option>]"

      First_Idx := Index (Diag, String_To_Search);

      if First_Idx = 0 then
         return "";
      else
         First_Idx := First_Idx + String_To_Search'Length;
         return Diag (First_Idx .. First_Idx);
      end if;
   end Style_Rule_Parameter;

   ----------------------------
   -- Warning_Rule_Parameter --
   ----------------------------

   function Warning_Rule_Parameter (Diag : String) return String is
      First_Idx, Last_Idx : Natural;
      String_To_Search    : constant String :=
        (if Arg.Show_Rule.Get then "warnings:" else "[-gnatw");

   begin
      --  This function returns non-empty result only if .d parameter is
      --  specified for Warnings rule or if --show-rule gnatcheck option is
      --  set (that is, if Diag ends with "[warnings:<option>]"

      First_Idx := Index (Diag, String_To_Search);

      if First_Idx = 0 then
         return "";
      else
         First_Idx := First_Idx + String_To_Search'Length;
         Last_Idx :=
           (if Diag (First_Idx) in '.' | '_'
            then First_Idx + 1
            else First_Idx);
      end if;

      return Diag (First_Idx .. Last_Idx);
   end Warning_Rule_Parameter;

   -----------------------------------
   -- XML_Print_Active_Restrictions --
   -----------------------------------

   procedure XML_Print_Active_Restrictions (Indent_Level : Natural := 0) is
   begin
      XML_Report ("<rule id=""Restrictions"">", Indent_Level);

      for R in Restriction_Setting'Range loop

         if Restriction_Setting (R).Active then
            if R in All_Boolean_Restrictions then
               XML_Report
                 ("<parameter>" & To_Mixed (R'Img) & "</parameter>",
                  Indent_Level + 1);
            else
               for J in Restriction_Setting (R).Param.Iterate loop
                  XML_Report
                    ("<parameter>"
                     & To_Mixed (R'Img)
                     & "=>"
                     & Restriction_Setting (R).Param (J)
                     & "</parameter>",
                     Indent_Level + 1);
               end loop;
            end if;
         end if;
      end loop;

      for R in Special_Restriction_Setting'Range loop
         if Special_Restriction_Setting (R) then
            case R is
               when No_Dependence =>
                  Forbidden_Units_Dictionary.Reset_Iterator;

                  while not Forbidden_Units_Dictionary.Done loop
                     XML_Report
                       ("<parameter>No_Dependence=>"
                        & Forbidden_Units_Dictionary.Next_Entry
                        & "</parameter>",
                        Indent_Level + 1);
                  end loop;

               when No_Use_Of_Entity =>
                  Forbidden_Entities_Dictionary.Reset_Iterator;

                  while not Forbidden_Entities_Dictionary.Done loop
                     XML_Report
                       ("<parameter>No_Use_Of_Entity=>"
                        & Forbidden_Entities_Dictionary.Next_Entry
                        & "</parameter>",
                        Indent_Level + 1);
                  end loop;

               when No_Specification_Of_Aspect =>
                  Forbidden_Aspects_Dictionary.Reset_Iterator;

                  while not Forbidden_Aspects_Dictionary.Done loop
                     XML_Report
                       ("<parameter>No_Specification_Of_Aspect=>"
                        & Forbidden_Aspects_Dictionary.Next_Entry
                        & "</parameter>",
                        Indent_Level + 1);
                  end loop;
            end case;
         end if;
      end loop;

      XML_Report ("</rule>", Indent_Level);
   end XML_Print_Active_Restrictions;

end Gnatcheck.Compiler;

------------------------------------------------------------------------------
--                                                                          --
--                                 GNATCHECK                                --
--                                                                          --
--                     Copyright (C) 2005-2021, AdaCore                     --
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
with Ada.Command_Line;        use Ada.Command_Line;
with Ada.Strings;             use Ada.Strings;
with Ada.Strings.Fixed;       use Ada.Strings.Fixed;

with System.Rident;

with GNAT.Case_Util;

with Gnatcheck.Options;          use Gnatcheck.Options;
with Gnatcheck.Output;           use Gnatcheck.Output;
with Gnatcheck.Projects;         use Gnatcheck.Projects;
with Gnatcheck.Source_Table;     use Gnatcheck.Source_Table;
with Gnatcheck.String_Utilities; use Gnatcheck.String_Utilities;
with Gnatcheck.Rules;            use Gnatcheck.Rules;
with Gnatcheck.Rules.Rule_Table; use Gnatcheck.Rules.Rule_Table;

with Gnatcheck.Diagnoses;        use Gnatcheck.Diagnoses;
with Gnatcheck.Ids;              use Gnatcheck.Ids;

package body Gnatcheck.Compiler is

   Style_Options_String : String_Access := new String'("-gnaty");
   --  Stores parameters of the Style_Checks

   Warning_Options_String : String_Access := new String'("-gnatw");
   --  Stores parameters of the Warnings rule

   type Message_Kinds is
     (Not_A_Message,
      Warning,
      Style,
      Restriction,
      Error);

   function To_Mixed (A : String) return String
     renames GNAT.Case_Util.To_Mixed;

   procedure Process_Style_Options (Param : String);
   --  Stores Param as parameter of the compiler -gnaty... option as is,
   --  (if some -gnaty... parameter has already been stored, appends Param to
   --  it.)

   function Adjust_Message
     (Diag         : String;
      Message_Kind : Message_Kinds) return String;
   --  Does the following adjustments:
   --
   --  * Remove from the diagnostic message the reference to the configuration
   --    file with restriction pragmas that is created by gnatcheck.
   --
   --  * If Gnatcheck.Options.Mapping_Mode is ON, annotates the message by
   --    adding the compiler check (if for a warning message '.d' is specified,
   --    the trailing part that indicates the warning message that causes this
   --    warning is removed from the diagnosis, and the corresponding warning
   --    parameter is added to the annotation.

   function Annotation
     (Message_Kind : Message_Kinds;
      Parameter    : String)
      return         String;
   --  Returns annotation to be added to the compiler diagnostic message if
   --  Gnatcheck.Options.Mapping_Mode is ON. Parameter, if non-empty, is the
   --  parameter of '-gnatw' option that causes the diagnosis

   function Get_Rule_Id (Check : Message_Kinds) return Rule_Id;
   --  Returns the Id corresponding to the given compiler check

   ---------------------------------------------------------
   -- Data structures and routines for restriction checks --
   ---------------------------------------------------------

   subtype Option_Parameter is Natural;

   package Gnatcheck_Restrictions is new System.Rident;
   use Gnatcheck_Restrictions;
   --  We cannot use the instantiation of System.Rident in System.Restrictions
   --  because of the pragma Discard_Names that does not allow to use
   --  Restriction_Id'Value when analyzing gnatcheck restriction parameters.

   type Restriction_State is record
      Active : Boolean;
      Param  : String_List_Access;
   end record;
   --  We can not use Option_Parameter here, because some restrictions (e.g.
   --  Max_Task_Entries) may be active and may have zero parameter

   Restriction_Setting : array (All_Restrictions) of Restriction_State :=
     [others => (False, null)];
   --  This array represents only restrictions that are values of
   --  System.Rident.Restriction_Id. But we need to process restrictions that
   --  are not included in values of this type.

   type Special_Restriction_Id is
     (Not_A_Special_Restriction_Id, No_Dependence);

   subtype All_Special_Restrictions is Special_Restriction_Id range
     No_Dependence .. No_Dependence;
   --  All special restrictions, excluding Not_A_Special_Restriction_Id.

   subtype All_Special_Parameter_Restrictions is Special_Restriction_Id range
     No_Dependence .. No_Dependence;
   --  All special restrictions that have a parameter

   function Needs_Parameter_In_Exemption
     (R    : Restriction_Id;
      SR   : Special_Restriction_Id) return Boolean;
   --  Checks if R or SR denotes a restriction that needs a restriction
   --  parameter if used in parametric rule exemption (such as
   --  'No_Dependence => Foo).

   Special_Restriction_Setting : array (All_Special_Restrictions)
     of Boolean := [others => False];
   --  This array only indicates if a given special restriction is ON or OFF,
   --  we cannot store any restriction parameter information, because
   --  parameter format is restriction-specific

   package Forbidden_Units_Dictionary is new Simple_String_Dictionary
     (Dictionary_Name => "Forbidden units dictionary");

   --------------------
   -- Adjust_Message --
   --------------------

   function Adjust_Message
     (Diag         : String;
      Message_Kind : Message_Kinds) return String
    is
      Result    : constant String (1 .. Diag'Length) := Diag;
      Last_Idx  : Natural;
      Diag_End  : Natural;
      Par_Start : Natural := 1;
      Par_End   : Natural := 0;

   begin
      Last_Idx := Index (Result, Gnatcheck_Config_File.all);

      if Last_Idx = 0 then
         Last_Idx := Result'Last;
      else
         Last_Idx := Last_Idx - 5;
      end if;

      if Mapping_Mode then
         if Message_Kind = Warning then
            Diag_End := Index (Source  => Result (1 .. Last_Idx),
                               Pattern => "[-gnatw",
                               Going   => Backward);

            if Diag_End = 0 then
               Diag_End := Last_Idx;
            else
               Par_Start := Diag_End + 7;
               Par_End   := Par_Start;

               if Result (Par_End) = '.' then
                  Par_End := Par_End + 1;
               end if;

               Diag_End := Diag_End - 2;
            end if;
         else
            Diag_End := Last_Idx;
         end if;

         return Result (1 .. Diag_End) &
                Annotation (Message_Kind, Result (Par_Start .. Par_End));

      else
         return Result (1 .. Last_Idx);
      end if;
   end Adjust_Message;

   ----------------------------
   -- Analyze_Builder_Output --
   ----------------------------

   procedure Analyze_Output (File_Name : String; Errors : out Boolean) is
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
         SF      : SF_Id;
         Discard : Natural;

         Message_Kind : Message_Kinds := Not_A_Message;

         First_Idx : constant Natural := Msg'First;
         Idx       : Natural := First_Idx;
         Word_End  : Natural  := 0;
         Kind      : Diagnosis_Kinds := Rule_Violation;

         procedure Format_Error;
         --  Emit an error about an unexpected format encountered and set
         --  Errors to True.

         ------------------
         -- Format_Error --
         ------------------

         procedure Format_Error is
         begin
            Error ("Unexpected format of message:");
            Error_No_Tool_Name (Msg);
            Errors := True;
         end Format_Error;

      begin
         if Msg'Last = 0 then
            --  An empty line?
            return;
         end if;

         --  We assume the following format of the message:
         --
         --   filename:line:column: <message body>
         --
         --  If -dJ is set, <message body> has the following structure
         --
         --    [warning] scopename: line:col: text
         --
         --  So the first thing we have to do is to skip 3 colons and to define
         --  the source the message is siiued for, and the line and column
         --  numbers:

         Idx := Index (Msg (Idx .. Msg'Last), ":");

         if Idx = 0 then
            Format_Error;
            return;
         end if;

         SF := File_Find (Msg (First_Idx .. Idx - 1), Use_Short_Name => True);

         if not Is_Argument_Source (SF) then
            --  This source is not an argument of this check
            return;
         end if;

         Word_End := Index (Msg (Idx + 1 .. Msg'Last), ":");

         if Word_End = 0 then
            Format_Error;
            return;
         end if;

         begin
            Discard := Positive'Value (Msg (Idx + 1 .. Word_End - 1));
         exception
            when others =>
               Format_Error;
               return;
         end;

         Idx := Word_End;
         Word_End := Index (Msg (Idx + 1 .. Msg'Last), ":");

         if Word_End = 0 then
            Format_Error;
            return;
         end if;

         begin
            Discard := Positive'Value (Msg (Idx + 1 .. Word_End - 1));
         exception
            when others =>
               Format_Error;
               return;
         end;

         Idx := Word_End + 2;

         --  A gnatcheck message emitted by a child process via --subprocess

         if Msg (Idx .. Idx + 6) = "check: " then
            if Msg (Msg'Last) /= ']' then
               Format_Error;
               return;
            end if;

            declare
               Last : constant Natural :=
                 Index (Source  => Msg (Msg'First .. Msg'Last - 1),
                        Pattern => "[",
                        Going   => Backward);
               Id    : Rule_Id;

            begin
               if Last = 0 then
                  Format_Error;
                  return;
               end if;

               Id := Get_Rule (Msg (Last + 1 .. Msg'Last - 1));
               Store_Diagnosis
                 (Text           => Msg (Msg'First .. Idx - 1) &
                                    Msg (Idx + 7 .. Last - 2) &
                                    Annotate_Rule (All_Rules.Table (Id).all),
                  Diagnosis_Kind =>
                    (if Last - Idx > 24
                       and then Msg (Idx + 7 .. Idx + 23) = "internal error at"
                     then Internal_Error else Rule_Violation),
                  SF             => SF,
                  Rule           => Id);
               return;
            end;
         elsif Msg (Idx .. Idx + 6) = "error: " then
            Message_Kind := Error;
            Errors := True;
            Kind   := Compiler_Error;

         elsif Msg (Idx .. Idx + 8) = "warning: " then
            if Index (Msg (Idx .. Msg'Last), ": violation of restriction") /= 0
            then
               Message_Kind := Restriction;
            else
               Message_Kind := Warning;
            end if;
         elsif Index (Msg (Idx .. Msg'Last), "(style)") /= 0 then
            Message_Kind := Style;
         else
            Format_Error;
            return;
         end if;

         if Message_Kind = Restriction
           and then Index (Msg, Gnatcheck_Config_File.all) = 0
         then
            --  This means that the diagnoses correspond to some pragma that
            --  is not from the configuration file created from rule
            --  options, so we should not file it.

            return;
         end if;

         Store_Diagnosis
           (Text           => Adjust_Message (Msg, Message_Kind),
            Diagnosis_Kind => Kind,
            SF             => SF,
            Rule           => (if Message_Kind = Error then No_Rule
                               else Get_Rule_Id (Message_Kind)));
      end Analyze_Line;

      Dump_Stdout : Boolean := True;

   --  Start of processing for Analyze_Builder_Output

   begin
      Errors := False;
      Open (File => File, Mode => In_File, Name => File_Name);

      while not End_Of_File (File) loop
         Get_Line (File, Line, Line_Len);

         if (Line_Len >= 24
             and then Line (1 .. 24) = "gnat1: bad -gnaty switch")
           or else (Line_Len > 29
                    and then Line (1 .. 29) = "gnat1: invalid switch: -gnatw")
         then
            Error ("wrong parameter specified for compiler-related rule:");
            Error_No_Tool_Name (Line (1 .. Line_Len));
            Errors := True;

         elsif Index (Line (1 .. Line_Len), ".gpr:") /= 0
           or else Index (Line (1 .. Line_Len), "gprbuild: ") /= 0
         then
            Error ("error when calling gprbuild:");

            if Is_Regular_File (File_Name & ".out") and then Dump_Stdout then
               declare
                  Str : String_Access := Read_File (File_Name & ".out");
               begin
                  Error_No_Tool_Name (Str (Str'First .. Str'Last - 1));
                  Free (Str);
                  Dump_Stdout := False;
               end;
            end if;

            Error_No_Tool_Name (Line (1 .. Line_Len));
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
                      (Line (13 .. Index_Non_Blank
                                     (Line, Line'Last - 1, Backward)),
                       Use_Short_Name => True);

               begin
                  if Is_Argument_Source (SF) then
                     Errors := True;
                     Store_Diagnosis
                       (Text           =>
                          Adjust_Message
                            ((if Full_Source_Locations
                              then Source_Name (SF)
                              else Short_Source_Name (SF)) &
                             ":1:1: fatal compiler error", Error),
                        Diagnosis_Kind => Compiler_Error,
                        SF             => SF);
                  end if;
               end;
            end if;
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
     (Message_Kind : Message_Kinds;
      Parameter    : String) return String is
   begin
      case Message_Kind is
         when Not_A_Message =>
            pragma Assert (False);
            return "";
         when Warning =>
            return " [Warnings" &
                   (if Parameter = "" then "" else ":" & Parameter) & "]";
         when Style =>
            return " [Style_Checks]";
         when Restriction =>
            return " [Restrictions]";
         when Error =>
            return " [Errors]";
      end case;
   end Annotation;

   -------------------------------------
   -- Create_Restriction_Pragmas_File --
   -------------------------------------

   procedure Create_Restriction_Pragmas_File is
      RPF : File_Type;
   begin
      Create (File => RPF,
              Mode => Out_File,
              Name => Gnatcheck_Config_File.all);

      Put_Line (RPF, "pragma Warnings (Off, ""[enabled by default]"");");

      for R in All_Restrictions loop
         if Restriction_Setting (R).Active then
            if R in All_Boolean_Restrictions then
               Put_Line (RPF, "pragma Restriction_Warnings (" & R'Img & ");");
            else
               for J in Restriction_Setting (R).Param'Range loop
                  Put (RPF, "pragma Restriction_Warnings (");
                  Put (RPF, R'Img);
                  Put (RPF, " =>"  & Restriction_Setting (R).Param (J).all);
                  Put_Line (RPF, ");");
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
                     Put
                       (RPF, "pragma Restriction_Warnings (No_Dependence => ");
                     Put_Line
                       (RPF, Forbidden_Units_Dictionary.Next_Entry & ");");
                  end loop;
            end case;
         end if;
      end loop;

      Close (RPF);
   end Create_Restriction_Pragmas_File;

   -----------------
   -- Get_Rule_Id --
   -----------------

   function Get_Rule_Id (Check : Message_Kinds) return Rule_Id is
   begin
      case Check is
         when Not_A_Message | Error =>
            pragma Assert (False);
            return No_Rule;
         when Warning =>
            return Warnings_Id;
         when Style =>
            return Style_Checks_Id;
         when Restriction =>
            return Restrictions_Id;
      end case;
   end Get_Rule_Id;

   ----------------------------------
   -- Get_Specified_Warning_Option --
   ----------------------------------

   function Get_Specified_Warning_Option return String is
   begin
      return Warning_Options_String (7 .. Warning_Options_String'Last);
   end Get_Specified_Warning_Option;

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

   ----------------------------------
   -- Needs_Parameter_In_Exemption --
   ----------------------------------

   function Needs_Parameter_In_Exemption
     (R    : Restriction_Id;
      SR   : Special_Restriction_Id)
      return Boolean
   is
      Result : Boolean := False;
   begin
      if SR in All_Special_Parameter_Restrictions then
         Result := True;
      elsif R in All_Parameter_Restrictions then
         --  Not all the restrictions from All_Parameter_Restrictions require
         --  restriction parameter in parametric exemptions
         Result := R not in Integer_Parameter_Restrictions;
      end if;

      return Result;
   end Needs_Parameter_In_Exemption;

   ----------------------------------
   -- Is_Restriction_Exemption_Par --
   ----------------------------------

   function Is_Restriction_Exemption_Par (Par : String) return Boolean is
      Result        :          Boolean  := False;
      Arrow_Idx     : constant Natural  := Index (Par, "=>");
      Rest_Name_End :          Natural  := Par'Last;
      Par_Start     : constant Positive := Par'First;
      R_Id          :          Restriction_Id         := Not_A_Restriction_Id;
      Special_R_Id  :          Special_Restriction_Id :=
        Not_A_Special_Restriction_Id;

   begin
      if Arrow_Idx /= 0 then
         Rest_Name_End := Arrow_Idx - 1;

         while Rest_Name_End >= Par_Start and then Par (Par_Start) = ' ' loop
            Rest_Name_End := Rest_Name_End - 1;
         end loop;
      end if;

      begin
         R_Id := Restriction_Id'Value (Par (Par_Start .. Rest_Name_End));
      exception
         when Constraint_Error =>
            R_Id := Not_A_Restriction_Id;
      end;

      if R_Id = Not_A_Restriction_Id then
         begin
            Special_R_Id :=
              Special_Restriction_Id'Value (Par (Par_Start .. Rest_Name_End));
         exception
            when Constraint_Error =>
               Special_R_Id := Not_A_Special_Restriction_Id;
         end;

      end if;

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

   ------------------------------
   -- Is_Warning_Exemption_Par --
   ------------------------------

   function Is_Warning_Exemption_Par (Par : String) return Boolean is
      Last_Idx : constant Positive := Par'Last;
      Result   :          Boolean  := True;
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

   -------------------------------
   -- Print_Active_Restrictions --
   -------------------------------

   procedure Print_Active_Restrictions (Ident_Level : Natural := 0) is
      Bool_Tmp : Boolean := True;
   begin
      for R in Restriction_Setting'Range loop
         if Restriction_Setting (R).Active then
            if R in All_Boolean_Restrictions then
               Report (To_Mixed (R'Img), Ident_Level);
            else
               for J in Restriction_Setting (R).Param'Range loop
                  Report_No_EOL (To_Mixed (R'Img), Ident_Level);
                  Report (" =>"  & Restriction_Setting (R).Param (J).all);
               end loop;
            end if;
         end if;
      end loop;

      for R in Special_Restriction_Setting'Range loop
         if Special_Restriction_Setting (R) then
            Report_No_EOL (To_Mixed (R'Img), Ident_Level);

            case R is
               when No_Dependence =>
                  Report_No_EOL (" => ");

                  Forbidden_Units_Dictionary.Reset_Iterator;

                  while not Forbidden_Units_Dictionary.Done loop
                     if Bool_Tmp then
                        Report (Forbidden_Units_Dictionary.Next_Entry);
                        Bool_Tmp := False;
                     else
                        Report
                          ("No_Dependence => " &
                           Forbidden_Units_Dictionary.Next_Entry,
                           Ident_Level);
                     end if;
                  end loop;
            end case;
         end if;
      end loop;
   end Print_Active_Restrictions;

   ---------------------------------------
   -- Print_Active_Restrictions_To_File --
   ---------------------------------------

   procedure Print_Active_Restrictions_To_File (Rule_File : File_Type) is
   begin
      for R in Restriction_Setting'Range loop
         if Restriction_Setting (R).Active then
            if R in All_Boolean_Restrictions then
               Put_Line (Rule_File, "+RRestrictions : " & To_Mixed (R'Img));
            else
               for J in Restriction_Setting (R).Param'Range loop
                  Put (Rule_File, "+RRestrictions : " & To_Mixed (R'Img));
                  Put_Line (Rule_File,
                            " =>"  & Restriction_Setting (R).Param (J).all);
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
                     Put      (Rule_File, "+RRestrictions : ");
                     Put      (Rule_File, To_Mixed (R'Img) & " => ");
                     Put_Line (Rule_File,
                               Forbidden_Units_Dictionary.Next_Entry);
                  end loop;
            end case;
         end if;
      end loop;
   end Print_Active_Restrictions_To_File;

   -------------------------------
   -- Process_Restriction_Param --
   -------------------------------

   procedure Process_Restriction_Param
     (Parameter : String;
      Enable    : Boolean)
   is
      Param        : constant String  := Trim (Parameter, Both);
      First_Idx    : constant Natural := Param'First;
      Last_Idx     :          Natural := Param'Last;
      Arg_Present  :          Boolean := False;
      R_Id         :          Restriction_Id;
      Special_R_Id :          Special_Restriction_Id;
      R_Val        :          Option_Parameter;

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

      begin
         R_Id := Restriction_Id'Value (Param (First_Idx .. Last_Idx));
      exception
         when Constraint_Error =>
            R_Id := Not_A_Restriction_Id;
      end;

      if R_Id = Not_A_Restriction_Id then
         begin
            Special_R_Id :=
              Special_Restriction_Id'Value (Param (First_Idx .. Last_Idx));
         exception
            when Constraint_Error =>
               Special_R_Id := Not_A_Special_Restriction_Id;
         end;

      end if;

      if R_Id = Not_A_Restriction_Id
        and then
         Special_R_Id = Not_A_Special_Restriction_Id
      then
         Error ("wrong restriction identifier : " &
                 Param (First_Idx .. Last_Idx) & ", ignored");
         return;
      end if;

      --  Check if we have a restriction_parameter_argument, and if we do,
      --  set First_Idx to the first character after '=>'

      for J in Last_Idx + 1 .. Param'Last - 2 loop
         if Param (J) = '=' then
            if J <= Param'Last - 2 and then Param (J + 1) = '>' then
               Arg_Present := True;
               Last_Idx := J + 2;
               exit;

            else
               Error ("wrong structure of restriction rule parameter " &
                      Param & ", ignored");
               return;
            end if;
         end if;
      end loop;

      if not Enable then

         if R_Id in All_Restrictions then
            Restriction_Setting (R_Id).Active := False;
         else
            Special_Restriction_Setting (Special_R_Id) := False;
            --  We may need to correct stored parameters of some restrictions

            if Arg_Present then
               case Special_R_Id is
                  when No_Dependence =>
                     Forbidden_Units_Dictionary.Remove_From_Dictionary
                       (Trim (Param (Last_Idx .. Param'Last), Both));

                  when others =>
                     null;
               end case;
            end if;
         end if;

         return;
      end if;

      if R_Id in All_Boolean_Restrictions then

         if Arg_Present then
            Error ("RESTRICTIONS rule parameter: " & Param &
                   " can not contain expression, ignored");
         else
            Restriction_Setting (R_Id).Active := Enable;
         end if;

      elsif R_Id /= Not_A_Restriction_Id then

         if not Arg_Present then
            Error ("RESTRICTIONS rule parameter: " & Param &
                    " should contain an expression, ignored");
            return;
         else
            if R_Id in Integer_Parameter_Restrictions then
               begin
                  R_Val :=
                    Option_Parameter'Value
                      (Trim (Param (Last_Idx .. Param'Last), Both));

                  if Restriction_Setting (R_Id).Param /= null
                    and then
                     Gnatcheck.Options.Check_Param_Redefinition
                  then
                     Free (Restriction_Setting (R_Id).Param);
                     Last_Idx := Index (Param, "=", Backward) - 1;

                     for J in reverse First_Idx .. Last_Idx loop
                        if Param (J) /= ' ' then
                           Last_Idx := J;
                           exit;
                        end if;
                     end loop;

                     Error ("expression for RESTRICTIONS rule parameter: " &
                            Param (First_Idx .. Last_Idx) &
                            " is specified more than once");
                  end if;

                  Restriction_Setting (R_Id).Param  :=
                    new String_List'(1 => new String'(R_Val'Img));
               exception
                  when Constraint_Error =>
                     Error ("wrong restriction parameter expression in " &
                             Param & ", ignored");
                  return;
               end;
            else
               --  No check is made for the moment for non-integer restriction
               --  parameters:

               if Restriction_Setting (R_Id).Param = null then
                  Restriction_Setting (R_Id).Param  :=
                    new String_List'(1 => new String'
                      (Trim (Param (Last_Idx .. Param'Last), Both)));

               else
                  declare
                     Tmp : constant String_List :=
                       Restriction_Setting (R_Id).Param.all &
                       new String'(Trim
                         (Param (Last_Idx .. Param'Last), Both));
                  begin
                     Restriction_Setting (R_Id).Param :=
                       new String_List'(Tmp);
                  end;
               end if;
            end if;
         end if;

         Restriction_Setting (R_Id).Active := Enable;

      else
         --  If we are here, R_Id = Not_A_Restriction_Id, therefore
         --  Special_R_Id /= Not_A_Special_Restriction_Id

         case Special_R_Id is
            when No_Dependence =>

               if not Arg_Present then
                  Error ("RESTRICTIONS rule parameter: " & Param &
                          " should contain an unit name, ignored");
                  return;
               end if;

               Special_Restriction_Setting (Special_R_Id) := True;
               Forbidden_Units_Dictionary.Add_To_Dictionary
                 (Trim (Param (Last_Idx .. Param'Last), Both));

            when Not_A_Special_Restriction_Id =>
               null;
               pragma Assert (False);
         end case;
      end if;

      --  Check if a warning about restrictions that should be checked at
      --  compile time instead of coding standard should be issued (these
      --  restrictions need information that does not exist in the trees
      --  created for ASIS)

      if R_Id in No_Implicit_Dynamic_Code                 |
                 No_Elaboration_Code                      |
                 No_Implicit_Heap_Allocations             |
                 No_Implicit_Task_Allocations             |
                 No_Implicit_Protected_Object_Allocations |
                 No_Secondary_Stack                       |
                 No_Implicit_Conditionals                 |
                 No_Implicit_Loops                        |
                 No_Default_Initialization                |
                 Static_Dispatch_Tables                   |
                 No_Exception_Propagation
      then
         Warning ("restriction " & To_Mixed (R_Id'Img) &
                  " ignored - only fully effective during code generation");

         Restriction_Setting (R_Id).Active := False;
      end if;

      --  Check if a warning about (potentially) statically uncheckable
      --  restriction should be issued

      if R_Id in No_Standard_Allocators_After_Elaboration |
                 Max_Entry_Queue_Length                   |
                 Max_Storage_At_Blocking                  |
                 Max_Tasks                                |
                 No_Task_Termination                      |
                 No_Entry_Queue                           |
                 No_Reentrancy
      then
         Warning ("restriction " & To_Mixed (R_Id'Img) &
                  " ignored - cannot be checked statically");

         Restriction_Setting (R_Id).Active := False;

      elsif R_Id = No_Recursion then
         Warning ("restriction No_Recursion ignored (cannot be checked " &
                  "statically), use rule Recursive_Subprograms instead");

         Restriction_Setting (R_Id).Active := False;

      elsif R_Id = Max_Asynchronous_Select_Nesting and then R_Val /= 0 then
         Warning ("restriction Max_Asynchronous_Select_Nesting ignored - " &
                  "cannot be checked statically if parameter is not 0");
         Restriction_Setting (R_Id).Active := False;
      end if;
   end Process_Restriction_Param;

   -------------------------------
   -- Process_Style_Check_Param --
   -------------------------------

   procedure Process_Style_Check_Param (Param : String) is
   begin
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

   ---------------------------
   -- Process_Warning_Param --
   ---------------------------

   procedure Process_Warning_Param (Param : String) is
      New_Options : constant String := Warning_Options_String.all & Param;
   begin
      --  Checking for 'e' and 's' that should not be supplied for gnatcheck
      --  Warnings rule.

      for J in Param'Range loop
         if Param (J) in 'e' | 's'
           and then (J = Param'First or else Param (J - 1) not in '.' | '_')
         then
            Error ("Warnings rule cannot have " & Param (J) &
                   " parameter, parameter string " & Param & " ignored");
            return;
         end if;
      end loop;

      Use_gnatw_Option := True;
      Free (Warning_Options_String);
      Warning_Options_String := new String'(New_Options);
   end Process_Warning_Param;

   --------------------------------
   -- Restriction_Rule_parameter --
   ---------------------------------

   function Restriction_Rule_Parameter (Diag : String) return String is
      R_Name_Start :          Natural;
      R_Name_End   :          Natural;
      Par_End      :          Natural;
      Arr_Idx      :          Natural;
      Diag_End     : constant Positive := Diag'Last;
      R_Id          :          Restriction_Id         := Not_A_Restriction_Id;
      Special_R_Id  :          Special_Restriction_Id :=
        Not_A_Special_Restriction_Id;

   begin
      R_Name_Start := Index (Diag, "of restriction ");
      pragma Assert (R_Name_Start /= 0);

      R_Name_Start := R_Name_Start + 16;

      Arr_Idx := Index (Diag (R_Name_Start .. Diag_End), "=>");

      if Arr_Idx /= 0 then
         R_Name_End := Arr_Idx - 1;

         while R_Name_End > R_Name_Start loop
            exit when Diag (R_Name_End) /= ' ';
            R_Name_End := R_Name_End - 1;
         end loop;
      else
         R_Name_End := Index (Diag (R_Name_Start .. Diag_End), """") - 1;
      end if;

      begin
         R_Id := Restriction_Id'Value (Diag (R_Name_Start .. R_Name_End));
      exception
         when Constraint_Error =>
            R_Id := Not_A_Restriction_Id;
      end;

      if R_Id = Not_A_Restriction_Id then
         begin
            Special_R_Id :=
              Special_Restriction_Id'Value
                (Diag (R_Name_Start .. R_Name_End));
         exception
            when Constraint_Error =>
               Special_R_Id := Not_A_Special_Restriction_Id;
         end;
      end if;

      if Arr_Idx /= 0
        and then Needs_Parameter_In_Exemption (R_Id, Special_R_Id)
      then
         Par_End := Index (Diag (R_Name_Start .. Diag_End), """") - 1;
         return To_Lower (Remove_Spaces (Diag (R_Name_Start .. Par_End)));
      else
         return To_Lower (Diag (R_Name_Start .. R_Name_End));
      end if;
   end Restriction_Rule_Parameter;

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

   ---------------------
   -- Spawn_Gnatcheck --
   ---------------------

   function Spawn_Gnatcheck
     (Rule_File   : String;
      Msg_File    : String;
      Source_File : String) return Process_Id
   is
      Pid       : Process_Id;
      Gnatcheck : String_Access := Locate_Exec_On_Path (Command_Name);
      Prj       : constant String := Gnatcheck_Prj.Source_Prj;
      Args      : Argument_List (1 .. 128);
      Num_Args  : Integer := 0;

   begin
      Args (1) := new String'("--subprocess");
      Num_Args := 1;

      if Prj /= "" then
         Num_Args := @ + 1;
         Args (Num_Args) := new String'("-P" & Prj);
      end if;

      if Debug_Mode then
         Num_Args := @ + 1;
         Args (Num_Args) := new String'("-d");
      end if;

      if Follow_Symbolic_Links then
         Num_Args := @ + 1;
         Args (Num_Args) := new String'("-eL");
      end if;

      if Full_Source_Locations then
         Num_Args := @ + 1;
         Args (Num_Args) := new String'("-l");
      end if;

      if U_Option_Set then
         Num_Args := @ + 1;
         Args (Num_Args) := new String'("-U");
      end if;

      if Main_Unit /= null then
         Num_Args := @ + 1;
         Args (Num_Args) := new String'(Main_Unit.all);
      end if;

      Num_Args := @ + 1;
      Args (Num_Args) := new String'("-files=" & Source_File);

      Append_Variables (Args, Num_Args);

      Num_Args := @ + 1;
      Args (Num_Args) := new String'("-rules");
      Num_Args := @ + 1;
      Args (Num_Args) := new String'("-from=" & Rule_File);

      if Debug_Mode then
         Put (Command_Name);

         for J in 1 .. Num_Args loop
            Put (" " & Args (J).all);
         end loop;

         New_Line;
      end if;

      Pid :=
        Non_Blocking_Spawn
          (Gnatcheck.all, Args (1 .. Num_Args), Msg_File, Msg_File);
      Free (Gnatcheck);

      for J in Args'Range loop
         Free (Args (J));
      end loop;

      return Pid;
   end Spawn_Gnatcheck;

   --------------------
   -- Spawn_GPRbuild --
   --------------------

   function Spawn_GPRbuild (Output_File : String) return Process_Id is
      Pid      : Process_Id;
      GPRbuild : String_Access := Locate_Exec_On_Path ("gprbuild");
      Prj      : constant String := Gnatcheck_Prj.Source_Prj;
      Args     : Argument_List (1 .. 128);
      Num_Args : Integer := 0;

   begin
      Args (1) := new String'("-c");
      Args (2) := new String'("-s");
      Args (3) := new String'("-k");
      Args (4) := new String'("-q");
      Args (5) := new String'("--subdirs=gnatcheck");

      Args (6) := new String'("--no-object-check");
      Args (7) := new String'("--complete-output");
      Args (8) := new String'("--restricted-to-languages=ada");
      Num_Args := 8;

      if Process_Num > 1 then
         Num_Args := @ + 1;
         Args (Num_Args) := new String'("-j" & Image (Process_Num));
      end if;

      if Prj /= "" then
         Num_Args := @ + 1;
         Args (Num_Args) := new String'("-P" & Prj);
      end if;

      if Follow_Symbolic_Links then
         Num_Args := @ + 1;
         Args (Num_Args) := new String'("-eL");
      end if;

      if U_Option_Set then
         Num_Args := @ + 1;
         Args (Num_Args) := new String'("-U");
      end if;

      if Main_Unit /= null then
         Num_Args := @ + 1;
         Args (Num_Args) := new String'(Main_Unit.all);
      end if;

      Append_Variables (Args, Num_Args);

      if Debug_Mode then
         Put ("gprbuild");

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
           Output_File & ".out", Output_File);
      Free (GPRbuild);

      for J in Args'Range loop
         Free (Args (J));
      end loop;

      return Pid;
   end Spawn_GPRbuild;

   ----------------------------
   -- Warning_Rule_Parameter --
   ----------------------------

   function Warning_Rule_Parameter (Diag : String) return String is
      First_Idx, Last_Idx :          Natural;
      String_To_Search    : constant String := "[Warnings:";
   begin
      --  This function returns non-empty result only if .d parameter is
      --  specified for Warnings rule or if --show-rule gnatcheck option is
      --  set (that is, if Diag ends with "[Warnings:<option>]"

      First_Idx := Index (Diag, String_To_Search);

      if First_Idx = 0 then
         return "";
      else
         First_Idx := First_Idx + String_To_Search'Length;
         Last_Idx  :=
           (if Diag (First_Idx) = '.' then First_Idx + 1 else First_Idx);
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
               for J in Restriction_Setting (R).Param'Range loop
                  XML_Report
                    ("<parameter>" & To_Mixed (R'Img) &
                     "=>"  & Restriction_Setting (R).Param (J).all &
                     "</parameter>",
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
                       ("<parameter>No_Dependence=>" &
                          Forbidden_Units_Dictionary.Next_Entry &
                          "</parameter>",
                        Indent_Level + 1);
                  end loop;
            end case;
         end if;
      end loop;

      XML_Report ("</rule>", Indent_Level);
   end XML_Print_Active_Restrictions;

end Gnatcheck.Compiler;

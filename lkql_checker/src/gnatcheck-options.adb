with GNAT.Command_Line; use GNAT.Command_Line;

with Gnatcheck.Output;       use Gnatcheck.Output;
with Gnatcheck.Projects;     use Gnatcheck.Projects;
with Gnatcheck.Source_Table; use Gnatcheck.Source_Table;

with GNATCOLL.Strings; use GNATCOLL.Strings;

with System.Multiprocessors;

package body Gnatcheck.Options is

   ------------------------------
   -- Opt_Parse error handling --
   ------------------------------

   procedure Warning (Self : in out Gnatcheck_Error_Handler; Msg : String) is
   begin
      Warning (Msg);
   end Warning;

   procedure Error (Self : in out Gnatcheck_Error_Handler; Msg : String) is
   begin
      Error (Msg);
   end Error;

   -------------------
   -- Local helpers --
   -------------------

   function Parse_Arg_As_Natural (Arg : String) return Natural;
   --  Parse the provided string as a ``Natural`` value, raising an
   --  ``Opt_Parse_Error`` in case of parsing error.

   function Parse_Arg_As_Natural (Arg : String) return Natural is
   begin
      begin
         return Natural'Value (Arg);
      exception
         when Constraint_Error =>
            raise Opt_Parse_Error with "cannot parse provided value: " & Arg;
      end;
   end Parse_Arg_As_Natural;

   ------------------
   -- Jobs_Convert --
   ------------------

   function Jobs_Convert (Arg : String) return Natural is
      Value : constant Natural := Parse_Arg_As_Natural (Arg);
   begin
      if Value = 0 then
         return Natural (System.Multiprocessors.Number_Of_CPUs);
      else
         return Value;
      end if;
   end Jobs_Convert;

   -------------------------------
   -- Project_Verbosity_Convert --
   -------------------------------

   function Project_Verbosity_Convert (Arg : String) return Natural is
      Value : constant Natural := Parse_Arg_As_Natural (Arg);
   begin
      if Value > 2 then
         raise Opt_Parse_Error with "invalid value: " & Arg;
      else
         return Value;
      end if;
   end Project_Verbosity_Convert;

   ---------------------------
   -- Max_Diagnoses_Convert --
   ---------------------------

   function Max_Diagnoses_Convert (Arg : String) return Max_Diagnoses_Count is
   begin
      begin
         return Max_Diagnoses_Count'Value (Arg);
      exception
         when Constraint_Error =>
            raise Opt_Parse_Error
              with
                "invalid maximum diagnoses value: " & Arg & " (max is 1000)";
      end;
   end Max_Diagnoses_Convert;

   --------------------
   -- Scan_Arguments --
   --------------------

   procedure Scan_Arguments
     (First_Pass : Boolean := False; Args : Argument_List_Access := null)
   is
      Unknown_Opt_Parse_Args : XString_Vector;
      Args_After_Opt_Parse   : Argument_List_Access;
      Parser                 : Opt_Parser;

      function To_Arg_List (Args : XString_Vector) return Argument_List_Access;
      function To_XString_Array
        (Args : Argument_List_Access) return XString_Array;

      function To_Arg_List (Args : XString_Vector) return Argument_List_Access
      is
         Ret : constant Argument_List_Access :=
           new String_List (1 .. Args.Last_Index);
      begin
         for I in Ret'Range loop
            Ret (I) := new String'(Args (I).To_String);
         end loop;
         return Ret;
      end To_Arg_List;

      function To_XString_Array
        (Args : Argument_List_Access) return XString_Array
      is
         Ret : XString_Array (Args'Range);
      begin
         for I in Args'Range loop
            Ret (I) := To_XString (Args (I).all);
         end loop;
         return Ret;
      end To_XString_Array;

      procedure Process_Sections;
      --  Processes the 'rules' section.

      procedure Process_Sections is
      begin
         --  Processing the 'cargs' section

         Goto_Section ("cargs", Parser => Parser);

         while Getopt ("*", Parser => Parser) /= ASCII.NUL loop
            Store_Compiler_Option (Full_Switch (Parser => Parser));
         end loop;

         --  Processing the 'rules' section
         Goto_Section ("rules", Parser => Parser);

         loop
            case Getopt ("* from=", Parser => Parser) is
               --  We do not want to depend on the set of the currently
               --  implemented rules

               when ASCII.NUL =>
                  exit;

               when 'f'       =>
                  Rule_Options.Append
                    (Option_Record'
                       (File,
                        To_Unbounded_String (Parameter (Parser => Parser))));
                  if not More_Than_One_Legacy_Rule_File_Set then
                     Legacy_Rule_File_Name :=
                       new String'(Parameter (Parser => Parser));
                     More_Than_One_Legacy_Rule_File_Set := True;
                  else
                     Free (Legacy_Rule_File_Name);
                  end if;

               when others    =>
                  Add_Legacy_Rule_Option (Full_Switch (Parser => Parser));
                  Individual_Rules_Set := True;
            end case;
            if not Rules_Depreciation_Emitted then
               Info_In_Tty
                 ("The '-rules' section is now deprecated. You should only use"
                  & " the '--rule' and '--rule-file' command-line options.");
               Info_In_Tty
                 ("You can use the '--emit-lkql-rule-file' flag to "
                  & "automatically translate your rule configuration to the "
                  & "new LKQL format.");
               Rules_Depreciation_Emitted := True;
            end if;
         end loop;
      end Process_Sections;

      Executable : GNAT.OS_Lib.String_Access :=
        Locate_Exec_On_Path (Command_Name);
      Prefix     : constant String :=
        Containing_Directory (Containing_Directory (Executable.all));
      Lkql       : constant String :=
        Compose (Compose (Prefix, "share"), "lkql");

      Args_From_Project : constant Boolean := Args /= null;
      Initial_Char      : Character;
      Success           : Boolean;

      --  Start of processing for Scan_Arguments

   begin
      --  Set Legacy early so that this flag can be checked elsewhere.
      --  If legacy-rules.txt is found, it means we have a full packaging
      --  and a full gnatcheck. If the file is not found, it means we have
      --  a reduced packaging and a legacy gnatcheck.

      if not Is_Regular_File (Compose (Lkql, "legacy-rules.txt")) then
         Legacy := True;
      end if;

      Free (Executable);

      --  Disallow arguments that are not allowed to be specified in project
      --  files
      --  TODO: It might be possible to have a list of subparsers and do a for
      --  loop
      if Args_From_Project then
         declare
            In_Project_Msg : constant String :=
              " is forbidden in project file";
         begin
            Disallow (Arg.Aggregate_Subproject.This, "-A" & In_Project_Msg);
            Disallow (Arg.Project_File.This, "-P" & In_Project_Msg);
            Disallow (Arg.Transitive_Closure.This, "-U" & In_Project_Msg);
            Disallow (Arg.Scenario_Vars.This, "-Xname=val" & In_Project_Msg);
            Disallow
              (Arg.No_Subprojects.This, "--no-subprojects" & In_Project_Msg);
            Disallow (Arg.Project_Verbosity.This, "-vP" & In_Project_Msg);
            Disallow (Arg.Follow_Symbolic_Links.This, "-eL" & In_Project_Msg);
            Disallow (Arg.Lkql_Path.This, "--lkql-path" & In_Project_Msg);
            Disallow (Arg.Rules.This, "-r" & In_Project_Msg);
            Disallow (Arg.Rule_File.This, "--rule-file" & In_Project_Msg);
            Disallow (Arg.Target.This, "--target" & In_Project_Msg);
            Disallow (Arg.RTS.This, "--RTS" & In_Project_Msg);
            Disallow (Arg.Help.This, "-h, --help" & In_Project_Msg);
            Disallow (Arg.List_Rules.This, "--list-rules" & In_Project_Msg);
         end;
      end if;

      if Arg.Parser.Parse
           ((if Args /= null then To_XString_Array (Args) else No_Arguments),
            Unknown_Arguments => Unknown_Opt_Parse_Args)
      then
         Args_After_Opt_Parse := To_Arg_List (Unknown_Opt_Parse_Args);
         Initialize_Option_Scan
           (Parser, Args_After_Opt_Parse, Section_Delimiters => "cargs rules");
      else
         raise Parameter_Error;
      end if;

      --  Reallow arguments that were disallowed
      if Args_From_Project then
         Allow (Arg.Transitive_Closure.This);
         Allow (Arg.Scenario_Vars.This);
         Allow (Arg.Aggregate_Subproject.This);
         Allow (Arg.Project_File.This);
         Allow (Arg.No_Subprojects.This);
         Allow (Arg.Project_Verbosity.This);
         Allow (Arg.Follow_Symbolic_Links.This);
         Allow (Arg.Lkql_Path.This);
         Allow (Arg.Rules.This);
         Allow (Arg.Rule_File.This);
         Allow (Arg.Target.This);
         Allow (Arg.RTS.This);
         Allow (Arg.Help.This);
         Allow (Arg.List_Rules.This);
      end if;

      loop
         Initial_Char :=
           Getopt
             ("-kp-version= "
              & "o= "
              & "ox= "
              & "-version "
              & "nt xml",
              Parser => Parser);

         case Initial_Char is
            when ASCII.NUL =>
               Success := False;

               loop
                  declare
                     Arg : constant String :=
                       Get_Argument (Do_Expansion => True, Parser => Parser);

                  begin
                     exit when Arg = "";
                     Success := True;

                     if Options.Arg.Transitive_Closure.Get then
                        Store_Main_Unit (Arg, Args_From_Project or First_Pass);
                     else
                        Store_Sources_To_Process
                          (Arg, Args_From_Project or First_Pass);

                        if not Args_From_Project then
                           Argument_File_Specified := True;
                        end if;
                     end if;
                  end;
               end loop;

               exit when not Success;

            when 'n'       =>
               if not First_Pass then
                  if Full_Switch (Parser => Parser) = "nt" then
                     Text_Report_ON := False;
                     XML_Report_ON := True;
                  end if;
               end if;

            when 'o'       =>
               if not First_Pass then
                  if Full_Switch (Parser => Parser) = "o" then
                     Set_Report_File_Name (Parameter (Parser => Parser));
                     Custom_Text_Report_File := True;

                  elsif Full_Switch (Parser => Parser) = "ox" then
                     Set_XML_Report_File_Name (Parameter (Parser => Parser));
                     XML_Report_ON := True;
                     Custom_XML_Report_File := True;
                  end if;
               end if;

            when 'x'       =>
               if not First_Pass then
                  if Full_Switch (Parser => Parser) = "xml" then
                     XML_Report_ON := True;
                  end if;
               end if;

            when '-'       =>
               if not First_Pass then
                  if Full_Switch (Parser => Parser) = "-kp-version" then
                     Free (KP_Version);
                     KP_Version := new String'(Parameter (Parser => Parser));
                  end if;
               else
                  if Full_Switch (Parser => Parser) = "-version" then
                     if Args_From_Project then
                        Error
                          ("project file should not contain '--version' "
                           & "option");
                        raise Parameter_Error;
                     end if;

                     Print_Version := True;

                  end if;
               end if;

            when others    =>
               Error
                 ("unrecognized switch: " & Full_Switch (Parser => Parser));
               raise Parameter_Error;
         end case;
      end loop;

      if Current_Section (Parser => Parser) = "" and then not First_Pass then
         Process_Sections;
      end if;

   exception
      when Invalid_Switch =>
         Error ("invalid switch: " & Full_Switch (Parser => Parser));
         raise Parameter_Error;

      when Invalid_Parameter =>
         Error
           ("missing Parameter (Parser => Parser) for: -"
            & Full_Switch (Parser => Parser));
         raise Parameter_Error;
   end Scan_Arguments;

end Gnatcheck.Options;

--
--  Copyright (C) 2005-2024, AdaCore
--  SPDX-License-Identifier: GPL-3.0-or-later
--

with Ada.Characters.Conversions;      use Ada.Characters.Conversions;
with Ada.Characters.Handling;         use Ada.Characters.Handling;
with Ada.Containers.Ordered_Sets;
with Ada.Exceptions;                  use Ada.Exceptions;
with Ada.Strings;                     use Ada.Strings;
with Ada.Strings.Fixed;               use Ada.Strings.Fixed;
with Ada.Strings.Unbounded;           use Ada.Strings.Unbounded;
with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;
with Ada.Text_IO;                     use Ada.Text_IO;

with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib;               use GNAT.OS_Lib;
with GNAT.Regexp;               use GNAT.Regexp;
with GNAT.Table;

with Gnatcheck.Compiler;         use Gnatcheck.Compiler;
with Gnatcheck.JSON_Utilities;   use Gnatcheck.JSON_Utilities;
with Gnatcheck.Options;          use Gnatcheck.Options;
with Gnatcheck.Output;           use Gnatcheck.Output;
with Gnatcheck.String_Utilities; use Gnatcheck.String_Utilities;

with Langkit_Support.Text; use Langkit_Support.Text;

with Rule_Commands; use Rule_Commands;
with Rules_Factory; use Rules_Factory;

package body Gnatcheck.Rules.Rule_Table is

   subtype String_Access is GNAT.OS_Lib.String_Access;

   -----------------------
   -- Local subprograms --
   -----------------------

   Fatal_Error : exception;

   type Rule_File_Record is record
      Arg_Name : String_Access;
      --  Rule file name as it is given in '-from=...' option, used to
      --  generate diagnostic message
      Full_Name : String_Access;
   end record;

   package Rule_File_Stack is new GNAT.Table
    (Table_Component_Type => Rule_File_Record,
     Table_Index_Type     => Natural,
     Table_Low_Bound      => 1,
     Table_Initial        => 20,
     Table_Increment      => 100,
     Table_Name           => "Rule file stack");
   --  Keeps the names of the "nested" rule files, in the order of the
   --  macro expansion that is currently performed, is used to detect looping
   --  in macro expansions

   function Get_Rule_File_Name (RF : String) return String is
        (if Is_Absolute_Path (RF)
         then RF
         else Gnatcheck_Prj.Get_Project_Relative_File (RF));
      --  If gnatcheck is called with a project file, all the (relative) names
      --  of the rule files are considered as related to the project file
      --  directory, otherwise - as related to the current directory

   procedure Check_For_Looping (RF_Name : String; Success : in out Boolean);
   --  Checks if we have a looping in rule files macro expansions. That is,
   --  checks if RF_Name is already stored in Rule_File_Stack. If it is,
   --  generates the corresponding diagnostic message and sets Success OFF.
   --  Otherwise appends the record corresponding to the rule file to
   --  Rule_File_Stack.
   --  This procedure is supposed to be called when we already know that
   --  RF_Name is the name of some existing file.

   procedure Pop_Rule_File;
   --  Removes the last record corresponding to the latest processed rule file
   --  from Rule_File_Stack (we can not just call
   --  Rule_File_Stack.Decrement_Last, because we have to free memory occupied
   --  by the dynamic strings)

   procedure Restrictions_Help (Level : Natural);
   --  Prints out an XML tag representing the gnatcheck Restrictions rule. Note
   --  that the representation is incomplete: the rule may have as its
   --  parameters either restriction identifiers or pairs
   --  'Restriction_Identifier => Restriction_Parameter', but the generated tag
   --  includes information about restriction identifiers only, but not about
   --  parameters that may be needed by some restrictions.

   procedure Restriction_Help (R : Rident.All_Restrictions; Level : Natural);
   --  Prints out an XML tag representing the rule Restrictions with the
   --  corresponding restriction_Id as a parameter. Takes care about
   --  restrictions that need parameters.

   procedure Exception_Cases (Level : Natural);
   --  Prints out XML tags for restrictions that because of some reason are not
   --  included in the set of values of Rident.All_Restrictions type. At the
   --  moment the only restriction that is known as this exception is
   --  No_Dependence.

   function Has_Natural_Parameter (R : Rident.All_Restrictions) return Boolean;
   function Has_Name_Parameter (R : Rident.All_Restrictions) return Boolean;
   --  Tries to guess what kind of parameter the argument restriction has.

   function Check_Rule_Exists
     (Rule_Name : String; Instantiation_Location : String) return Boolean;
   --  Check that provided ``Rule`` designates an existing rule, return
   --  ``True`` in that case. Otherwise, set ``Bad_Rule_Detected`` to
   --  ``True`` and return ``False``.

   function Check_Instance_Is_Unique
     (Instance_Name, Instantiation_Location : String) return Boolean;
   --  Return whether the given instance name isn't already registered in the
   --  global instance table.
   --  In case the instance name is already registered this function also sets
   --  ``Bad_Rule_Detected`` to ``True``, and displays an error message telling
   --  that the instance cannot be instantiated at ``Instantiation_Location``
   --  because it has already be registered.

   procedure Process_Rule_Object
     (LKQL_Rule_File_Name : String;
      Instance_Id : String;
      Instance_Object : GNATCOLL.JSON.JSON_Value);
   --  Process a JSON object representing a rule option coming from the JSON
   --  configuration file.
   --  This function populates the `All_Rules` table according to the given
   --  rule object.

   -----------------------
   -- Check_For_Looping --
   -----------------------

   procedure Check_For_Looping (RF_Name : String; Success : in out Boolean) is
      Full_Name : constant String  := Normalize_Pathname (RF_Name);
   begin
      for J in 1 .. Rule_File_Stack.Last loop
         if Full_Name = Rule_File_Stack.Table (J).Full_Name.all then
            Success := False;
            exit;
         end if;
      end loop;

      if not Success then
         Error ("cycling in rule files:");

         for J in 1 .. Rule_File_Stack.Last loop
            Info_No_EOL (Rule_File_Stack.Table (J).Arg_Name.all);
            Info_No_EOL (" needs ");

            if J < Rule_File_Stack.Last then
               Info (Rule_File_Stack.Table (J + 1).Arg_Name.all);
            end if;
         end loop;

         Info (RF_Name);
         Info ("");

         raise Fatal_Error;

      else
         --  Add new file to the rule file stack
         Rule_File_Stack.Append
           ((Arg_Name =>  new String'(RF_Name),
             Full_Name => new String'(Full_Name)));
      end if;
   end Check_For_Looping;

   ---------------------
   -- Exception_Cases --
   ---------------------

   procedure Exception_Cases (Level : Natural) is
   begin
      --  No_Dependence

      Info ((Level + 1) * Indent_String &
         "<field switch=""+RRestrictions:No_Dependence" &
         """ label=""No_Dependence"  &
         ", specify one unit to check"" separator=""=&gt;""/>");
   end Exception_Cases;

   --------------
   -- Get_Rule --
   --------------

   function Get_Rule (Rule_Name : String) return Rule_Id is
      Try_Id : constant Rule_Id := Find_Rule_Id (Rule_Name);
      Normalized_Rule_Name : constant String := Get_Id_Text (Try_Id);

      function Has_Element
        (Container : String_Maps.Map; Element : String) return Boolean;
      --  Return whether Element (assumed lower case) is present in Container

      -----------------
      -- Has_Element --
      -----------------

      function Has_Element
        (Container : String_Maps.Map; Element : String) return Boolean is
      begin
         for E of Container loop
            if To_Lower (E) = Element then
               return True;
            end if;
         end loop;

         return False;
      end Has_Element;

   begin
      --  First, check if we have a compiler check:
      if Try_Id = Restrictions_Id then
         return Restrictions_Id;

      elsif Try_Id = Style_Checks_Id
        or else Has_Element (Style_To_Instance, Normalized_Rule_Name)
      then
         return Style_Checks_Id;

      elsif Try_Id = Warnings_Id
        or else Has_Element (Warning_To_Instance, Normalized_Rule_Name)
      then
         return Warnings_Id;
      end if;

      --  Try to get the rule from the rule map
      if All_Rules.Contains (Try_Id) then
         return Try_Id;

      --  Then try to get the rule from the instance map which contains all
      --  defined aliases.
      elsif All_Rule_Instances.Contains (Normalized_Rule_Name) then
         return All_Rule_Instances (Normalized_Rule_Name).Rule;

      --  If not found, return the No_Rule identifier
      else
         return No_Rule_Id;
      end if;
   end Get_Rule;

   ------------------
   -- Get_Instance --
   ------------------

   function Get_Instance
     (Instance_Name : String) return Rule_Instance_Access
   is
      Normalized_Instance_Name : constant String := To_Lower (Instance_Name);
   begin
      if All_Rule_Instances.Contains (Normalized_Instance_Name) then
         return All_Rule_Instances (Normalized_Instance_Name);
      else
         return null;
      end if;
   end Get_Instance;

   ----------------------
   -- Turn_Instance_On --
   ----------------------

   procedure Turn_Instance_On (Instance : Rule_Instance_Access)
   is
      Normalized_Instance_Name : constant String :=
        To_Lower (Instance_Name (Instance.all));
      Other_Instance : constant Rule_Instance_Access :=
        Get_Instance (Normalized_Instance_Name);
   begin
      --  Check if the instance with the same name is already in the global map
      if Other_Instance /= null then
         --  If the instance to insert is the same as the already inserted one,
         --  do nothing. Else remove the already present instance.
         if Instance = Other_Instance then
            return;
         else
            Turn_Instance_Off (Normalized_Instance_Name);
         end if;
      end if;

      --  Insert the instance in the global map and the rule instance
      All_Rule_Instances.Insert (Normalized_Instance_Name, Instance);
      if not Is_Compiler_Rule (Instance.Rule) then
         All_Rules (Instance.Rule).Instances.Append (Instance);
      end if;
   end Turn_Instance_On;

   -----------------------
   -- Turn_Instance_Off --
   -----------------------

   procedure Turn_Instance_Off (Instance_Name : String)
   is
      Normalized_Instance_Name : constant String := To_Lower (Instance_Name);
      Instance : Rule_Instance_Access := Get_Instance (Instance_Name);
   begin
      if Instance /= null then
         All_Rule_Instances.Delete (Normalized_Instance_Name);
         if not Is_Compiler_Rule (Instance.Rule) then
            All_Rules (Instance.Rule).Instances.Delete
              (All_Rules (Instance.Rule).Instances.Find_Index (Instance));
         end if;
         Free (Instance);
      end if;
   end Turn_Instance_Off;

   ------------------------
   -- Has_Name_Parameter --
   ------------------------

   function Has_Name_Parameter (R : Rident.All_Restrictions) return Boolean is
      Result : Boolean := False;
   begin
      if R in Rident.All_Parameter_Restrictions then
         declare
            Img       : constant String  := R'Img;
            Img_First : constant Natural := Img'First;
         begin
            if Img'Length >= 4 then
               Result := Img (Img_First .. Img_First + 2) = "NO_";
            end if;
         end;
      end if;

      return Result;
   end Has_Name_Parameter;

   ---------------------------
   -- Has_Natural_Parameter --
   ---------------------------

   function Has_Natural_Parameter
     (R : Rident.All_Restrictions) return Boolean
   is
      Result : Boolean := False;
   begin
      if R in Rident.All_Parameter_Restrictions then
         declare
            Img       : constant String  := R'Img;
            Img_First : constant Natural := Img'First;
         begin
            if Img'Length >= 5 then
               Result := Img (Img_First .. Img_First + 3) = "MAX_";
            end if;
         end;
      end if;

      return Result;
   end Has_Natural_Parameter;

   -----------------------
   -- Check_Rule_Exists --
   -----------------------

   function Check_Rule_Exists
     (Rule_Name : String; Instantiation_Location : String) return Boolean
   is
      Rule : constant Rule_Id := Get_Rule (Rule_Name);
   begin
      if not Present (Rule) then
         Error
           ("unknown rule: "
            & Rule_Name
            & ", ignored"
            & Instantiation_Location);
         Bad_Rule_Detected := True;
         return False;
      end if;
      return True;
   end Check_Rule_Exists;

   ------------------------------
   -- Check_Instance_Is_Unique --
   ------------------------------

   function Check_Instance_Is_Unique
     (Instance_Name, Instantiation_Location : String) return Boolean
   is
      Instance : constant Rule_Instance_Access := Get_Instance (Instance_Name);
   begin
      if Instance /= null then
         Error
           ("rule instance with the same name already exists: """
            & Instance_Name
            & """ previously instantiated at "
            & (if Instance.Defined_At /= ""
               then To_String (Instance.Defined_At)
               else "command line")
            & Instantiation_Location);
         Bad_Rule_Detected := True;
         return False;
      end if;
      return True;
   end Check_Instance_Is_Unique;

   ----------------
   -- Is_Enabled --
   ----------------

   function Is_Enabled (Rule : Rule_Id) return Boolean is
   begin
      pragma Assert (Present (Rule));

      if Rule = Restrictions_Id then
         return Check_Restrictions;
      elsif Rule = Style_Checks_Id then
         return Use_gnaty_Option;
      elsif Rule = Warnings_Id then
         return Use_gnatw_Option;
      else
         return Is_Enabled (All_Rules (Rule));
      end if;
   end Is_Enabled;

   -------------------
   -- Pop_Rule_File --
   -------------------

   procedure Pop_Rule_File is
   begin
      Free (Rule_File_Stack.Table (Rule_File_Stack.Last).Arg_Name);
      Free (Rule_File_Stack.Table (Rule_File_Stack.Last).Full_Name);
      Rule_File_Stack.Decrement_Last;
   end Pop_Rule_File;

   -------------
   -- Present --
   -------------

   function Present (Rule : Rule_Id) return Boolean is
   begin
      return All_Rules.Contains (Rule) or else Is_Compiler_Rule (Rule);
   end Present;

   ------------------------------
   -- Process_Legacy_Rule_File --
   ------------------------------

   procedure Process_Legacy_Rule_File (RF_Name : String) is
      RF : File_Type;

      Rule_Start_Line : Natural := 1;
      --  Number of the line of the rule file where the latest beginning of a
      --  rule or '-from' option is detected, used in diagnostic messages

      Current_Line : Natural := 0;
      --  Number of the currently processed line from the rule file, used in
      --  diagnostic messages

      Last_Rule_Opt_Start_Col  : Natural := 0;
      Last_Rule_Opt_Start_Line : Natural := 0;
      --  Point to the position where the last scanned rule option starts
      Last_Rule_Opt_Start_Col_Old  : Natural := 0;
      Last_Rule_Opt_Start_Line_Old : Natural := 0;
      --  Point to the position where the next before the last scanned rule
      --  option starts

      Line_Buf : String (1 .. 1024);
      Line_Len : Natural := 0;
      --  Buffer to read next line from the file into

      Rule_Buf_Last : constant Positive :=  16 * 1024;
      Rule_Buf      :          String (1 .. Rule_Buf_Last);
      Rule_Len      :          Natural := 0;
      --  Buffer to form the new rule option

      type Scan_Status is (
         Indefinite,      --  we do not know what option is scanned
         In_Rule_Option,  --  we are scanning the rule option
         In_From_Option); --  we are scanning the '-from' option
      --  This type is used to represent the current state of the rule file
      --  scanning process

      New_State : Scan_Status := Indefinite;
      --  Corresponds to the option that is just detected in the rule file

      Old_State : Scan_Status := Indefinite;
      --  represents the option that was detected before detecting the new
      --  one.

      Success : Boolean := True;

      Rule_File_Name : constant String := Get_Rule_File_Name (RF_Name);
      Rule_File_Base : constant String := Base_Name (Rule_File_Name);

      Include_RF_Name : String_Access;

      procedure Scan_Line_Buf (Success : in out Boolean);
      --  Scans Line_Buff, tries to select the rule option or '-from' option
      --  from the sequence of scanned lines and copies this option into
      --  Rule_Buf, setting Rule_Len accordingly. Spaces, comments and empty
      --  lines are skipped.
      --
      --  At the moment, we do not process spaces inside parameters. We will
      --  come back to this problem as soon as we get the first rule that would
      --  need spaces in the parameters
      --
      --  Success is set OFF if a serious problem that does not allow to
      --  continue parsing the rule file has been encountered

      function Is_Opt_Start (Idx : Positive) return Boolean;
      --  Check that Idx (that is supposed to be an index in Line_Buf) points
      --  to the beginning of the rule or '-from' option. If the result is
      --  True, as a side effect this function sets New_State to In_Rule_Option
      --  if the beginning of the rule option is detected, or it sets it
      --  to In_From_Option if the beginning of '-from' option is detected.
      --  If the result is False, this function does not change New_State

      procedure Set_File_Name (Success : in out Boolean);
      --  This procedure is supposed to be called when the  rule buffer
      --  contains (the whole) '-from' option. It scans the buffer and tries
      --  to locate the name of the rule file that is a part of this option.
      --  It sets Success OFF if it can not locate the file name because of
      --  any reason. Otherwise it copies the file name in the beginning of the
      --  line buffer and updates Rule_Len accordingly

      ------------------
      -- Is_Opt_Start --
      ------------------

      function Is_Opt_Start (Idx : Positive) return Boolean is
         Result : Boolean := False;
      begin

         if (Line_Buf (Idx) = '+' or else Line_Buf (Idx) = '-')
           and then (Idx = 1 or else Is_White_Space (Line_Buf (Idx - 1)))
           and then Idx + 1 < Line_Len
         then
            if Line_Buf (Idx + 1) = 'R' then
               Result    := True;
               New_State := In_Rule_Option;
            elsif Idx <= Line_Len - 3
              and then Line_Buf (Idx + 1 .. Idx + 3) = "ALL"
            then
               if Idx + 3 = Line_Len
                 or else
                  (Idx + 3 < Line_Len
                   and then Is_White_Space (Line_Buf (Idx + 4)))
                 or else
                  (Idx + 4 < Line_Len
                   and then Line_Buf (Idx + 4 .. Idx + 5) = "--")
               then
                  Result    := True;
                  New_State := In_Rule_Option;
               end if;
            elsif Idx + 4 <= Line_Len
              and then
               Line_Buf (Idx .. Idx + 4) = "-from"
            then
               Result    := True;
               New_State := In_From_Option;
            end if;
         end if;

         return Result;
      end Is_Opt_Start;

      -------------------
      -- Scan_Line_Buf --
      -------------------

      procedure Scan_Line_Buf (Success : in out Boolean) is
         Idx : Positive := 1;
      begin
         while Idx <= Line_Len loop

            --  White spaces are just ignored. We also skip CR and LF in case
            --  if the rule file has wrong line ends for the given platform

            if Is_White_Space (Line_Buf (Idx))
              or else
               Line_Buf (Idx) = ASCII.CR
              or else
               Line_Buf (Idx) = ASCII.LF
            then
               Idx := Idx + 1;
            else

               --  First, filter out the situation when we have a comment
               if Line_Buf (Idx) = '-'
                 and then
                   Idx < Line_Len
                 and then
                   Line_Buf (Idx + 1) = '-'
               then
                  --  nothing else can be done with this line, so
                  return;
               end if;

               --  Here we have non-blank character that is not the beginning
               --  of the comment

               if Is_Opt_Start (Idx) then
                  --  Start of the new rule option

                  Last_Rule_Opt_Start_Col_Old  := Last_Rule_Opt_Start_Col;
                  Last_Rule_Opt_Start_Line_Old := Last_Rule_Opt_Start_Line;

                  if Line_Buf (Idx) = '+'
                    and then
                     Line_Buf (Idx + 1) = 'R'
                  then
                     Last_Rule_Opt_Start_Line     := Current_Line;
                     Last_Rule_Opt_Start_Col      := Idx;
                  end if;

                  if Rule_Len > 0 then
                     --  We need this condition to process correctly the very
                     --  first option in the rule file

                     case Old_State is
                        when In_Rule_Option =>
                           Process_Legacy_Rule_Option
                             (Rule_Buf (1 .. Rule_Len),
                              Rule_File_Base & ":" &
                              Image (Last_Rule_Opt_Start_Line_Old) & ":" &
                              Image (Last_Rule_Opt_Start_Col_Old));

                        when In_From_Option =>
                           Set_File_Name (Success);

                           if not Success then
                              Error
                                 ("bad format of rule file "   &
                                  RF_Name & ", part of lines " &
                                  Image (Rule_Start_Line)      &
                                  ":"                          &
                                  Image (Current_Line)         &
                                  " ignored");
                              Rule_Option_Problem_Detected := True;

                              Success := True;
                              --  To allow further processing of this rule file

                           else
                              if Is_Regular_File
                                (Rule_Buf (1 .. Rule_Len))
                              then
                                 Process_Legacy_Rule_File
                                   (Rule_Buf (1 .. Rule_Len));
                              else
                                 Error ("can not locate rule file " &
                                 Rule_Buf (1 .. Rule_Len));
                                 Missing_Rule_File_Detected := True;
                              end if;
                           end if;

                        when Indefinite =>
                           Error
                             ("bad format of rule file "   &
                              RF_Name & ", lines "         &
                              Image (Rule_Start_Line)      &
                              ":"                          &
                              Image (Current_Line - 1)     &
                              " do not have format of rule option");
                           Rule_Option_Problem_Detected := True;
                     end case;
                  end if;

                  Rule_Len  := 0;
                  Rule_Start_Line := Current_Line;
                  Old_State := New_State;

                  Rule_Len            := Rule_Len + 1;
                  Rule_Buf (Rule_Len) := Line_Buf (Idx);
                  Idx := Idx + 1;

               else
                  if Rule_Len < Rule_Buf_Last then
                     Rule_Len            := Rule_Len + 1;
                     Rule_Buf (Rule_Len) := Line_Buf (Idx);
                     Idx := Idx + 1;
                  else
                     Error ("can not read rule options from " & RF_Name);
                     Error_No_Tool_Name
                       ("(too long rule option, the content of the file " &
                        "ignored starting from line " & Image (Current_Line));
                     Rule_Option_Problem_Detected := True;
                     Success := False;
                     return;
                  end if;
               end if;
            end if;
         end loop;
      end Scan_Line_Buf;

      -------------------
      -- Set_File_Name --
      -------------------

      procedure Set_File_Name (Success : in out Boolean) is
         First_Idx : Natural := 0;
         Last_Idx  : Natural := Rule_Len;
         --  We will try to set First_Idx and Last_Idx pointing to the part
         --  of Line_Buf that could be a file name

         Eq_Detected : Boolean := False;
      begin

         --  Set First_Idx:

         for J in 6 .. Rule_Len loop
            --  6 means that we skip '-from'

            if not Is_White_Space (Rule_Buf (J)) then
               case Rule_Buf (J) is
                  when '=' =>
                     if not Eq_Detected then
                        Eq_Detected := True;
                        --  this means that we have '-from = <file_name>'
                     else
                        --  a file name can not start from '='
                        exit;
                     end if;

                  when '.'        |
                       '/'        |
                       '\'        |
                       '~'        |
                       'a' .. 'z' |
                       'A' .. 'Z' |
                       '0' .. '9' =>
                     --  This can be the beginning of a file name
                     First_Idx := J;
                     exit;

                  when others =>
                     --  a file name can not start from this character
                     exit;
               end case;
            end if;
         end loop;

         if First_Idx = 0 then
            Success := False;
            return;
         end if;

         --  Set Last_Idx:

         for J in First_Idx + 1 .. Rule_Len loop
            if Is_White_Space (Rule_Buf (J)) then
               Last_Idx := J - 1;
               exit;
            end if;
         end loop;

         --  Check that we have nothing after Last_Idx:

         for J in Last_Idx + 1 .. Rule_Len loop
            if not Is_White_Space (Line_Buf (J)) then
               Success := False;
               exit;
            end if;
         end loop;

         if Success then
            Rule_Len                 := Last_Idx - First_Idx + 1;
            Rule_Buf (1 .. Rule_Len) := Rule_Buf (First_Idx .. Last_Idx);
         end if;
      end Set_File_Name;

   begin -- Process_Rule_File

      if not Is_Regular_File (Rule_File_Name) then
         Error ("can not locate rule file " & Rule_File_Name);
         Missing_Rule_File_Detected := True;
         return;
      else
         Check_For_Looping (Rule_File_Name, Success);

         if not Success then
            return;
         end if;

      end if;

      Open (RF, In_File, Rule_File_Name);

      Rule_Len := 0;

      while Success and then not End_Of_File (RF) loop
         Get_Line (RF, Line_Buf, Line_Len);
         Current_Line :=  Current_Line + 1;

         Scan_Line_Buf (Success);
      end loop;

      --  Process the last rule option, if any

      if Rule_Len > 0 then

         case Old_State is
            when In_Rule_Option =>
               Process_Legacy_Rule_Option
                 (Rule_Buf (1 .. Rule_Len),
                  Rule_File_Base & ":" & Image (Last_Rule_Opt_Start_Line) &
                  ":" & Image (Last_Rule_Opt_Start_Col));
            when In_From_Option =>
               Set_File_Name (Success);

               if not Success then
                  Error
                     ("bad format of rule file "          &
                      Rule_File_Base & ", part of lines " &
                      Image (Rule_Start_Line)             &
                      ":"                                 &
                      Image (Current_Line)                &
                      " ignored");

                  Rule_Option_Problem_Detected := True;
                  Success := True;
                  --  To allow further processing of this rule file
               else
                  Include_RF_Name :=
                    new String'(Get_Rule_File_Name (Rule_Buf (1 .. Rule_Len)));

                  if Is_Regular_File (Include_RF_Name.all) then
                     Process_Legacy_Rule_File (Include_RF_Name.all);
                  else
                     Error ("can not locate rule file " &
                     Rule_Buf (1 .. Rule_Len));
                     Missing_Rule_File_Detected := True;
                  end if;

                  Free (Include_RF_Name);
               end if;

            when Indefinite =>
               Error
                 ("bad format of rule file "          &
                  Rule_File_Base & ", lines "         &
                  Image (Rule_Start_Line)             &
                  ":"                                 &
                  Image (if New_State = Indefinite then
                            Current_Line
                         else Current_Line - 1)       &
                  " do not have format of rule option");
               Rule_Option_Problem_Detected := True;
         end case;

      end if;

      Close (RF);

      Pop_Rule_File;
   exception
      when others =>
         Error ("cannot read rule options from " & Rule_File_Name);
         Rule_Option_Problem_Detected := True;

         if Is_Open (RF) then
            Close (RF);
         end if;

         --  Exception info will be generated in main driver
         raise;
   end Process_Legacy_Rule_File;

   ----------------------------
   -- Process_LKQL_Rule_File --
   ----------------------------

   procedure Process_LKQL_Rule_File (LKQL_RF_Name : String)
   is
      Rule_File_Absolute_Path : constant String :=
        Get_Rule_File_Name (LKQL_RF_Name);
      JSON_Config_File_Name : constant String :=
        Global_Report_Dir.all & "gnatcheck-rules.json.out";
      Parser_Pid : Process_Id;
      Waited_Pid : Process_Id;
      Success : Boolean;
      Config_JSON : Read_Result;

      procedure Rule_Object_Mapper
        (Instance_Id : UTF8_String;
         Instance_Object : JSON_Value);
      --  Stub procedure to call the instance JSON object processing function

      procedure Rule_Object_Mapper
        (Instance_Id : UTF8_String;
         Instance_Object : JSON_Value) is
      begin
         Process_Rule_Object
           (Rule_File_Absolute_Path, String (Instance_Id), Instance_Object);
      end Rule_Object_Mapper;

   begin
      --  Ensure that the provided rule file exists
      if not Is_Regular_File (Rule_File_Absolute_Path) then
         Error ("can not locate LKQL rule file " & Rule_File_Absolute_Path);
         Missing_Rule_File_Detected := True;
         return;
      end if;

      --  Call the LKQL rule config file parser and parse its result
      Parser_Pid :=
        Spawn_LKQL_Rule_File_Parser
          (Rule_File_Absolute_Path, JSON_Config_File_Name);
      Wait_Process (Waited_Pid, Success);

      if Parser_Pid /= Waited_Pid or else not Success then
         Error ("can not call the LKQL rule file parser");
         Rule_Option_Problem_Detected := True;
      else
         Config_JSON := Read (Read_File (JSON_Config_File_Name).all);

         --  If the JSON parsing failed, it means that LKQL rule file
         --  processing failed and diagnostics are in the output file.
         if not Config_JSON.Success then
            Analyze_Output (JSON_Config_File_Name, Success);
            Rule_Option_Problem_Detected := True;

         --  Else, populate the global rule table with the rule config
         else
            Map_JSON_Object (Config_JSON.Value, Rule_Object_Mapper'Access);
         end if;

         --  Delete the temporary JSON files if not it debug mode
         if not Arg.Debug_Mode.Get then
            Delete_File (JSON_Config_File_Name, Success);
         end if;
      end if;
   end Process_LKQL_Rule_File;

   ------------------------------
   -- Process_Single_Rule_Name --
   ------------------------------

   procedure Process_Single_Rule_Name (Rule_Name : String) is
      Lower_Rule_Name : constant String := To_Lower (Rule_Name);
      Rule            : constant Rule_Id := Get_Rule (Lower_Rule_Name);
   begin
      --  Handle cases where the rule is "all"
      if Lower_Rule_Name = "all" then
         Turn_All_Rules_On;
         return;
      end if;

      --  First, check that the designated rule exists
      if not Check_Rule_Exists (Lower_Rule_Name, "") then
         return;
      end if;

      --  Then, check that the rule isn't already instantiated
      if not Check_Instance_Is_Unique (Lower_Rule_Name, "") then
         return;
      end if;

      --  Finally, check that the designated rule is not a compiler rule
      if Is_Compiler_Rule (Rule) then
         Error
           ("Cannot enable a compiler based rule through the '--rule' CLI "
            & "option");
         Bad_Rule_Detected := True;
         return;
      end if;

      --  Finally create a new default instance for the rule
      declare
         New_Instance : constant Rule_Instance_Access :=
           All_Rules (Rule).Create_Instance (False);
      begin
         New_Instance.Rule := Rule;
         New_Instance.Source_Mode := General;
         Turn_Instance_On (New_Instance);
      end;
   end Process_Single_Rule_Name;

   --------------------------------
   -- Process_Legacy_Rule_Option --
   --------------------------------

   procedure Process_Legacy_Rule_Option (Option : String; Defined_At : String)
   is
      First_Idx    : constant Natural := Option'First;
      Last_Idx     : constant Natural := Option'Last;
      Lower_Option : constant String  := To_Lower (Option);

      Word_Start : Natural := 0;
      Word_End   : Natural := 0;
      --  Should be set to select the next subword from Option - either the
      --  rule name or a rule parameter

      Instance_Name_Start : Natural := 0;
      Instance_Name_End   : Natural := 0;
      --  Set to point to the beginning and to the end of the user-defined
      --  instance name (if any).

      procedure Set_Parameter;
      --  Provided that Word_Start points to the beginning of the rule name or
      --  rule parameter, sets Word_Start and Word_End to point to the next
      --  parameter, Sets Word_Start to 0 if there is no parameter any more.
      --  This procedure also checks the syntax of the rule option - that is,
      --  that the rule name is separated from parameter(s) by ':', and
      --  parameters are separated by ',', if this check fails, Word_Start is
      --  set to 0.

      function Check_For_Compiler_Rule
        (Comp_Rule : Rule_Id;
         Instance_Name : String) return Boolean;
      --  Check that the current rule option is fulfilling all requirements for
      --  compiler-based GNATcheck rules. Returns whether the rule option
      --  processing should continue.

      Rule          : Rule_Id;
      Enable        : Boolean;
      Instance_Name : Unbounded_String;
      Instance      : Rule_Instance_Access;

      Diag_Defined_At : constant String :=
        (if Defined_At = "" then "" else " (" & Defined_At & ")");

      -------------------
      -- Set_Parameter --
      -------------------

      procedure Set_Parameter is
         Found : Boolean := False;
      begin
         if Word_End < Last_Idx then
            Word_Start := Word_End + 2;
         else
            Word_Start := 0;
            return;
         end if;

         --  Set Word_Start to the first non-blank and non-comma character.
         --  We skip empty parameters like this
         --
         --     +RRule:"par1, par2,  , par3"

         for J in Word_Start .. Option'Last loop
            if not (Is_White_Space (Option (J)) or else Option (J) = ',') then
               Found      := True;
               Word_Start := J;
               exit;
            end if;
         end loop;

         if not Found then
            Word_Start := 0;
            return;
         end if;

         Word_End := Last_Idx;

         for J in Word_Start + 1 .. Last_Idx loop
            if Option (J) = ',' then
               Word_End := J - 1;
               exit;
            end if;
         end loop;
      end Set_Parameter;

      -----------------------------
      -- Check_For_Compiler_Rule --
      -----------------------------

      function Check_For_Compiler_Rule
        (Comp_Rule : Rule_Id;
         Instance_Name : String) return Boolean
      is
         R_Name : constant String := Rule_Name (Comp_Rule);
      begin
         if Word_Start = 0 and then Enable then
            Error
              (R_Name & " rule option must have a parameter" &
               Diag_Defined_At);
            return False;
         elsif Word_Start /= 0 and then not Enable then
            Error
              ("(" & Instance_Name & ") no parameter allowed for -R" &
               Diag_Defined_At);
            return False;
         end if;

         return True;
      end Check_For_Compiler_Rule;

   begin
      if Lower_Option = "-all" then
         Turn_All_Rules_Off;
         return;
      elsif Lower_Option = "+all" then
         Turn_All_Rules_On;
         return;
      end if;

      if Last_Idx - First_Idx > 2
       and then
         (Option (First_Idx) = '+'
         or else
          Option (First_Idx) = '-')
       and then
          Option (First_Idx + 1) = 'R'
      then
         Enable := Option (First_Idx) = '+';

         --  Computing the rule name
         Word_Start := First_Idx + 2;

         --  Check if we have a user-defined instance name
         if Option (Word_Start) = ':' then
            Word_End := Index (Option (Word_Start + 1 .. Last_Idx), ":");

            if Word_End = 0 then
               Error ("bad structure of rule option " & Option &
                      Diag_Defined_At);
               Rule_Option_Problem_Detected := True;
               return;
            end if;

            Instance_Name_Start := Word_Start + 1;
            Instance_Name_End   := Word_End - 1;

            Word_Start := Word_End + 1;
         end if;

         Word_End := Last_Idx;

         for J in Word_Start + 1 .. Last_Idx loop
            if Option (J) = ':' then
               Word_End := J - 1;
               exit;
            end if;
         end loop;

         --  We start by getting the instantiated rule identifier, and verify
         --  its existence.
         declare
            R_Name : constant String := Option (Word_Start .. Word_End);
         begin
            if Check_Rule_Exists (R_Name, Diag_Defined_At) then
               Rule := Get_Rule (Option (Word_Start .. Word_End));
            else
               return;
            end if;
         end;

         --  Then we get the instance name, either the user defined one, or
         --  the default: the rule name.
         Instance_Name := To_Unbounded_String
           ((if Instance_Name_Start > 0
             then Option (Instance_Name_Start .. Instance_Name_End)
             else Rule_Name (Rule)));
         Instance := Get_Instance (To_String (Instance_Name));

         --  Check that the option is not instantiating a rule with an already
         --  registered instance name. If the option is trying to disable an
         --  instance, check that this instance exists.
         if Enable
           and then not Check_Instance_Is_Unique
                          (To_String (Instance_Name), Diag_Defined_At)
         then
            if not Instance_Help_Emitted then
               Info
                 ("if you want to pass multiple parameters to a rule you " &
                  "should use the comma separated notation: e.g. "         &
                  "+RMy_Rule:Param1,Param2");
               Instance_Help_Emitted := True;
            end if;
            return;
         elsif not Enable and then Instance = null then
            Error
              ("""" & To_String (Instance_Name) & """ is not enabled, " &
               "therefore, cannot be disabled"                          &
               Diag_Defined_At);
            Bad_Rule_Detected := True;
            return;
         end if;

         --  Set the reader to the first parameter
         Set_Parameter;

         --  We need to process compiler-based rules in a separate way
         if Is_Compiler_Rule (Rule) then
            --  Get the rule parameter and perform the initial verification
            if not Check_For_Compiler_Rule (Rule, To_String (Instance_Name))
            then
               Bad_Rule_Detected := True;
               return;
            end if;

            --  If the option is enabling the rule, we have to create a new
            --  instance and process the rule parameter.
            if Enable then
               Instance := new Compiler_Instance (Instance_Name_Start > 0);
               Instance.Rule := Rule;
               Instance.Defined_At := To_Unbounded_String (Defined_At);
               if Instance.Is_Alias then
                  Instance.Alias_Name := Instance_Name;
               end if;

               declare
                  Tagged_Instance : Compiler_Instance renames
                    Compiler_Instance (Instance.all);
               begin
                  while Word_Start /= 0 loop
                     Tagged_Instance.Arguments.Append
                       (Option (Word_Start .. Word_End));
                     Set_Parameter;
                  end loop;
               end;
               Turn_Instance_On (Instance);

            --  Else, we just remove the instance from the global map
            else
               Turn_Instance_Off (To_String (Instance_Name));
            end if;

         --  If the rule is not compiler-based, we process it normally
         else
            if Word_Start = 0 then
               All_Rules (Rule).Process_Rule_Parameter
                 (Rule          => Rule,
                  Instance_Name => To_String (Instance_Name),
                  Param         => "",
                  Enable        => Enable,
                  Defined_At    => Defined_At);
            else
               while Word_Start /= 0 loop
                  All_Rules (Rule).Process_Rule_Parameter
                    (Rule          => Rule,
                     Instance_Name => To_String (Instance_Name),
                     Param         => Option (Word_Start .. Word_End),
                     Enable        => Enable,
                     Defined_At    => Defined_At);
                  Set_Parameter;
               end loop;
            end if;
         end if;
      else
         Error ("unknown rule option: " & Option & ", ignored" &
                 Diag_Defined_At);
         Rule_Option_Problem_Detected := True;
      end if;
   end Process_Legacy_Rule_Option;

   -------------------------
   -- Process_Rule_Object --
   -------------------------

   procedure Process_Rule_Object
     (LKQL_Rule_File_Name : String;
      Instance_Id : String;
      Instance_Object : JSON_Value)
   is
      pragma Unreferenced (Instance_Id);

      Output_Rule_File : constant String :=
        (if Arg.Full_Source_Locations.Get
         then LKQL_Rule_File_Name
         else Base_Name (LKQL_Rule_File_Name));
      Rule_Name : constant String := Instance_Object.Get ("ruleName");
      Instance_Name : constant String :=
        (if Instance_Object.Has_Field ("instanceName")
         then Instance_Object.Get ("instanceName")
         else "");
      R_Id : constant Rule_Id := Get_Rule (Rule_Name);
      Instance : Rule_Instance_Access;
      Source_Mode_String : constant String :=
        Expect (Instance_Object, "sourceMode");
      Params_Object : JSON_Value := Instance_Object.Get ("arguments");

      function Precise_Rule_Name return String is
        ("""" & Rule_Name & """" &
           (if Instance_Name /= ""
            then " (instance """ & Instance_Name & """)"
            else ""));

      procedure Error_In_Rule_File (Msg : String);
      --  Emit a GNATcheck error when there is an error during the processing
      --  of rules defined in `LKQL_Rule_File_Name`.

      procedure Report_Extra_Arg
        (Arg_Name : UTF8_String;
         Arg_Value : JSON_Value);
      --  Report an given argument that hasn't been used during the LKQL rule
      --  file processing.

      ------------------------
      -- Error_In_Rule_File --
      ------------------------

      procedure Error_In_Rule_File (Msg : String) is
      begin
         Error (Msg & " (" & Output_Rule_File & ")");
         Bad_Rule_Detected := True;
      end Error_In_Rule_File;

      ----------------------
      -- Report_Extra_Arg --
      ----------------------

      procedure Report_Extra_Arg
        (Arg_Name : UTF8_String;
         Arg_Value : JSON_Value)
      is
         pragma Unreferenced (Arg_Value);
      begin
         Error_In_Rule_File
           ("extra argument for rule " & Precise_Rule_Name &
            ": '" & Arg_Name & "'");
      end Report_Extra_Arg;

   begin
      --  If the rule is a compiler check then get the argument and process it
      if Is_Compiler_Rule (R_Id) then
         --  Ensure the source mode is "BOTH"
         if Source_Mode_String /= "GENERAL" then
            Error_In_Rule_File
              ("cannot run compiler base rule """
               & Rule_Name
               & """ only on "
               & Source_Mode_String
               & " code");
         end if;

         Instance := new Compiler_Instance (Instance_Name /= "");
         Instance.Rule := R_Id;
         Instance.Defined_At := To_Unbounded_String (Output_Rule_File);
         if Instance.Is_Alias then
            Instance.Alias_Name := To_Unbounded_String (Instance_Name);
         end if;

         declare
            Tagged_Instance : Compiler_Instance renames
              Compiler_Instance (Instance.all);
         begin
            --  Restrictions rule expect a string list as argument
            if R_Id = Restrictions_Id then
               Tagged_Instance.Arguments.Append_Vector
                 (Expect_Literal (Params_Object, "arg"));

            --  Others expects a simple string
            else
               declare
                  S : constant String := Expect_Literal (Params_Object, "arg");
               begin
                  Tagged_Instance.Arguments.Append (S);
               end;
            end if;
            Params_Object.Unset_Field ("arg");
         end;

         --  Turn the newly created instance on
         Turn_Instance_On (Instance);

      --  Else the rule is an LKQL check, check its presence, get the rule
      --  template and call the processing function.
      elsif Present (R_Id) then
         Instance := All_Rules (R_Id).Create_Instance (Instance_Name /= "");
         Instance.Rule := R_Id;
         Instance.Defined_At := To_Unbounded_String (Output_Rule_File);
         if Instance.Is_Alias then
            Instance.Alias_Name := To_Unbounded_String (Instance_Name);
         end if;

         --  Start by setting the rule's source mode
         if Source_Mode_String = "ADA" then
            Instance.Source_Mode := Ada_Only;
         elsif Source_Mode_String = "SPARK" then
            Instance.Source_Mode := Spark_Only;
         else
            Instance.Source_Mode := General;
         end if;

         --  Process the arguments object with the rule template
         Instance.Process_Instance_Params_Object (Params_Object);

         --  Enable the newly created instance
         Turn_Instance_On (Instance);

         --  Finally check that all arguments have been used, else emit an
         --  error for each.
         Params_Object.Map_JSON_Object (Report_Extra_Arg'Access);

      --  Else the rule is not present, emit an error
      else
         Error_In_Rule_File ("unknown rule: " & Rule_Name);
      end if;

   exception
      when E : Field_Not_Found =>
         Error_In_Rule_File
           ("missing '" & Exception_Message (E) & "' parameter for rule " &
            Precise_Rule_Name);
      when E : Invalid_Type =>
         Error_In_Rule_File
           ("invalid parameter for rule " & Precise_Rule_Name & ": " &
            Exception_Message (E));
      when E : Invalid_Value =>
         Error_In_Rule_File
           ("invalid parameter value for rule " & Precise_Rule_Name &
            ": " & Exception_Message (E));
   end Process_Rule_Object;

   --------------------------------
   -- Process_Compiler_Instances --
   --------------------------------

   procedure Process_Compiler_Instances is
   begin
      --  Iterate over all rule instances and process the compiler based ones
      for Instance of All_Rule_Instances loop
         if Is_Compiler_Rule (Instance.Rule) then
            declare
               Tagged_Instance : Compiler_Instance renames
                 Compiler_Instance (Instance.all);
            begin
               for Arg of Tagged_Instance.Arguments loop
                  if Instance.Rule = Restrictions_Id then
                     Process_Restriction_Param (Arg, Instance);
                  elsif Instance.Rule = Style_Checks_Id then
                     Process_Style_Check_Param (Arg, Instance);
                  else
                     Process_Warning_Param (Arg, Instance);
                  end if;
               end loop;
            end;
         end if;
      end loop;
      Set_Compiler_Checks;
   end Process_Compiler_Instances;

   -------------------------------------
   -- Processed_Legacy_Rule_File_Name --
   -------------------------------------

   function Processed_Legacy_Rule_File_Name return String is
   begin
      if Rule_File_Stack.Is_Empty then
         return "";
      else
         return Rule_File_Stack.Table (Rule_File_Stack.Last).Full_Name.all;
      end if;
   end Processed_Legacy_Rule_File_Name;

   ---------------
   -- Rule_Name --
   ---------------

   function Rule_Name (Rule : Rule_Id) return String is
   begin
      pragma Assert (Present (Rule));

      if Rule = Restrictions_Id then
         return "restrictions";
      elsif Rule = Style_Checks_Id then
         return "style_checks";
      elsif Rule = Warnings_Id then
         return "warnings";
      else
         return To_String (All_Rules (Rule).Name);
      end if;
   end Rule_Name;

   ----------------
   -- Rules_Help --
   ----------------

   procedure Rules_Help is
      function "<" (Left, Right : Rule_Info) return Boolean is
        (Left.Name < Right.Name);

      function Equal (Left, Right : Rule_Info) return Boolean is
        (Left.Name = Right.Name);

      package Rule_Sets is new
        Ada.Containers.Ordered_Sets (Rule_Info, "=" => Equal);

      Set : Rule_Sets.Set;

   begin
      if Gnatkp_Mode then
         Info ("gnatkp currently implements the following detectors:");

         if KP_Version /= null then
            for Rule in All_Rules.Iterate loop
               if All_Rules (Rule).Impact = null
                 or else
                   (Match (KP_Version.all, All_Rules (Rule).Impact.all)
                    and then
                      (All_Rules (Rule).Target = null
                       or else To_String (Target) = ""
                       or else Match (To_String (Target),
                                      All_Rules (Rule).Target.all)))
               then
                  Set.Include (All_Rules (Rule));
               end if;
            end loop;
         else
            for Rule in All_Rules.Iterate loop
               Set.Include (All_Rules (Rule));
            end loop;
         end if;

         if Set.Is_Empty then
            Info (" No relevant detector found");
         else
            for Rule of Set loop
               Info
                 (" " & To_String (Rule.Name) & " - " &
                  To_String (Rule.Help_Info));
            end loop;
         end if;
      else
         Info (Executable & " currently implements the following rules:");

         for Rule in All_Rules.Iterate loop
            Set.Include (All_Rules (Rule));
         end loop;

         if Set.Is_Empty then
            Info (" No rule found");
         else
            for R of Set loop
               Print_Rule_Help (R);
            end loop;
         end if;

         Info (Executable & " allows activation of the following checks " &
               "provided by GNAT");
         Info ("using the same syntax to control these checks as for other " &
               "rules:");
         Info (" warnings     - compiler warnings - EASY");

         Info (" style_checks - compiler style checks - TRIVIAL");

         Info (" restrictions - checks made by pragma Restriction_Warnings" &
               " - EASY");
      end if;
   end Rules_Help;

   ----------------------
   -- Restriction_Help --
   ----------------------

   procedure Restriction_Help (R : Rident.All_Restrictions; Level : Natural) is
   begin
      if R in Rident.All_Parameter_Restrictions then
         if Has_Natural_Parameter (R) then
            Info ((Level + 1) * Indent_String &
               "<spin switch=""+RRestrictions:" & Capitalize (R'Img) &
               """ label=""" & Capitalize (R'Img) &
               """ min=""1"" max=""99999"" default=""0""" &
               " separator=""=&gt;""/>");
         elsif Has_Name_Parameter (R) then
            Info ((Level + 1) * Indent_String &
               "<field switch=""+RRestrictions:" & Capitalize (R'Img) &
               """ label=""" & Capitalize (R'Img) &
               ", specify one feature to check"" separator=""=&gt;""/>");
         else
            Error ("restriction " & R'Img & " unknown");
            Bad_Rule_Detected := True;
            pragma Assert (False);
         end if;
      else
         null;
         Info ((Level + 1) * Indent_String &
            "<check switch=""+RRestrictions:" & Capitalize (R'Img) &
            """ label=""" & Capitalize (R'Img) &
            """/>");
      end if;
   end Restriction_Help;

   -----------------------
   -- Restrictions_Help --
   -----------------------

   procedure Restrictions_Help (Level : Natural) is
   begin
      Info (Level * Indent_String & "<category name=""Restriction rules"">");

      Exception_Cases (Level);

      for R in Rident.All_Restrictions loop
         Restriction_Help (R, Level);
      end loop;

      Info (Level * Indent_String & "</category>");
   end Restrictions_Help;

   --------------
   -- XML_Help --
   --------------

   function Category_Name (R : Rule_Info) return String is
     (To_String (R.Category & "/" & R.Subcategory));

   function Lt (Left, Right : Rule_Info) return Boolean is
     (Left.Name < Right.Name);

   function Category_Lt (Left, Right : Rule_Info) return Boolean is
     (Category_Name (Left) < Category_Name (Right));

   function Equal (Left, Right : Rule_Info) return Boolean is
     (Left.Name = Right.Name);

   function Category_Equal (Left, Right : Rule_Info) return Boolean is
     (Category_Name (Left) = Category_Name (Right));

   package Rule_Sets is new Ada.Containers.Ordered_Sets
     (Element_Type => Rule_Info,
      "=" => Equal,
      "<" => Lt);

   package Category_Sets is new Ada.Containers.Ordered_Sets
     (Element_Type => Rule_Info,
      "=" => Category_Equal,
      "<" => Category_Lt);

   procedure XML_Help is
      Level        : Positive;
      Cat_Set      : Category_Sets.Set;
      Rule_Set     : Rule_Sets.Set;
      Previous     : Rule_Info;
      Has_Previous : Boolean := False;
      Args         : Rule_Argument_Vectors.Vector;
   begin
      Info ("<?xml version=""1.0""?>");
      Info ("<gnatcheck>");

      for Rule in All_Rules.Iterate loop
         Cat_Set.Include (All_Rules (Rule));
      end loop;

      for R of Cat_Set loop
         Level := 1;

         if Has_Previous
           and then Previous.Subcategory /= ""
           and then Previous.Category /= R.Category
         then
            Info (Indent_String & "</category>");
         end if;

         if R.Subcategory = "" then
            Info (Indent_String & "<category name=""" &
                  To_String (R.Category) & """>");
         else
            if Has_Previous
              and then Previous.Category /= R.Category
            then
               Info (Indent_String & "<category name=""" &
                     To_String (R.Category) & """>");
            end if;

            Level := 2;
            Info (2 * Indent_String & "<category name=""" &
                  To_String (R.Subcategory) & """>");
         end if;

         declare
            Category : constant String := Category_Name (R);
         begin
            for Rule in All_Rules.Iterate loop
               if Category_Name (All_Rules (Rule)) = Category then
                  Rule_Set.Include (All_Rules (Rule));
               end if;
            end loop;

            for Rule of Rule_Set loop
               Rule.XML_Rule_Help (Rule, Level + 1);
            end loop;
            Rule_Set.Clear;
         end;

         Info (Level * Indent_String & "</category>");
         Previous := R;
         Has_Previous := True;
      end loop;

      if Previous.Subcategory /= "" then
         Info (Indent_String & "</category>");
      end if;

      --  What about warnings and style checks???

      Restrictions_Help (Level => 1);

      --  Display all rule instances
      Info (Indent_String & "<instances>");

      --  Use the rule ordered set to sort the rules by their name
      for Rule of All_Rules loop
         Rule_Set.Include (Rule);
      end loop;

      for Rule of Rule_Set loop
         if not Rule.Instances.Is_Empty then
            Info
              (2 * Indent_String & "<rule name=""" & Rule_Name (Rule) & """>");

            for Instance of Rule.Instances loop
               Instance.Map_Parameters (Args);
               Info
                 (3 * Indent_String & "<instance name=""" &
                  Instance_Name (Instance.all) & """" &
                  (if Args.Is_Empty then " />" else ">"));

               if not Args.Is_Empty then
                  for Arg of Args loop
                     Info
                       (4 * Indent_String & "<arg name=""" &
                        To_String (To_Text (Arg.Name)) & """ value=""" &
                        Escape_Quotes (To_String (To_Text (Arg.Value))) &
                        """ />");
                  end loop;
                  Info (3 * Indent_String & "</instance>");
               end if;
               Args.Clear;
            end loop;

            Info (2 * Indent_String & "</rule>");
         end if;
      end loop;

      Info (Indent_String & "</instances>");

      --  Close the XML help
      Info ("</gnatcheck>");
   end XML_Help;

   ------------------------
   -- Turn_All_Rules_Off --
   ------------------------

   procedure Turn_All_Rules_Off is
      To_Turn_Off : String_Vector;
   begin
      --  Disable all compiled-based rules
      Disable_Restrictions;
      Disable_Style_Checks;
      Disable_Warnings;

      --  Remove all standard rules instances
      for Instance_Cursor in All_Rule_Instances.Iterate loop
         To_Turn_Off.Append (Rule_Instance_Map.Key (Instance_Cursor));
      end loop;
      for Instance_Name in To_Turn_Off.Iterate loop
         Turn_Instance_Off (To_Turn_Off (Instance_Name));
      end loop;
   end Turn_All_Rules_Off;

   -----------------------
   -- Turn_All_Rules_On --
   -----------------------

   procedure Turn_All_Rules_On is
   begin
      for Rule_Cursor in All_Rules.Iterate loop
         declare
            Rule : constant Rule_Info := All_Rules (Rule_Cursor);
            Instance_Name : constant String := To_Lower (Rule_Name (Rule));
            Instance : Rule_Instance_Access;
         begin
            if not All_Rule_Instances.Contains (Instance_Name) then
               Instance := Rule.Create_Instance (Is_Alias => False);
               Instance.Rule := Rule_Map.Key (Rule_Cursor);
               Instance.Source_Mode := General;
               Turn_Instance_On (Instance);
            end if;
         end;
      end loop;
   end Turn_All_Rules_On;

   -------------------
   -- Process_Rules --
   -------------------

   procedure Process_Rules (Ctx : in out Lkql_Context) is
      Rule : Rule_Info;
   begin
      if not Ctx.All_Rules.Is_Empty then
         return;
      end if;

      Ctx.All_Rules :=
        Rules_Factory.All_Rules
          (Ctx.LKQL_Analysis_Context, Path_Array (Arg.Rules_Dirs.Get));

      for R of Ctx.All_Rules loop
         declare
            Name : constant String := To_String (To_Wide_Wide_String (R.Name));
            Id   : constant Rule_Id := Find_Rule_Id (Name);
         begin
            --  Check that the current rule isn't already in the stored rules
            if All_Rules.Contains (Id)
              or else Id = Restrictions_Id
              or else Id = Style_Checks_Id
              or else Id = Warnings_Id
            then
               Error ("multiple rules with the same name: " & Name);
               raise Gnatcheck.Options.Fatal_Error;
            end if;

            Rule           := Create_Rule (R.Param_Kind, To_Lower (Name));
            Rule.Name      := To_Unbounded_String (Name);
            Rule.Help_Info :=
              To_Unbounded_String
                (To_String (To_Wide_Wide_String (R.Help)));

            declare
               Category : constant String :=
                 To_String (To_Wide_Wide_String (R.Category));
            begin
               if Category = "Feature" then
                  Rule.Category := To_Unbounded_String ("Feature Usage Rules");
               elsif Category = "Style" then
                  Rule.Category := To_Unbounded_String ("Style-Related Rules");
               else
                  Rule.Category := To_Unbounded_String (Category);
               end if;
            end;

            Rule.Subcategory :=
              To_Unbounded_String
                (To_String (To_Wide_Wide_String (R.Subcategory)));

            Rule.Parameters                    := R.Parameters;
            Rule.Remediation_Level             := R.Remediation_Level;
            Rule.Allows_Parametrized_Exemption := R.Parametric_Exemption;
            Rule.Impact                        := R.Impact;
            Rule.Target                        := R.Target;
            All_Rules.Insert (Id, Rule);
         end;
      end loop;
   end Process_Rules;

   --------------
   -- Clean_Up --
   --------------

   procedure Clean_Up is
   begin
      --  Free all rule instances
      for Instance_Cursor in All_Rule_Instances.Iterate loop
         Free (All_Rule_Instances (Instance_Cursor));
      end loop;
   end Clean_Up;

end Gnatcheck.Rules.Rule_Table;

------------------------------------------------------------------------------
--                                                                          --
--                                 GNATCHECK                                --
--                                                                          --
--                     Copyright (C) 2004-2022, AdaCore                     --
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

with Ada.Characters.Conversions; use Ada.Characters.Conversions;
with Ada.Characters.Handling;    use Ada.Characters.Handling;
with Ada.Containers.Ordered_Sets;
with Ada.Strings;                use Ada.Strings;
with Ada.Strings.Fixed;          use Ada.Strings.Fixed;
with Ada.Strings.Unbounded;      use Ada.Strings.Unbounded;
with Ada.Text_IO;                use Ada.Text_IO;
with Ada.Wide_Wide_Text_IO;

with GNAT.Directory_Operations;  use GNAT.Directory_Operations;
with GNAT.OS_Lib;                use GNAT.OS_Lib;
with GNAT.Regexp;                use GNAT.Regexp;

with Gnatcheck.Options;          use Gnatcheck.Options;
with Gnatcheck.Output;           use Gnatcheck.Output;
with Gnatcheck.String_Utilities; use Gnatcheck.String_Utilities;

with Gnatcheck.Compiler;         use Gnatcheck.Compiler;

with Rule_Commands;              use Rule_Commands;
with Rules_Factory;              use Rules_Factory;

with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;
with LKQL.Primitives; use LKQL.Primitives;
with LKQL.Evaluation; use LKQL.Evaluation;

with Langkit_Support.Text;        use Langkit_Support.Text;

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
      Normalised_Rule_Name : constant String := To_Lower (Rule_Name);
   begin
      --  First, check if we have a compiler check:

      if Normalised_Rule_Name = "restrictions" then
         return Restrictions_Id;
      elsif Normalised_Rule_Name = "style_checks" then
         return Style_Checks_Id;
      elsif Normalised_Rule_Name = "warnings" then
         return Warnings_Id;
      end if;

      --  This is a rather ineficient implementation. At some point we
      --  should think about a hash table.

      for J in First_Rule .. All_Rules.Last loop
         --  Check the rule name as well as the user specified name, if any

         if To_Lower (All_Rules.Table (J).Name.all) = Normalised_Rule_Name
           or else (All_Rules.Table (J).User_Synonym /= null
                    and then To_Lower (All_Rules.Table (J).User_Synonym.all) =
                             Normalised_Rule_Name)
         then
            return J;
         end if;
      end loop;

      return No_Rule;
   end Get_Rule;

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
     (R    : Rident.All_Restrictions)
      return Boolean
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

   ----------------
   -- Is_Enabled --
   ----------------

   function Is_Enabled (Rule : Rule_Id) return Boolean is
      Result : Boolean := False;
   begin
      if not Present (Rule) then
         raise Fatal_Error;
      end if;

      case Rule is
         when Restrictions_Id =>
            Result := Check_Restrictions;
         when Style_Checks_Id =>
            Result := Use_gnaty_Option;
         when Warnings_Id =>
            Result := Use_gnatw_Option;
         when others =>
            Result := Is_Enabled (All_Rules.Table (Rule).all);
      end case;

      return Result;
   end Is_Enabled;

   --------
   -- No --
   --------

   function No (Id : Rule_Id) return Boolean is
   begin
      return Id not in All_Rules.First .. All_Rules.Last;
   end No;

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

   function Present (Id : Rule_Id) return Boolean is
   begin
      return Id in Compiler_Checks | First_Rule .. All_Rules.Last;
   end Present;

   -----------------------
   -- Process_Rule_File --
   -----------------------

   procedure Process_Rule_File (RF_Name : String) is
      RF           : File_Type;

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

      function Get_Rule_File_Name (RF : String) return String is
        (if Is_Absolute_Path (RF_Name)
           or else not Gnatcheck.Options.Gnatcheck_Prj.Is_Specified
         then RF
         else Normalize_Pathname
                (Dir_Name (Gnatcheck.Options.Gnatcheck_Prj.Source_Prj) & RF));
      --  If gnatcheck is called with a project file, all the (relative) names
      --  of the rule files are considered as related to the project file
      --  directory, otherwise - as related to the current directory

      Rule_File_Name : constant String := Get_Rule_File_Name (RF_Name);

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
                           Process_Rule_Option
                             (Rule_Buf (1 .. Rule_Len),
                              Rule_File_Name & ":" &
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
                                 Process_Rule_File (Rule_Buf (1 .. Rule_Len));
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
               Process_Rule_Option
                 (Rule_Buf (1 .. Rule_Len),
                  Rule_File_Name & ":" & Image (Last_Rule_Opt_Start_Line) &
                  ":" & Image (Last_Rule_Opt_Start_Col));
            when In_From_Option =>
               Set_File_Name (Success);

               if not Success then
                  Error
                     ("bad format of rule file "          &
                      Rule_File_Name & ", part of lines " &
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
                     Process_Rule_File (Include_RF_Name.all);
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
                  Rule_File_Name & ", lines "         &
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
   end Process_Rule_File;

   -------------------------
   -- Process_Rule_Option --
   -------------------------

   procedure Process_Rule_Option
     (Option     : String;
      Defined_At : String)
   is
      First_Idx : constant Natural := Option'First;
      Last_Idx  : constant Natural := Option'Last;

      Word_Start : Natural := 0;
      Word_End   : Natural := 0;
      --  Should be set to select the next subword from Option - either the
      --  rule name or a rule parameter

      Rule_Synonym_Start : Natural := 0;
      Rule_Synonym_End   : Natural := 0;
      --  Set to point to the beginning and to the end of the user-defined
      --  rule synonyv (if any).

      procedure Set_Parameter;
      --  Provided that Word_Start points to the beginning of the rule name or
      --  rule parameter, sets Word_Start and Word_End to point to the next
      --  parameter, Sets Word_Start to 0 if there is no parameter any more.
      --  This procedure also checks the syntax of the rule option - that is,
      --  that the rule name is separated from parameter(s) by ':', and
      --  parameters are separated by ',', if this check fails, Word_Start is
      --  set to 0.

      Rule    : Rule_Id;
      Enable  : Boolean;

      Diag_Defined_At : constant String :=
        (if Defined_At = "" then "" else " (" & Defined_At & ")");

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

   begin
      if Option = "-ALL" then
         Turn_All_Rules_Off;
         return;
      elsif Option = "+ALL" then
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

         --  Computing the rule name and defining the rule

         Word_Start := First_Idx + 2;

         --  Check if we have a user-defined rule synonym for the rule name
         if Option (Word_Start) = ':' then
            Word_End := Index (Option (Word_Start + 1 .. Last_Idx), ":");

            if Word_End = 0 then
               Error ("bad structure of rule option " & Option &
                      Diag_Defined_At);
               Rule_Option_Problem_Detected := True;
               return;
            end if;

            Rule_Synonym_Start := Word_Start + 1;
            Rule_Synonym_End   := Word_End - 1;

            Word_Start := Word_End + 1;
         end if;

         Word_End := Last_Idx;

         for J in Word_Start + 1 .. Last_Idx loop
            if Option (J) = ':' then
               Word_End := J - 1;
               exit;
            end if;
         end loop;

         --  Separate processing for restrictions, warnings and ordinary
         --  rules

         if To_Lower (Option (Word_Start .. Word_End)) = "restrictions" then
            Set_Parameter;

            if Word_Start = 0 then
               Error ("restrictions rule option must have a parameter" &
                      Diag_Defined_At);
               Bad_Rule_Detected := True;
               return;

            else
               while Word_Start /= 0 loop
                  Process_Restriction_Param
                    (Option (Word_Start .. Word_End),
                     Enable);
                  Set_Parameter;
               end loop;
            end if;

         elsif To_Lower (Option (Word_Start .. Word_End)) = "style_checks" then
            if not Enable then
               Error ("there is no -R option for style checks, " &
                      "use style options to turn checks OFF"     &
                      Diag_Defined_At);
               Bad_Rule_Detected := True;
               return;
            end if;

            Set_Parameter;

            if Word_Start = 0 then
               Error ("style_checks rule option must have a parameter" &
                      Diag_Defined_At);
               Bad_Rule_Detected := True;
               return;

            else
               while Word_Start /= 0 loop
                  Process_Style_Check_Param
                    (Option (Word_Start .. Word_End));
                  Set_Parameter;
               end loop;
            end if;

         elsif To_Lower (Option (Word_Start .. Word_End)) = "warnings" then
            if not Enable then
               Error ("there is no -R option for warnings, "     &
                      "use warning options to turn warnings OFF" &
                       Diag_Defined_At);
               Bad_Rule_Detected := True;
               return;
            end if;

            Set_Parameter;

            if Word_Start = 0 then
               Error ("warnings rule option must have a parameter" &
                      Diag_Defined_At);
               Bad_Rule_Detected := True;
               return;

            else
               while Word_Start /= 0 loop
                  Process_Warning_Param (Option (Word_Start .. Word_End));
                  Set_Parameter;
               end loop;
            end if;
         else
            Rule := Get_Rule (Option (Word_Start .. Word_End));

            if Present (Rule) then
               Set_Parameter;

               if Enable and then Rule_Synonym_Start > 0 then
                  Free (All_Rules.Table (Rule).User_Synonym);
                  All_Rules.Table (Rule).User_Synonym :=
                    new String'(Option
                      (Rule_Synonym_Start .. Rule_Synonym_End));
               end if;

               if Word_Start = 0 then
                  Process_Rule_Parameter
                    (Rule       => All_Rules.Table (Rule).all,
                     Param      => "",
                     Enable     => Enable,
                     Defined_At => Defined_At);

               else
                  while Word_Start /= 0 loop
                     Process_Rule_Parameter
                       (Rule       => All_Rules.Table (Rule).all,
                        Param      => Option (Word_Start .. Word_End),
                        Enable     => Enable,
                        Defined_At => Defined_At);
                     Set_Parameter;
                  end loop;
               end if;
            else
               Error ("unknown rule: " & Option (Word_Start .. Word_End) &
                      ", ignored" & Diag_Defined_At);
               Bad_Rule_Detected := True;
            end if;
         end if;
      else
         Error ("unknown rule option: " & Option & ", ignored" &
                 Diag_Defined_At);
         Bad_Rule_Detected := True;
      end if;
   end Process_Rule_Option;

   ------------------------------
   -- Processed_Rule_File_Name --
   ------------------------------

   function Processed_Rule_File_Name return String is
   begin
      if Rule_File_Stack.Is_Empty then
         return "";
      else
         return Rule_File_Stack.Table (Rule_File_Stack.Last).Full_Name.all;
      end if;
   end Processed_Rule_File_Name;

   ---------------
   -- Rule_Name --
   ---------------

   function Rule_Name (R : Rule_Id) return String is
   begin
      pragma Assert (Present (R));

      case R is
         when Restrictions_Id =>
            return "restrictions";
         when Style_Checks_Id =>
            return "style_checks";
         when Warnings_Id =>
            return "warnings";
         when others =>
            return All_Rules.Table (R).Name.all;
      end case;
   end Rule_Name;

   ----------------
   -- Rules_Help --
   ----------------

   procedure Rules_Help is
      function "<" (Left, Right : Rule_Access) return Boolean is
        (Left.Name.all < Right.Name.all);

      function Equal (Left, Right : Rule_Access) return Boolean is
        (Left.Name.all = Right.Name.all);

      package Rule_Sets is new
        Ada.Containers.Ordered_Sets (Rule_Access, "=" => Equal);

      Set : Rule_Sets.Set;

   begin
      if Gnatkp_Mode then
         Info ("gnatkp currently implements the following detectors:");

         if KP_Version /= null then
            for Rule in All_Rules.First .. All_Rules.Last loop
               if All_Rules.Table (Rule).Impact = null
                 or else
                   (Match (KP_Version.all, All_Rules.Table (Rule).Impact.all)
                    and then
                      (All_Rules.Table (Rule).Target = null
                       or else Target.all = ""
                       or else Match (Target.all,
                                      All_Rules.Table (Rule).Target.all)))
               then
                  Set.Include (All_Rules.Table (Rule));
               end if;
            end loop;
         else
            for J in First_Rule .. All_Rules.Last loop
               Set.Include (All_Rules.Table (J));
            end loop;
         end if;

         if Set.Is_Empty then
            Info (" No relevant detector found");
         else
            for R of Set loop
               Print_Rule_Help (R.all);
            end loop;
         end if;
      else
         Info (Executable & " currently implements the following rules:");

         for J in First_Rule .. All_Rules.Last loop
            Set.Include (All_Rules.Table (J));
         end loop;

         if Set.Is_Empty then
            Info (" No rule found");
         else
            for R of Set loop
               Print_Rule_Help (R.all);
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

   function Name (R : Rule_Access) return String is
     (R.Category.all & "/" & R.Subcategory.all);

   function "<" (Left, Right : Rule_Access) return Boolean is
     (Name (Left) < Name (Right));

   function Equal (Left, Right : Rule_Access) return Boolean is
     (Name (Left) = Name (Right));

   package Category_Sets is new
     Ada.Containers.Ordered_Sets (Rule_Access, "=" => Equal);

   procedure XML_Help is
      Level    : Positive;
      Set      : Category_Sets.Set;
      Previous : Rule_Access;

   begin
      Info ("<?xml version=""1.0""?>");
      Info ("<gnatcheck>");

      for J in First_Rule .. All_Rules.Last loop
         Set.Include (All_Rules.Table (J));
      end loop;

      for R of Set loop
         Level := 1;

         if Previous /= null
           and then Previous.Subcategory.all /= ""
           and then Previous.Category.all /= R.Category.all
         then
            Info (Indent_String & "</category>");
         end if;

         if R.Subcategory.all = "" then
            Info (Indent_String & "<category name=""" &
                  R.Category.all & """>");
         else
            if Previous = null
              or else Previous.Category.all /= R.Category.all
            then
               Info (Indent_String & "<category name=""" &
                     R.Category.all & """>");
            end if;

            Level := 2;
            Info (2 * Indent_String & "<category name=""" &
                  R.Subcategory.all & """>");
         end if;

         declare
            Category : constant String := Name (R);
         begin
            for J in First_Rule .. All_Rules.Last loop
               if Name (All_Rules.Table (J)) = Category then
                  XML_Rule_Help (All_Rules.Table (J).all, Level + 1);
               end if;
            end loop;
         end;

         Info (Level * Indent_String & "</category>");
         Previous := R;
      end loop;

      if Previous.Subcategory.all /= "" then
         Info (Indent_String & "</category>");
      end if;

      --  What about warnings and style checks???

      Restrictions_Help (Level => 1);
      Info ("</gnatcheck>");
   end XML_Help;

   --------------------
   -- Set_Rule_State --
   --------------------

   procedure Set_Rule_State (For_Rule : Rule_Id; To_State : Rule_States) is
   begin
      pragma Assert (Present (For_Rule));
      All_Rules.Table (For_Rule).Rule_State := To_State;
   end Set_Rule_State;

   ------------------------
   -- Turn_All_Rules_Off --
   ------------------------

   procedure Turn_All_Rules_Off is
   begin
      for J in All_Rules.First .. All_Rules.Last loop
         All_Rules.Table (J).Rule_State := Disabled;
      end loop;
   end Turn_All_Rules_Off;

   -----------------------
   -- Turn_All_Rules_On --
   -----------------------

   procedure Turn_All_Rules_On is
   begin
      for J in All_Rules.First .. All_Rules.Last loop
         All_Rules.Table (J).Rule_State := Enabled;
      end loop;
   end Turn_All_Rules_On;

   -------------------
   -- Process_Rules --
   -------------------

   procedure Process_Rules (Ctx : in out Lkql_Context) is
      Rule : Rule_Access;
   begin
      if not Ctx.All_Rules.Is_Empty then
         return;
      end if;

      Ctx.All_Rules :=
        Rules_Factory.All_Rules (Ctx.Eval_Ctx, Additional_Rules_Dirs);

      for R of Ctx.All_Rules loop
         declare
            Name : constant String := To_String (To_Wide_Wide_String (R.Name));
         begin
            case R.Param_Kind is
               when No_Param =>
                  Rule := new Rule_Template;
                  Init_Rule (Rule_Template (Rule.all));
               when One_Integer =>
                  Rule := new One_Integer_Parameter_Rule;
                  Init_Rule (One_Integer_Parameter_Rule (Rule.all));
               when One_Boolean =>
                  Rule := new One_Boolean_Parameter_Rule;
                  Init_Rule (One_Boolean_Parameter_Rule (Rule.all));
               when One_String =>
                  Rule := new One_String_Parameter_Rule;
                  Init_Rule (One_String_Parameter_Rule (Rule.all));
               when One_Integer_Or_Booleans =>
                  Rule := new One_Integer_Or_Booleans_Parameter_Rule;
                  Init_Rule
                    (One_Integer_Or_Booleans_Parameter_Rule (Rule.all));
               when One_Array =>
                  Rule := new One_Array_Parameter_Rule;
                  Init_Rule (One_Array_Parameter_Rule (Rule.all));
               when Custom =>
                  if Name = "identifier_suffixes" then
                     Rule := new Identifier_Suffixes_Rule;
                     Init_Rule (Identifier_Suffixes_Rule (Rule.all));

                  elsif Name = "identifier_prefixes" then
                     Rule := new Identifier_Prefixes_Rule;
                     Init_Rule (Identifier_Prefixes_Rule (Rule.all));

                  elsif Name = "identifier_casing" then
                     Rule := new Identifier_Casing_Rule;
                     Init_Rule (Identifier_Casing_Rule (Rule.all));

                  elsif Name = "forbidden_attributes"
                    or else Name = "forbidden_pragmas"
                  then
                     --  all, forbidden [], allowed []
                     Rule := new Forbidden_Rule;
                     Init_Rule (Forbidden_Rule (Rule.all));

                  elsif Name = "silent_exception_handlers" then
                     Rule := new Silent_Exception_Handlers_Rule;
                     Init_Rule (Silent_Exception_Handlers_Rule (Rule.all));

                  else
                     Rule := new Custom_Rule;
                     Init_Rule (Custom_Rule (Rule.all));
                  end if;
            end case;

            Rule.Name        := new String'(Name);
            Rule.Help_Info   :=
              new String'(To_String (To_Wide_Wide_String (R.Help)));

            declare
               Category : constant String :=
                 To_String (To_Wide_Wide_String (R.Category));
            begin
               if Category = "Feature" then
                  Rule.Category := new String'("Feature Usage Rules");
               elsif Category = "Style" then
                  Rule.Category := new String'("Style-Related Rules");
               else
                  Rule.Category := new String'(Category);
               end if;
            end;

            Rule.Subcategory :=
              new String'(To_String (To_Wide_Wide_String (R.Subcategory)));

            Rule.Parameters                    := R.Parameters;
            Rule.Remediation_Level             := R.Remediation_Level;
            Rule.Allows_Parametrized_Exemption := R.Parametric_Exemption;
            Rule.Follow_Instantiations         := R.Follow_Instantiations;
            Rule.Impact                        := R.Impact;
            Rule.Target                        := R.Target;
            All_Rules.Append (Rule);
         end;
      end loop;
   end Process_Rules;

   -----------------------------
   -- Process_Requested_Rules --
   -----------------------------

   procedure Process_Requested_Rules (Ctx : in out Lkql_Context) is

      Dummy : Primitive;

   --  Start of processing for Process_Requested_Rules

   begin
      --  Process potential arguments for rules

      for Rule of Ctx.All_Rules loop
         declare
            Rule_Name : constant Unbounded_Text_Type := Rule.Name;
         begin
            --  Modify the rule command in place, by appending an argument to
            --  the Rule_Command's arg vector.

            if Rule.Is_Unit_Check then
               Rule.Rule_Args.Append
                 (Rule_Argument'(Name  => To_Unbounded_Text ("unit"),
                                 Value => To_Unbounded_Text ("unit")));
            else
               Rule.Rule_Args.Append
                 (Rule_Argument'(Name  => To_Unbounded_Text ("node"),
                                 Value => To_Unbounded_Text ("node")));
            end if;

            --  Compute the map of argument names to values.

            for R in All_Rules.First .. All_Rules.Last loop
               if To_Text (All_Rules.Table (R).Name.all) = To_Text (Rule_Name)
               then
                  if Is_Enabled (All_Rules.Table (R).all) then
                     Map_Parameters (All_Rules.Table (R).all, Rule.Rule_Args);
                  end if;

                  exit;
               end if;
            end loop;
         end;

         --  Call prepare *after* processing the arguments, since it needs the
         --  arguments processed.

         Rule.Prepare;
      end loop;

      for Rule in All_Rules.First .. All_Rules.Last loop
         if Is_Enabled (All_Rules.Table (Rule).all) then
            declare
               Found : Boolean := False;
            begin
               for R of Ctx.All_Rules loop
                  if To_Text (All_Rules.Table (Rule).Name.all)
                       = To_Text (R.Name)
                  then
                     Append_Rule (Ctx, R);
                     Found := True;
                     exit;
                  end if;
               end loop;

               if not Found then
                  raise Exit_App
                    with "no such rule - " & All_Rules.Table (Rule).Name.all;
               end if;
            end;
         end if;
      end loop;

      for Rule of Ctx.All_Rules loop
         --  Eval the rule's code (which should contain only definitions). TODO
         --  this should be encapsulated.
         begin
            Dummy := Eval (Rule.Eval_Ctx, Rule.Lkql_Root);
         exception
            when others =>
               Put ("internal error loading rule ");
               Ada.Wide_Wide_Text_IO.Put (To_Wide_Wide_String (Rule.Name));
               Put_Line (":");
               raise;
         end;
      end loop;
   end Process_Requested_Rules;

end Gnatcheck.Rules.Rule_Table;

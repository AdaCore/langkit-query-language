with Ada.Directories; use Ada.Directories;
with Ada.Wide_Wide_Characters.Handling; use Ada.Wide_Wide_Characters.Handling;
with Langkit_Support.Diagnostics.Output;

with Libadalang.Analysis; use Libadalang.Analysis;

with Rules_Factory; use Rules_Factory;

with LKQL.Eval_Contexts; use LKQL.Eval_Contexts;
with Ada_AST_Nodes; use Ada_AST_Nodes;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Wide_Wide_Unbounded.Wide_Wide_Hash;
with Ada.Containers.Hashed_Maps;

package body Checker_App is

   package Rules_Args_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Unbounded_Text_Type,
      Element_Type    => Rule_Argument_Vectors.Vector,
      Hash            => Ada.Strings.Wide_Wide_Unbounded.Wide_Wide_Hash,
      Equivalent_Keys => Ada.Strings.Wide_Wide_Unbounded."=",
      "="             => Rule_Argument_Vectors."=");

   function Rules return Rule_Vector;

   -----------
   -- Rules --
   -----------

   function Rules return Rule_Vector is
      Explicit_Rules_Names : constant Args.Rules.Result_Array
        := Args.Rules.Get;

      Ret : Rule_Vector;

      Rules_Args_Map : Rules_Args_Maps.Map;
      --  Map from argument names to argument values.
   begin

      --  Compute the map of argument names to values.

      for Rule_Arg of Args.Rules_Args.Get loop
         declare
            Dummy : Boolean;
            C     : Rules_Args_Maps.Cursor;
         begin
            Rules_Args_Map.Insert
              (Rule_Arg.Rule_Name,
               Rule_Argument_Vectors.Empty_Vector,
               C, Dummy);

            Rules_Args_Map
              .Reference (C).Append (Rule_Arg.Arg);
         end;
      end loop;

      --  First, process the set of rules that has to be ran.

      if Explicit_Rules_Names'Length = 0 then
         --  No rules passed by the user: return all rules
         Ret := All_Rules;
      else
         --  Some rules passed by the user: only return the ones specified

         for R of All_Rules loop
            for Explicit_Rule_Name of Explicit_Rules_Names loop
               if To_Lower
                 (To_Text (To_String (Explicit_Rule_Name))) = To_Text (R.Name)
               then
                  Ret.Append (R);
               end if;
            end loop;
         end loop;
      end if;

      --  Then, process potential arguments for those rules

      for Rule of Ret loop
         declare
            Rule_Name : constant Unbounded_Text_Type := Rule.Name;
            C         : constant Rules_Args_Maps.Cursor
              := Rules_Args_Map.Find (Rule_Name);
         begin
            --  Modify the rule command in place, by appending an argument to
            --  the Rule_Command's arg vector.

            if Rules_Args_Maps.Has_Element (C) then
               for Arg of Rules_Args_Map.Reference (C) loop
                  Rule.Rule_Args.Append (Arg);
               end loop;
            end if;
         end;
      end loop;

      return Ret;
   end Rules;

   ---------------------
   -- Process_Context --
   ---------------------

   procedure Job_Post_Process (Context : App_Job_Context)
   is
   begin

      --  Set property error recovery with the value of the command line flag.
      LKQL.Errors.Property_Error_Recovery := Args.Property_Error_Recovery.Get;

      declare
         Ctx : constant Eval_Context :=
           Make_Eval_Context (Context.Units_Processed);
      begin
         for Rule of Rules loop
            for Diag of Rule.Evaluate (Ctx) loop
               Langkit_Support.Diagnostics.Output.Print_Diagnostic
                 (Diag.Diag, Diag.Unit, Simple_Name (Diag.Unit.Get_Filename));
            end loop;
         end loop;
      end;
   end Job_Post_Process;

   package body Args is

      -------------
      -- Convert --
      -------------

      function Convert (Raw_Arg : String) return Qualified_Rule_Argument is
         First_Dot   : constant Natural :=
           Index (Raw_Arg, Pattern => ".");
         First_Equal : constant Natural :=
           Index (Raw_Arg, Pattern => "=", From => First_Dot);
         Ret : Qualified_Rule_Argument;
      begin
         if First_Dot = 0 or First_Equal = 0 then
            raise Opt_Parse_Error
              with "Wrong format for rule argument: " & Raw_Arg;
         end if;
         Ret.Rule_Name :=
           To_Unbounded_Text
             (To_Lower (To_Text (Raw_Arg (Raw_Arg'First .. First_Dot - 1))));
         Ret.Arg.Name :=
           To_Unbounded_Text
             (To_Text (Raw_Arg (First_Dot + 1 .. First_Equal - 1)));
         Ret.Arg.Value :=
           To_Unbounded_Text
             (To_Text (Raw_Arg (First_Equal + 1 .. Raw_Arg'Last)));

         return Ret;
      end Convert;

   end Args;

end Checker_App;

with Ext_AST_Nodes; use Ext_AST_Nodes;

with LKQL.Unit_Utils;    use LKQL.Unit_Utils;
with LKQL.Primitives;    use LKQL.Primitives;
with LKQL.Evaluation;    use LKQL.Evaluation;
with LKQL.Eval_Contexts; use LKQL.Eval_Contexts;

with Liblkqllang.Analysis;

with ${lib_name}.Analysis;

with GNATCOLL.Opt_Parse; use GNATCOLL.Opt_Parse;

with Ada.Text_IO;            use Ada.Text_IO;
with Ada.Characters.Latin_1;
with Ada.Strings.Unbounded;  use Ada.Strings.Unbounded;

procedure Repl is

   package LAL renames ${lib_name}.Analysis;

   package Arg is

      Parser : Argument_Parser := Create_Argument_Parser
        (Help => "The repl will build an AST from the given ${lang_name} program" &
                 " and then run the queries against it.");

      package Input_Program is new Parse_Positional_Arg
        (Parser   => Parser,
         Name     => "script_path",
         Arg_Type => Unbounded_String,
         Help     => "Path of the program to analyze");

   end Arg;

   function Get_Unit (Context : LAL.Analysis_Context;
                       FileName : String) return LAL.Analysis_Unit;

   --------------
   -- Get_Unit --
   --------------

   function Get_Unit (Context  : LAL.Analysis_Context;
                      FileName : String) return LAL.Analysis_Unit
   is
      Unit : constant LAL.Analysis_Unit := Context.Get_From_File (FileName);
   begin
      if Unit.Has_Diagnostics then
         for D of Unit.Diagnostics loop
            Put_Line (Unit.Format_GNU_Diagnostic (D));
         end loop;

         raise Program_Error with "Invalid analysis unit... Quiting.";
      end if;

      return Unit;
   end Get_Unit;

   function Get_User_Input return String;

   --------------------
   -- Get_User_Input --
   --------------------

   function Get_User_Input return String is
      use Ada.Characters.Latin_1;
      Result : Unbounded_String;
      Buffer : Unbounded_String;
   begin
      Put (">>> ");

      loop
         Buffer := To_Unbounded_String (Get_Line);

         if Tail (Buffer, 2) = ";;" then
            Append (Result, Head (Buffer, Length (Buffer) - 2));
            return To_String (Result);
         else
            Append (Result, Buffer & LF);
         end if;
      end loop;
   end Get_User_Input;

   Buffer       : Unbounded_String;
   Ada_Context  : constant LAL.Analysis_Context :=
     ${lib_name}.Analysis.Create_Context;
   LKQL_Context : constant Liblkqllang.Analysis.Analysis_Context :=
     Liblkqllang.Analysis.Create_Context;
   LKQL_Unit    : Liblkqllang.Analysis.Analysis_Unit;
   Ctx          : Eval_Context;

begin
   if not Arg.Parser.Parse then
      return;
   end if;

   declare
      Ada_Unit : constant LAL.Analysis_Unit :=
        Get_Unit (Ada_Context, To_String (Arg.Input_Program.Get));
   begin
      Ctx := Make_Eval_Context (Make_Ext_AST_Node (Ada_Unit.Root), 
                                Make_Ext_AST_Node (LAL.No_${root_type}));
   end;

   loop
      Buffer := To_Unbounded_String (Get_User_Input);

      if Buffer = "exit" then
         return;
      end if;

      LKQL_Unit := Make_LKQL_Unit_From_Code (LKQL_Context, To_String (Buffer));

      Display (Check_And_Eval (Ctx, LKQL_Unit.Root));
   end loop;
end Repl;

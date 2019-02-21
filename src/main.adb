with Interpreter.Evaluation; use Interpreter.Evaluation;
with Interpreter.Types.Primitives; use Interpreter.Types.Primitives;

with Liblkqllang.Analysis;

with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;

procedure Main is
   package LEL renames Liblkqllang.Analysis;
   Context: constant LEL.Analysis_Context := LEL.Create_Context;
   Unit: constant LEL.Analysis_Unit := Context.Get_From_File (Argument (1));
   Evaluation_Context: EvalCtx;
   Ignore: Primitive;
begin
   if Unit.Has_Diagnostics then
      for D of Unit.Diagnostics loop
         Put_Line (Unit.Format_GNU_Diagnostic (D));
      end loop;
   else 
      Ignore := Eval (Evaluation_Context, Unit.Root);
   end if;
end Main;

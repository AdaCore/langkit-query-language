with LKQL.Unit_Utils; use LKQL.Unit_Utils;
with LKQL.Evaluation; use LKQL.Evaluation;

with Ada.Assertions;                  use Ada.Assertions;

package body LKQL.Exec is

   ----------
   -- Load --
   ----------

   function Load
     (Path : String; Context : out L.Analysis_Context) return Eval_Context
   is (Load (Make_LKQL_Unit (Path, Context).Root));

   ----------
   -- Load --
   ----------

   function Load (Script : L.LKQL_Node) return Eval_Context is
      Ctx    : constant Eval_Context := Make_Eval_Context;
      Ignore : constant Primitive := Eval (Ctx, Script);
   begin
      return Ctx;
   end Load;

   ------------
   -- Lookup --
   ------------

   function Lookup
     (Ctx : Eval_Context; Name : Unbounded_Text_Type) return Primitive
   is
      use String_Value_Maps;
      Position : constant Cursor := Ctx.Lookup (Name);
   begin
      pragma Assert (Has_Element (Position),
                     "Unknwown identifier : " & To_UTF8 (To_Text (Name)));

      return Element (Position);
   end Lookup;

   -------------------
   -- Call_Function --
   -------------------

   function Call_Function (Ctx       : Eval_Context;
                           Name      : Unbounded_Text_Type;
                           Arguments : Environment_Map) return Primitive
   is
      Fun        : constant Primitive := Lookup (Ctx, Name);
      Definition : L.Fun_Def;
   begin
      pragma Assert (Kind (Fun) = Kind_Fun,
                     To_UTF8 (To_Text (Name)) & " is not a function");

      Definition := Fun_Val (Fun);

      pragma Assert
        (Definition.F_Parameters.Children_Count = Natural (Arguments.Length),
         "Function " & To_UTF8 (To_Text (Name)) & " expected" &
          Integer'Image (Definition.F_Parameters.Children_Count) &
          " parameters bu got " & Integer'Image (Natural (Arguments.Length)));

      for Name of Definition.F_Parameters loop
         if not Arguments.Contains (To_Unbounded_Text (Name.Text)) then
            raise Assertion_Error with
              "Missing parameter: " & To_UTF8 (Name.Text);
         end if;
      end loop;

      return Eval
        (Ctx, Fun_Val (Fun).F_Body_Expr, Local_Bindings => Arguments);
   end Call_Function;

   --------------------
   -- Eval_LKQL_Code --
   --------------------

   function Eval_LKQL_Code (LKQL_Code : String) return Primitive is
     (Eval_LKQL_Code (Make_Eval_Context, LKQL_Code));

   --------------------
   -- Eval_LKQL_Code --
   --------------------

   function Eval_LKQL_Code
     (Ctx : Eval_Context; LKQL_Code : String) return Primitive
   is
      LKQL_Unit    : constant L.Analysis_Unit :=
        Make_LKQL_Unit_From_Code (LKQL_Code);
   begin
      return Eval (Ctx, LKQL_Unit.Root);
   end Eval_LKQL_Code;

end LKQL.Exec;

with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with GNATCOLL.Projects; use GNATCOLL.Projects;
with GNATCOLL.Utils; use GNATCOLL.Utils;

with Langkit_Support.Text; use Langkit_Support.Text;
with Langkit_Support.Symbols; use Langkit_Support.Symbols;

with Liblkqllang.Analysis;
with Liblkqllang.Prelude;
with Liblkqllang.Public_Converters;

with Libadalang.Helpers; use Libadalang.Helpers;
with Libadalang.Analysis; use Libadalang.Analysis;
with Libadalang.Introspection; use Libadalang.Introspection;
with Libadalang.Common;

with Ada_AST_Nodes; use Ada_AST_Nodes;
with LKQL.Evaluation; use LKQL.Evaluation;
with LKQL.Primitives; use LKQL.Primitives;
with LKQL.Errors;
with LKQL.Eval_Contexts; use LKQL.Eval_Contexts;

package body Liblkqllang.Implementation.Extensions is

   package LALCO renames Libadalang.Common;

   function Get_All_Completions_For_Id
     (Id : Ada_AST_Nodes.Node_Type_Id;
      In_Pattern : Boolean := False) return Unbounded_String_Array;
   --  Get all valid completions (fields & properties) for a given
   --  Node_Type_Id.

   function Units return Unit_Vectors.Vector;
   --  Return all the units for the LKQL context.

   --  TODO: for the moment the state is global, we need to store it in the
   --  LKQL context.
   Ctx      : Libadalang.Analysis.Analysis_Context;
   Files    : String_Vectors.Vector;
   LKQL_Ctx : Eval_Context;
   Init     : Boolean := False;

   -----------
   -- Units --
   -----------

   function Units return Unit_Vectors.Vector is
      Ret : Unit_Vectors.Vector;
   begin
      for F of Files loop
         Ret.Append (Ctx.Get_From_File (To_String (F)));
      end loop;
      return Ret;
   end Units;

   ------------------------------
   -- LKQL_Node_P_Prelude_Unit --
   ------------------------------

   function LKQL_Node_P_Prelude_Unit
     (Node : Bare_LKQL_Node) return Internal_Unit
   is
     (Liblkqllang.Prelude.Prelude_Unit);

   ------------------------------------------
   -- LKQL_Node_P_Interp_Init_From_Project --
   ------------------------------------------

   function LKQL_Node_P_Interp_Init_From_Project
     (Node         : Bare_LKQL_Node;
      Project_File : Character_Type_Array_Access) return Boolean
   is
      Project : Project_Tree_Access;
      Env     : Project_Environment_Access;

      UFP     : Unit_Provider_Reference;
   begin
      if Init then
         return False;
      end if;

      Libadalang.Helpers.Load_Project
        (Image (Project_File.Items), Project => Project, Env => Env);

      List_Sources_From_Project (Project.all, False, Files);

      UFP := Project_To_Provider (Project);
      Ctx := Create_Context (Charset => "utf-8", Unit_Provider => UFP);

      LKQL_Ctx := Make_Eval_Context (Units);

      Init := True;
      return True;
   end LKQL_Node_P_Interp_Init_From_Project;

   -----------------------------
   -- LKQL_Node_P_Interp_Eval --
   -----------------------------

   function LKQL_Node_P_Interp_Eval (Node : Bare_LKQL_Node) return Symbol_Type
   is
   begin
      return Find
           (Node.Unit.Context.Symbols,
            To_Text
              (To_Unbounded_Text
                   (Check_And_Eval
                        (LKQL_Ctx, Public_Converters.Wrap_Node (Node)))));
   exception
      when E : LKQL.Errors.Stop_Evaluation_Error=>
         return Find (Node.Unit.Context.Symbols,
                      To_Text ("<ERROR>: " & Exception_Message (E)));
   end LKQL_Node_P_Interp_Eval;

   --------------------------------
   -- Get_All_Completions_For_Id --
   --------------------------------

   function Get_All_Completions_For_Id
     (Id : Ada_AST_Nodes.Node_Type_Id;
      In_Pattern : Boolean := False) return Unbounded_String_Array
   is
      Props  : LALCO.Property_Reference_Array := Properties (Id);
      Fields : LALCO.Syntax_Field_Reference_Array := Syntax_Fields (Id);
      Ret    : Unbounded_String_Array (1 .. Props'Length + Fields'Length);
      Idx    : Positive := 1;
   begin
      for Field of Fields loop
         declare
            Val : Unbounded_String :=
              To_Unbounded_String (Member_Name (Field));
         begin
            if In_Pattern then
               Append (Val, "=");
            end if;

            Ret (Idx) := Val;
            Idx := Idx + 1;
         end;
      end loop;

      for Prop of Props loop
         declare
            Val : Unbounded_String :=
              To_Unbounded_String (Member_Name (Prop));
         begin
            if In_Pattern then
               if Property_Argument_Types (Prop)'Length > 0 then
                  Append (Val, "(");
               else
                  Append (Val, "() is");
               end if;
            end if;

            Ret (Idx) := Val;
            Idx := Idx + 1;
         end;
      end loop;

      return Ret;
   end Get_All_Completions_For_Id;

   ---------------------------------
   -- LKQL_Node_P_Interp_Complete --
   ---------------------------------

   function LKQL_Node_P_Interp_Complete
     (Node : Bare_LKQL_Node) return Symbol_Type_Array_Access
   is

      function Make_Sym_Array
        (Strings : Unbounded_String_Array) return Symbol_Type_Array_Access;

      --------------------
      -- Make_Sym_Array --
      --------------------

      function Make_Sym_Array
        (Strings : Unbounded_String_Array) return Symbol_Type_Array_Access
      is
         Ret : Symbol_Type_Array_Access :=
           Create_Symbol_Type_Array (Strings'Length);

         Idx : Positive := 1;
      begin
         for S of Strings loop
            Ret.Items (Idx)
              := Find (Node.Unit.Context.Symbols, To_Text (To_String (S)));
            Idx := Idx + 1;
         end loop;

         return Ret;
      end Make_Sym_Array;

      use Liblkqllang.Analysis;

      PNode : LKQL_Node := Public_Converters.Wrap_Node (Node);
      Last_Token : Token_Kind := Kind (Data (PNode.Token_End));

   begin

      --  Implement runtime based completion (a la IPython/etc) based on the
      --  syntactic content of the string.

      case Node.Kind is

      when LKQL_Query =>

         --  select |

         if Last_Token not in LKQL_Select_Tok then
            return No_Symbol_Type_Array_Type;
         end if;

         return Make_Sym_Array (Kind_Names);


      when LKQL_Node_Pattern_Property
         | LKQL_Node_Pattern_Field =>

         --  Node(a=|
         --  Node(a() is |

         if Last_Token not in LKQL_Eq | LKQL_Is then
            return No_Symbol_Type_Array_Type;
         end if;

         return Make_Sym_Array (Kind_Names);

      when LKQL_Extended_Node_Pattern =>

         --  Node(|

         if Last_Token not in LKQL_L_Par | LKQL_Coma then
            return No_Symbol_Type_Array_Type;
         end if;

         declare
            VP : Value_Pattern :=
              PNode.As_Extended_Node_Pattern.F_Node_Pattern;
         begin
            if VP.Kind = LKQL_Node_Kind_Pattern then
               return Make_Sym_Array
                 (Get_All_Completions_For_Id
                    (Kind (Image (VP.As_Node_Kind_Pattern.F_Kind_Name.Text))));
            else
               return No_Symbol_Type_Array_Type;
            end if;
         end;

      when LKQL_Dot_Access =>
         declare
            LHS : Analysis.Expr := PNode.As_Dot_Access.F_Receiver;
         begin

            --  a.|
            --  where a is a dot access/simple identifier, not a complex expr

            if Last_Token /= LKQL_Dot
              or else LHS.Kind not in
                LKQL_Dot_Access | LKQL_Identifier | LKQL_Safe_Access
            then
               return No_Symbol_Type_Array_Type;
            end if;

            declare
               Val : Primitive := Check_And_Eval (LKQL_Ctx, LHS);
            begin
               if Val.Get.Kind = Kind_Node then
                  return Make_Sym_Array
                    (Get_All_Completions_For_Id
                       (Get_Node_Type_Id
                            (Ada_Ast_Node
                                 (Val.Get.Node_Val.Unchecked_Get.all))));
               end if;
            end;

            return No_Symbol_Type_Array_Type;

         end;
      when others =>
         return No_Symbol_Type_Array_Type;
      end case;
   end LKQL_Node_P_Interp_Complete;

end Liblkqllang.Implementation.Extensions;

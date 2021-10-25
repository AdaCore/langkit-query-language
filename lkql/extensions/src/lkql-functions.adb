------------------------------------------------------------------------------
--                                                                          --
--                                   LKQL                                   --
--                                                                          --
--                     Copyright (C) 2019-2021, AdaCore                     --
--                                                                          --
-- LKQL is free software;  you can redistribute it and/or modify  it        --
-- under terms of the GNU General Public License  as published by the Free  --
-- Software Foundation;  either version 3,  or (at your option)  any later  --
-- version.   This  software  is distributed in the hope that it  will  be  --
-- useful but  WITHOUT ANY WARRANTY;  without even the implied warranty of  --
-- MERCHANTABILITY  or  FITNESS  FOR  A PARTICULAR PURPOSE.                 --
--                                                                          --
-- As a special  exception  under  Section 7  of  GPL  version 3,  you are  --
-- granted additional  permissions described in the  GCC  Runtime  Library  --
-- Exception, version 3.1, as published by the Free Software Foundation.    --
--                                                                          --
-- You should have received a copy of the GNU General Public License and a  --
-- copy of the GCC Runtime Library Exception along with this program;  see  --
-- the files COPYING3 and COPYING.RUNTIME respectively.  If not, see        --
-- <http://www.gnu.org/licenses/>.                                          --
------------------------------------------------------------------------------

with LKQL.Selector_Lists;   use LKQL.Selector_Lists;
with LKQL.Depth_Nodes;      use LKQL.Depth_Nodes;
with LKQL.Custom_Selectors; use LKQL.Custom_Selectors;
with LKQL.Errors;           use LKQL.Errors;
with LKQL.Evaluation;       use LKQL.Evaluation;
with LKQL.Error_Handling;   use LKQL.Error_Handling;
with LKQL.Node_Extensions;  use LKQL.Node_Extensions;
with LKQL.Node_Data;        use LKQL.Node_Data;

with Ada.Strings.Wide_Wide_Unbounded;
with LKQL.Partial_AST_Nodes; use LKQL.Partial_AST_Nodes;

with Ada.Containers;

package body LKQL.Functions is
   procedure Process_Function_Arguments
     (Ctx           : Eval_Context;
      Call          : L.Fun_Call;
      Param_Count   : Natural;
      Skip_First_Arg : Boolean := False;
      Param_Index   : access function (Param_Name : Symbol_Type)
                                       return Natural;
      Default_Value : access function (Param_Index : Positive)
                                       return Primitive_Option;
      Eval_Arg      : access function (Param_Index : Positive;
                                       Arg_Expr    : L.Expr)
                                       return Primitive;
      Match_Found   : access procedure (Param_Index : Positive;
                                        Arg_Value   : Primitive));
   --  Implements the common logic for processing a function call, such as
   --  checking existence of named arguments, arity, duplicate parameters, etc.
   --  If ``Skip_First_Arg`` is True, then we assume the call is a dot method
   --  call, and the ``Call`` syntax node doesn't contain the first argument.

   function Call_Builtin
     (Ctx            : Eval_Context;
      Builtin_Descr  : Builtin_Function_Description;
      Call           : L.Fun_Call;
      First_Arg      : Primitive := null) return Primitive;
   --  Call the given built-in function. If ``First_Arg`` has a non null value,
   --  then we assume it's a dot call to a built-in, and the ``Call`` syntax
   --  node will only contain the remaining arguments.

   function Eval_User_Fun_Call
     (Ctx  : Eval_Context;
      Call : L.Fun_Call;
      Func : Primitive) return Primitive;
   --  Evaluate a call to a user-defined function

   function Eval_Builtin_Call
     (Ctx  : Eval_Context;
      Call : L.Fun_Call;
      Fun  : Primitive) return Primitive;
   --  Evaluate a call to a built-in function

   function Eval_User_Selector_Call
     (Ctx  : Eval_Context;
      Call : L.Fun_Call;
      Sel  : Primitive) return Primitive;
   --  Eval a call to a selector

   ---------------
   -- Eval_Call --
   ---------------

   function Eval_Call
     (Ctx : Eval_Context; Call : L.Fun_Call) return Primitive
   is
      Func : Primitive;
      use LCO;
   begin
      --  Special case for builtin-method calls. Since we don't want to create
      --  closures for built-in methods, we make a special case here where we
      --  recognize the case when the called entity can be a built-in method
      --  reference, and call them directly.
      if Call.F_Name.Kind = LKQL_Dot_Access then
         declare
            Node : constant L.Dot_Access := Call.F_Name.As_Dot_Access;
            Receiver : constant Primitive := Eval (Ctx, Node.F_Receiver);
            Builtin_Desc : constant Builtin_Method_Descriptor :=
              (Receiver.Kind,
               Symbol (Node.F_Member));

            Cur          : constant Builtin_Methods_Maps.Cursor :=
              Get_Builtin_Methods (Ctx.Kernel).Find (Builtin_Desc);

         begin
            if Builtin_Methods_Maps.Has_Element (Cur) then
               return Call_Builtin
                 (Ctx,
                  Builtin_Methods_Maps.Element (Cur).all,
                  Call,
                  Receiver);
            end if;
         end;
      end if;

      Func := Eval (Ctx, Call.F_Name);

      --  If this is a safe call and the callable is null, return unit.
      if Call.F_Has_Safe and then Is_Nullish (Func) then
         return Make_Unit_Primitive;
      end if;

      --  Called entity should be a function or a selector
      if Kind (Func) not in
        Kind_Function | Kind_Selector | Kind_Builtin_Function
          | Kind_Property_Reference
      then
         Raise_Invalid_Type (Ctx, Call.As_LKQL_Node,
                             "function or selector", Func);
      end if;

      --  Call the proper eval sub function depending on the kind of the called
      --  entity.
      case Kind (Func) is
         when Kind_Function =>
            return Eval_User_Fun_Call (Ctx, Call, Func);
         when Kind_Selector =>
            return Eval_User_Selector_Call (Ctx, Call, Func);
         when Kind_Builtin_Function =>
            return Eval_Builtin_Call (Ctx, Call, Func);
         when Kind_Property_Reference =>
            return Eval_Node_Property
              (Ctx,
               Func.Property_Node.Unchecked_Get.all,
               Func.Ref.Unchecked_Get.all, Call.F_Arguments);
         when others =>
            raise Program_Error with "unreachable";
      end case;
   end Eval_Call;

   --------------------------------
   -- Process_Function_Arguments --
   --------------------------------

   procedure Process_Function_Arguments
     (Ctx            : Eval_Context;
      Call           : L.Fun_Call;
      Param_Count    : Natural;
      Skip_First_Arg : Boolean := False;
      Param_Index    : access function (Param_Name : Symbol_Type)
                                       return Natural;
      Default_Value  : access function (Param_Index : Positive)
                                       return Primitive_Option;
      Eval_Arg       : access function (Param_Index : Positive;
                                       Arg_Expr    : L.Expr)
                                       return Primitive;
      Match_Found    : access procedure (Param_Index : Positive;
                                        Arg_Value   : Primitive))
   is
      type Has_Arg_Array is array (Positive range 1 .. Param_Count) of Boolean;
      --  Array of booleans, used to check whether an arg was passed.

      Has_Arg  : Has_Arg_Array := (others => False);
   begin

      --  Do the argument evaluation and checking in the same pass
      for I in
        Call.F_Arguments.First_Child_Index .. Call.F_Arguments.Last_Child_Index
      loop
         declare
            Arg : constant L.Arg := Call.F_Arguments.Child (I).As_Arg;
            Arg_Name : constant Symbol_Type := Symbol (Arg.P_Name);
         begin
            if Arg_Name /= null then

               --  Named arg: check if the name exists in the definition's
               --  profile.
               declare
                  Position : constant Natural := Param_Index (Arg_Name);
               begin

                  if Position > 0 then
                     --  Check that it has not already been seen
                     if Has_Arg (Position) then
                        Raise_Already_Seen_Arg (Ctx, Arg);
                     end if;

                     --  All is good, mark the arg as passed
                     Has_Arg (Position) := True;

                     Match_Found (Position, Eval_Arg (Position, Arg.P_Expr));
                  else
                     --  No parameter for this arg: raise
                     Raise_Unknown_Argument (Ctx, Arg.P_Name);
                  end if;
               end;
            else
               declare
                  J : constant Positive :=
                    (if Skip_First_Arg then I + 1 else I);
               begin
                  --  Positional arg: check if there is an arg at this
                  --  position.
                  if J > Param_Count then

                     --  No arg at this pos: raise
                     Raise_Invalid_Arity (Ctx, Param_Count, Call.F_Arguments);
                  else

                     --  All is good, mark the arg as passed
                     Has_Arg (J) := True;

                     Match_Found (J, Eval_Arg (J, Arg.P_Expr));
                  end if;
               end;
            end if;
         end;
      end loop;

      --  Second step: check that every arg has been passed, and evaluate
      --  default values for parameters that were passed no value.
      for I in Has_Arg_Array'Range loop
         --  We have no argument at position I
         if not (Has_Arg (I) or else (Skip_First_Arg and then I = 1)) then
            declare
               Default : constant Primitive_Option := Default_Value (I);
            begin
               --  It could be an arg with a default value ..
               if Primitive_Options.Is_Some (Default) then

                  --  In that case eval the default value and add it to the
                  --  args map.
                  Match_Found (I, Primitive_Options.Extract (Default));
               else
                  --  But if not, raise
                  Raise_And_Record_Error
                    (Ctx,
                     Make_Eval_Error
                       (Call,
                        "Missing value for param #"
                        & I'Wide_Wide_Image & " in call"));
               end if;
            end;
         end if;
      end loop;
   end Process_Function_Arguments;

   ------------------------
   -- Eval_User_Fun_Call --
   ------------------------

   function Eval_User_Fun_Call
     (Ctx  : Eval_Context;
      Call : L.Fun_Call;
      Func : Primitive) return Primitive
   is

      use Callable_Caches;

      function Param_Index (Name : Symbol_Type) return Natural;
      function Default_Value (I : Positive) return Primitive_Option;
      function Eval_Arg (I : Positive; Arg : L.Expr) return Primitive;
      procedure Match_Found (Param_Index : Positive; Arg_Value : Primitive);

      Def : constant L.Base_Function := Func.Fun_Node;
      Env : constant LKQL.Primitives.Environment_Access :=
        Func.Frame;

      Def_Ext       : constant Ext := Get_Ext (Def);
      Args_Bindings : Environment_Map;
      Has_Cache     : constant Boolean := Func.Call_Cache /= No_Cache;
      Cache_Vector  : Primitive_Vectors.Vector;

      -----------------
      -- Param_Index --
      -----------------

      function Param_Index (Name : Symbol_Type) return Natural is
         Cur : constant Params_Maps.Cursor :=
           Def_Ext.Content.Params.Find (Name);
      begin
         return (if Params_Maps.Has_Element (Cur)
                 then Params_Maps.Element (Cur).Pos
                 else 0);
      end Param_Index;

      -------------------
      -- Default_Value --
      -------------------

      function Default_Value (I : Positive) return Primitive_Option is
         Default_Expr : constant L.Expr :=
            Def.F_Parameters.Child (I).As_Parameter_Decl.F_Default_Expr;
      begin
         return (if Default_Expr.Is_Null
                 then Primitive_Options.None
                 else Primitive_Options.To_Option (Eval (Ctx, Default_Expr)));
      end Default_Value;

      --------------
      -- Eval_Arg --
      --------------

      function Eval_Arg (I : Positive; Arg : L.Expr) return Primitive is
        (Eval (Ctx, Arg));

      -----------------
      -- Match_Found --
      -----------------

      procedure Match_Found (Param_Index : Positive; Arg_Value : Primitive) is
         Param_Name : constant Symbol_Type := Symbol
           (Def.F_Parameters.Child
              (Param_Index).As_Parameter_Decl.P_Identifier);

         use Ada.Containers;
      begin
         Args_Bindings.Insert (Param_Name, Arg_Value);
         if Has_Cache then
            if Cache_Vector.Length < Count_Type (Param_Index) then
               Cache_Vector.Set_Length (Count_Type (Param_Index));
            end if;
            Cache_Vector (Param_Index) := Arg_Value;
         end if;
      end Match_Found;

      Eval_Ctx : constant Eval_Context :=
        Eval_Context'(Ctx.Kernel, Eval_Contexts.Environment_Access (Env));
   begin
      Process_Function_Arguments
        (Ctx,
         Call,
         Def.F_Parameters.Children_Count,
         False,
         Param_Index'Access,
         Default_Value'Access,
         Eval_Arg'Access,
         Match_Found'Access);

      if Has_Cache then
         declare
            Cached_Return : constant Primitive :=
              Query (Func.Call_Cache, Cache_Vector);
         begin
            if Cached_Return /= null then
               return Cached_Return;
            else
               declare
                  Ret : constant Primitive := Eval
                   (Eval_Ctx, Def.F_Body_Expr,
                    Local_Bindings => Args_Bindings);
               begin
                  Insert (Func.Call_Cache, Cache_Vector, Ret);
                  return Ret;
               end;
            end if;
         end;
      end if;

      return Eval
        (Eval_Ctx, Def.F_Body_Expr, Local_Bindings => Args_Bindings);
   end Eval_User_Fun_Call;

   -----------------------------
   -- Eval_User_Selector_Call --
   -----------------------------

   function Eval_User_Selector_Call
     (Ctx  : Eval_Context;
      Call : L.Fun_Call;
      Sel  : Primitive) return Primitive
   is
      pragma Warnings (Off);
      Def : constant L.Selector_Decl := Sel.Sel_Node;
      Env : constant LKQL.Primitives.Environment_Access :=
        Sel.Frame;
      S_List : Selector_List;
      Eval_Ctx      : constant Eval_Context :=
        Eval_Context'(Ctx.Kernel, Eval_Contexts.Environment_Access (Env));
   begin
      if Call.F_Arguments.Last_Child_Index = 0 then
         Raise_And_Record_Error
           (Ctx,
            Make_Eval_Error
              (Call, "Selector call should have a node argument"));
      end if;

      declare
         Root_Node_Arg : Primitive := Eval
           (Ctx,
            Call.F_Arguments.Child (1).As_Expr_Arg.F_Value_Expr,
            Kind_Node);

         Root          : H.AST_Node_Holder := Root_Node_Arg.Node_Val;

         Selector_Iterator : constant Depth_Node_Iter_Access :=
           new Depth_Node_Iter'Class'
             (Depth_Node_Iter'Class
                (Make_Custom_Selector_Iter
                   (Ctx, Sel, L.No_Expr, L.No_Expr, Root)));
      begin
         return To_Primitive
           (Make_Selector_List (Selector_Iterator), Ctx.Pool);
      end;

   end Eval_User_Selector_Call;

   -----------------------
   -- Eval_Builtin_Call --
   -----------------------

   function Eval_Builtin_Call
     (Ctx  : Eval_Context;
      Call : L.Fun_Call;
      Fun  : Primitive) return Primitive
   is
      Builtin_Descr : constant Builtin_Function_Description :=
        Fun.Builtin_Fn.all;
   begin
      return Call_Builtin (Ctx, Builtin_Descr, Call);
   end Eval_Builtin_Call;

   ------------------
   -- Call_Builtin --
   ------------------

   function Call_Builtin
     (Ctx            : Eval_Context;
      Builtin_Descr  : Builtin_Function_Description;
      Call           : L.Fun_Call;
      First_Arg      : Primitive := null) return Primitive
   is
      function Param_Index (Name : Symbol_Type) return Natural;
      function Default_Value (I : Positive) return Primitive_Option;
      function Eval_Arg (I : Positive; Arg : L.Expr) return Primitive;
      procedure Match_Found (Param_Index : Positive; Arg_Value : Primitive);

      Param_Values  : Primitive_Array (Builtin_Descr.Params'Range);

      -----------------
      -- Param_Index --
      -----------------

      function Param_Index (Name : Symbol_Type) return Natural is
         use type Ada.Strings.Wide_Wide_Unbounded.Unbounded_Wide_Wide_String;
      begin
         for I in 1 .. Builtin_Descr.N loop
            if Builtin_Descr.Params (I).Name = Name.all then
               return I;
            end if;
         end loop;
         return 0;
      end Param_Index;

      -------------------
      -- Default_Value --
      -------------------

      function Default_Value (I : Positive) return Primitive_Option is
        (Builtin_Descr.Params (I).Default_Value);

      --------------
      -- Eval_Arg --
      --------------

      function Eval_Arg (I : Positive; Arg : L.Expr) return Primitive is
        (Eval (Ctx, Arg, Builtin_Descr.Params (I).Expected_Kind));

      -----------------
      -- Match_Found --
      -----------------

      procedure Match_Found (Param_Index : Positive; Arg_Value : Primitive) is
      begin
         Param_Values (Param_Index) := Arg_Value;
      end Match_Found;

   begin
      Process_Function_Arguments
        (Ctx,
         Call,
         Builtin_Descr.N,
         --  ??? GNAT bug: /= doesn't work here.
         not (First_Arg = null),
         Param_Index'Access,
         Default_Value'Access,
         Eval_Arg'Access,
         Match_Found'Access);

      if not (First_Arg = null) then
         Param_Values (1) := First_Arg;
      end if;

      return Builtin_Descr.Fn_Access (Ctx, Param_Values);
   end Call_Builtin;

end LKQL.Functions;

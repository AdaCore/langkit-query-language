with Options;
with Iters.Iterators;
with Iters.Maps;
with Interpreter.Primitives;       use Interpreter.Primitives;
with Interpreter.Eval_Contexts;    use Interpreter.Eval_Contexts;

with Langkit_Support.Text; use Langkit_Support.Text;

with Ada.Unchecked_Deallocation;

package Interpreter.Evaluation is

   function Eval (Ctx            : Eval_Context;
                  Node           : LEL.LKQL_Node'Class;
                  Expected_Kind  : Base_Primitive_Kind := No_Kind;
                  Local_Bindings : Environment := String_Value_Maps.Empty_Map)
                  return Primitive;
   --  Return the result of the AST node's evaluation in the given context.
   --  An Eval_Error will be raised if the node represents an invalid query or
   --  expression.

   function To_Ada_Node_Kind
     (Kind_Name : Text_Type) return LALCO.Ada_Node_Kind_Type;
   --  Return the Ada_Node_Kind_Type that matches the given name.
   --  Raise a program error if the name doesn't correspond to any
   --  ada_Node_Kind_Type.

private

   -----------------------------------------
   -- Comprehensions environment iterator --
   -----------------------------------------

   package Environment_Iters is new Iters.Iterators (Environment);

   package Primitive_Options is new Options (Primitive);
   use Primitive_Options;

   procedure Free_Resetable_Environement_Iter is new Ada.Unchecked_Deallocation
     (Environment_Iters.Resetable_Iter, Environment_Iters.Resetable_Access);

   type Comprehension_Env_Iter is new Environment_Iters.Iterator_Interface with
      record
         Binding_Name    : Unbounded_Text_Type;
         Current_Element : Primitive_Options.Option;
         --  Value of the next element to be yielded
         Gen             : Primitive_Iters.Iterator_Access;
         Nested          : Environment_Iters.Resetable_Access;
      end record;

   overriding function Next (Iter   : in out Comprehension_Env_Iter;
                             Result : out Environment) return Boolean;

   overriding function Clone
     (Iter : Comprehension_Env_Iter) return Comprehension_Env_Iter;

   overriding procedure Release (Iter : in out Comprehension_Env_Iter);

   type Comprehension_Env_Iter_Access is access all Comprehension_Env_Iter;

   -----------------------------
   -- Comprehesion evaluation --
   -----------------------------

   package Env_Primitive_Maps is
     new Iters.Maps (Environment_Iters, Primitive_Iters);

   type Closure is new Env_Primitive_Maps.Map_Funcs.Func with record
      Ctx       : Eval_Context;
      --  Copy of the evaluation context at call site
      Body_Expr : LEL.Expr;
      --  Body of the closure
   end record;

   overriding function Evaluate (Self    : in out Closure;
                                 Element : Environment) return Primitive;

   overriding function Clone (Self : Closure) return Closure;

   function Make_Closure
     (Ctx : Eval_Context; Body_Expr : LEL.Expr) return Closure;

   type Comprehension_Guard_Filter is new Environment_Iters.Predicates.Func
   with record
      Ctx   : Eval_Context;
      Guard : LEL.Expr;
   end record;

   function Evaluate (Self : in out Comprehension_Guard_Filter;
                      Element : Environment) return Boolean;

   function Clone
     (Self : Comprehension_Guard_Filter) return Comprehension_Guard_Filter;

   function Make_Guard_Filter (Ctx : Eval_Context;
                               Guard : LEL.Expr)
                               return Comprehension_Guard_Filter;

end Interpreter.Evaluation;

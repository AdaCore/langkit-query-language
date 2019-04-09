package body Interpreter.Eval_Contexts is

   ---------------
   -- Add_Error --
   ---------------

   procedure Add_Error (Ctx : Eval_Context; Error : Error_Data) is
   begin
      Ctx.Kernel.Last_Error := Error;
   end Add_Error;

   ---------------------------
   -- Release_Current_Frame --
   ---------------------------

   procedure Release_Current_Frame (Ctx : in out Eval_Context) is
   begin
      Free_Environment (Ctx.Frames);
   end Release_Current_Frame;

   ----------------
   -- Last_Error --
   ----------------

   function Last_Error (Ctx : Eval_Context) return Error_Data is
     (Ctx.Kernel.Last_Error);

   ----------------------------
   -- Error_Recovery_Enabled --
   ----------------------------

   function Error_Recovery_Enabled (Ctx : Eval_Context) return Boolean is
     (Ctx.Kernel.Error_Recovery_Enabled);

   --------------
   -- AST_Root --
   --------------

   function AST_Root (Ctx : Eval_Context) return LAL.Ada_Node is
      (Ctx.Kernel.Ast_Root);

   --------------
   -- Set_Root --
   --------------

   procedure Set_AST_Root (Ctx : Eval_Context; New_Root : LAL.Ada_Node) is
   begin
      Ctx.Kernel.Ast_Root := New_Root;
   end Set_AST_Root;

   ----------------------
   -- Create_New_Frame --
   ----------------------

   function Create_New_Frame (Ctx            : Eval_Context;
                              Local_Bindings : Environment_Map := Empty_Map)
                              return Eval_Context
   is
      New_Env     : constant Environment_Access :=
        new Environment'(Local_Bindings, Ctx.Frames);
   begin
      return Eval_Context'(Ctx.Kernel, New_Env);
   end Create_New_Frame;

   ------------
   -- Lookup --
   ------------

   function Lookup (Ctx : Eval_Context;
                    Key : Unbounded_Text_Type) return String_Value_Maps.Cursor
   is (Lookup (Ctx.Frames.all, Key));

   -----------------
   -- Add_Binding --
   -----------------

   procedure Add_Binding (Ctx   : Eval_Context;
                          Key   : Text_Type;
                          Value : Primitive)
   is
   begin
      Ctx.Frames.Local_Bindings.Include (To_Unbounded_Text (Key), Value);
   end Add_Binding;

   -----------------------
   -- Make_Eval_Context --
   -----------------------

   function Make_Eval_Context (Ast_Root     : LAL.Ada_Node := LAL.No_Ada_Node;
                               Err_Recovery : Boolean := False)
                               return Eval_Context
   is
      Kernel : constant Global_Data_Access :=
        new Global_Data'(Ast_Root, Make_Empty_Error, Err_Recovery);
      Env    : constant Environment_Access :=
        new Environment'(Make_Empty_Environment);
   begin
      return Eval_Context'(Kernel, Env);
   end Make_Eval_Context;

   -----------------------
   -- Free_Eval_Context --
   -----------------------

   procedure Free_Eval_Context (Ctx : in out Eval_Context) is
   begin
      pragma Assert (Ctx.Frames.Parent = null,
                     "Cannot free a non-root evaluation context");
      Free_Environment (Ctx.Frames);
      Free_Global_Data (Ctx.Kernel);
   end Free_Eval_Context;

   ------------
   -- Lookup --
   ------------

   function Lookup (Env : Environment;
                    Key : Unbounded_Text_Type) return String_Value_Maps.Cursor
   is
      Lookup_Result : constant Cursor := Env.Local_Bindings.Find (Key);
   begin
      if not Has_Element (Lookup_Result) and then Env.Parent /= null then
         return Lookup (Env.Parent.all, Key);
      end if;

      return Lookup_Result;
   end Lookup;

   ----------------------------
   -- Make_Empty_Environment --
   ----------------------------

   function Make_Empty_Environment (Parent : Environment_Access := null)
                                    return Environment
   is (Environment'(String_Value_Maps.Empty_Map, Parent));

   ------------------
   -- Add_Bindings --
   ------------------

   procedure Add_Bindings
     (Env : in out Environment_Map; New_Bindings : Environment_Map)
   is
   begin
      for C in Iterate (New_Bindings) loop
         Env.Include (Key (C), Element (C));
      end loop;
   end Add_Bindings;

end Interpreter.Eval_Contexts;

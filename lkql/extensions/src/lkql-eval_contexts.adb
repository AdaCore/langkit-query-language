package body LKQL.Eval_Contexts is

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

   -------------------------
   -- Exists_In_Local_Env --
   -------------------------

   function Exists_In_Local_Env (Ctx : Eval_Context;
                                 Key : Text_Type) return Boolean
   is (Ctx.Exists_In_Local_Env (To_Unbounded_Text (Key)));

   -------------------------
   -- Exists_In_Local_Env --
   -------------------------

   function Exists_In_Local_Env (Ctx : Eval_Context;
                                 Key : Unbounded_Text_Type) return Boolean
   is (Ctx.Frames.Local_Bindings.Contains (Key));

   ---------------
   -- Null_Node --
   ---------------

   function Null_Node (Ctx : Eval_Context) return AST_Node_Rc is
      (Ctx.Kernel.Null_Node);

   ----------------------------
   -- Error_Recovery_Enabled --
   ----------------------------

   function Error_Recovery_Enabled (Ctx : Eval_Context) return Boolean is
     (Ctx.Kernel.Error_Recovery_Enabled);

   --------------
   -- AST_Root --
   --------------

   function AST_Root (Ctx : Eval_Context) return AST_Node_Rc is
     (Ctx.Kernel.Ast_Root);

   -----------
   -- Clone --
   -----------

   function Clone_Frame (Ctx : Eval_Context) return Eval_Context is
     ((Kernel => Ctx.Kernel,
       Frames => new Environment'(Ctx.Frames.all)));

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

   ---------------------
   -- Is_Root_Context --
   ---------------------

   function Is_Root_Context (Ctx : Eval_Context) return Boolean is
     (Ctx.Frames.Parent = null);

   --------------------
   -- Parent_Context --
   --------------------

   function Parent_Context (Ctx : Eval_Context) return Eval_Context is
      Parent_Env : constant Environment_Access :=
        Ctx.Frames.Parent;
   begin
      return Eval_Context'(Ctx.Kernel, Parent_Env);
   end Parent_Context;

   -----------------------
   -- Make_Eval_Context --
   -----------------------

   function Make_Eval_Context (Ast_Root     : AST_Node_Rc;
                               Null_Node    : AST_Node_Rc;
                               Err_Recovery : Boolean := False)
                               return Eval_Context
   is
      Kernel : constant Global_Data_Access :=
        new Global_Data'(Ast_Root, Null_Node, Make_Empty_Error, Err_Recovery);
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

   procedure Merge_Item_Into (Env   : in out Environment_Map;
                              Key   : Unbounded_Text_Type;
                              Value : Primitive);

   ----------------
   -- Merge_Into --
   ----------------

   procedure Merge_Into
     (Env : in out Environment_Map; Other : Environment_Map)
   is
   begin
      for C in Iterate (Other) loop
         Merge_Item_Into (Env, Key (C), Element (C));
      end loop;
   end Merge_Into;

   ---------------------
   -- Merge_Item_Into --
   ---------------------

   procedure Merge_Item_Into (Env   : in out Environment_Map;
                              Key   : Unbounded_Text_Type;
                              Value : Primitive)
   is
      Pos : constant Cursor := Env.Find (Key);
   begin
      if not Has_Element (Pos) then
         Env.Insert (Key, Value);
      elsif Kind (Element (Pos)) = Kind_List then
         Extend (Element (Pos), Value);
      elsif Kind (Value) = Kind_List then
         Extend (Value, Element (Pos));
         Env.Include (Key, Value);
      else
         declare
            List : constant Primitive := Make_Empty_List;
         begin
            Append (List, Element (Pos));
            Append (List, Value);
            Env.Insert (Key, List);
         end;
      end if;
   end Merge_Item_Into;

end LKQL.Eval_Contexts;

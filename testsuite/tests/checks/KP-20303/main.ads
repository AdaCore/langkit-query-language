package Main is
   type T_Integer is new Integer with Default_Value => 1;
   subtype Sub_Integer is T_Integer range 1 .. 50;
   type Derived_Integer is new T_Integer range 1 .. 50;
   subtype Sub_String is String (1 .. 5);

   type Tag_T is tagged null record;
   type Tag_T_Acc is access all Tag_T'Class;

   type Other_Tag_T is tagged null record;
   type Other_Tag_T_Acc is access all Other_Tag_T'Class;

   procedure Proc (T : Tag_T; I : out T_Integer)
   is null
   with Pre'Class => True;

   procedure Proc (T : Tag_T; I : out Derived_Integer)
   is null
   with Pre'Class => True;

   function Func (T : Tag_T; I : out T_Integer) return Boolean
   is (False)
   with Pre'Class => True;

   function Func
     (T : Tag_T; O : Other_Tag_T_Acc; I : out T_Integer) return Boolean
   is (False)
   with Pre'Class => True;

   procedure Proc_Not_Out (T : Tag_T; I : T_Integer)
   is null
   with Pre'Class => True;

   procedure Proc_Str (T : Tag_T; I : out Sub_String)
   is null
   with Pre'Class => True;

   procedure Proc_No_Precond (T : Tag_T; I : out T_Integer) is null;

   procedure Main (T : Tag_T_Acc);

   generic
   package Gen_Pkg is
      procedure Main (T : Tag_T_Acc);

      I : Sub_Integer := 42;

      function Func (T : Tag_T_Acc) return Boolean
      is (T.Func (I));  -- FLAG

      function Func_No_Dispatch (T : Tag_T) return Boolean
      is (T.Func (I));  -- NOFLAG

      function Func_No_Dyn_Dispatch (T : Tag_T'Class) return Boolean
      is (T.Func (I));  -- NOFLAG

      function Func_Not_Controlling
        (T : Tag_T'Class; O : Other_Tag_T_Acc) return Boolean
      is (T.Func (O, I));  -- FLAG (False positive)
   end Gen_Pkg;

   generic
      type G_Int is range <>;
   package Gen_Pkg_Inner is
      subtype Sub_G_Int is G_Int range 1 .. 50;

      type Inner_Tag_T is tagged null record;
      type Inner_Tag_T_Acc is access all Inner_Tag_T'Class;

      procedure Proc (T : Inner_Tag_T; I : out G_Int)
      with Pre'Class => True;

      procedure Proc_Same_Type (T : Inner_Tag_T; I : out G_Int)
      with Pre'Class => True;

      procedure Main (T : Inner_Tag_T_Acc);
   end Gen_Pkg_Inner;

   generic
   package Gen_Pkg_No_Subp is
      T : Tag_T_Acc := new Tag_T;
      I : Sub_Integer := 42;
      B : Boolean := T.all.Func (I);  -- NOFLAG
   end Gen_Pkg_No_Subp;
end Main;

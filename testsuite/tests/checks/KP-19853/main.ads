package Main is
   function Process_Int (I : Integer) return Boolean is (I = 0);

   --  Test with tagged types

   type T_Pre is tagged null record;
   function F (V : T_Pre) return Integer is (0)
      with Pre'Class => Predicate (T_Pre'(Make));
   function Predicate (V : T_Pre) return Boolean is (False);
   function Make return T_Pre is (null record);

   type T_Pre_Pragma is tagged null record;
   function F (V : T_Pre_Pragma) return Integer is (0);
   pragma Pre_Class (Predicate (T_Pre_Pragma'(Make)));
   function Predicate (V : T_Pre_Pragma) return Boolean is (False);
   function Make return T_Pre_Pragma is (null record);

   type T_Post is tagged null record;
   function F (V : T_Post) return Integer is (0)
      with Post'Class => T_Post'(Make).Predicate;
   function Predicate (V : T_Post) return Boolean is (False);
   function Make return T_Post is (null record);

   type T_Post_Pragma is tagged null record;
   function F (V : T_Post_Pragma) return Integer is (0);
   pragma Post_Class (Predicate (T_Post_Pragma'(Make)));
   function Predicate (V : T_Post_Pragma) return Boolean is (False);
   function Make return T_Post_Pragma is (null record);

   type T_Post_Result is tagged record
      I : Integer;
   end record;
   function F (V : T_Post_Result) return Integer is (0)
      with Post'Class => T_Post_Result'(Make (F'Result)).I = 0;
   function Make (I : Integer) return T_Post_Result is ((I => I));

   type T_Pre_Ref_Formal is tagged record
      I : Integer;
   end record;
   function F (V : T_Pre_Ref_Formal; I : Integer) return Integer is (0)
      with Pre'Class => T_Pre_Ref_Formal'(Make (I)).I = 0;
   function Make (I : Integer) return T_Pre_Ref_Formal is ((I => I));

   type T_Pre_Ref_Prefix is tagged null record;
   function F (V : T_Pre_Ref_Prefix) return Integer is (0)
      with Pre'Class => V.Predicate;
   function Predicate (V : T_Pre_Ref_Prefix) return Boolean is (False);

   type T_Pre_No_Prim_Call is tagged null record;
   function F (V : T_Pre_No_Prim_Call) return Integer is (0)
      with Pre'Class => Process_Int (0);

   type T_Pre_No_Call is tagged null record;
   function F (V : T_Pre_No_Call) return Integer is (0)
      with Pre'Class => False;

   type T_Pre_Not_Class is tagged null record;
   function F (V : T_Pre_Not_Class) return Integer is (0)
      with Pre => Predicate (T_Pre_Not_Class'(Make));
   function Predicate (V : T_Pre_Not_Class) return Boolean is (False);
   function Make return T_Pre_Not_Class is (null record);

   type Child_Pre is new T_Pre with null record;                        --  FLAG
   type Child_Child_Pre is new Child_Pre with null record;              --  FLAG
   type Child_Pre_Pragma is new T_Pre_Pragma with null record;          --  FLAG
   type Child_Post is new T_Post with null record;                      --  FLAG
   type Child_Post_Pragma is new T_Post_Pragma with null record;        --  FLAG
   type Child_Post_Result is new T_Post_Result with null record;        --  NOFLAG
   type Child_Pre_Ref_Formal is new T_Pre_Ref_Formal with null record;  --  NOFLAG
   type Child_Pre_Ref_Prefix is new T_Pre_Ref_Prefix with null record;  --  NOFLAG
   type Child_No_Prim_Call is new T_Pre_No_Prim_Call with null record;  --  NOFLAG
   type Child_Pre_No_Call is new T_Pre_No_Call with null record;        --  NOFLAG
   type Child_Pre_Not_Class is new T_Pre_Not_Class with null record;    --  NOFLAG
end Main;

package Main is
   type Priv_Rec is private;

   type Rec is record
      I : Integer;
   end record;

   type Arr is array (1 .. 10) of Integer;

   task type Tsk is
   end Tsk;

   protected type Prot is
   end Prot;

   type Arr_Int is array (1 .. 10) of aliased Integer  --  NOFLAG
     with Pack;

   type Arr_1 is array (1 .. 10) of Rec;         --  NOFLAG
   type Arr_2 is array (1 .. 10) of aliased Rec; --  NOFLAG
   type Arr_3 is array (1 .. 10) of aliased Rec  --  FLAG
     with Pack;
   type Arr_1_New is new Arr_1 with Pack;        --  NOFLAG
   type Arr_2_New is new Arr_2 with Pack;        --  FLAG
   subtype Arr_3_Sub is Arr_3;                   --  FLAG
   type Arr_3_New is new Arr_3;                  --  FLAG

   type Arr_Comp_Size is array (1 .. 10) of aliased Rec  --  FLAG
     with Component_Size => 32;

   type Arr_Arr is array (1 .. 10) of aliased Arr    --  FLAG
     with Pack;
   type Arr_Task is array (1 .. 10) of aliased Tsk   --  FLAG
     with Pack;
   type Arr_Prot is array (1 .. 10) of aliased Prot  --  FLAG
     with Pack;
private
   type Priv_Rec is record
      I : Integer;
   end record;
end Main;

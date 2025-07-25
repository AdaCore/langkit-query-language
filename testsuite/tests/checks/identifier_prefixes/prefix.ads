package Prefix is
   --  type
   type Root;                                 --  NOFLAG
   type Root is new integer;                  --  FLAG
   type Priv is private;                      --  FLAG

   --  class_access
   type Type_Tag is tagged null record;      --  NOFLAG
   type CA_1 is access all Tag'Class;        --  NOFLAG
   type Type_CW is access all Tag'Class;     --  FLAG
   subtype CA_2 is CA_1;                     --  NOFLAG
   subtype Type_CA_3 is CA_1;                --  FLAG

   --  type and enum
   type Type_Enum_1 is (E_A1, E_B1, E_C1);
   type Enum_2      is                       --  FLAG
     (E_A2,
      B2);                                   --  FLAG

   --  enum via function renaming
   function Bad_Enum return Enum_2           --  FLAG
     renames E_A2;

   --  constant
   Const_C1 : constant Type_Enum_1 := E_A1;
   Const2   : constant Enum_2      := B2;    --  FLAG
   Private_Const : constant Priv;            --  FLAG

   --  constant via renames
   Const3   : Type_Enum_1 renames Const_C1;  --  FLAG

   --  formal decls
   generic
      Gen_Param       : Integer;             --  NOFLAG
      Const_Gen_Param : Integer;             --  FLAG
   package Gen_Pkg is
   end;

   --  exception
   X_Exc_1 : exception;
   Exc_2   : exception;                      --  FLAG

   --  derived type
   type Root_1 is new Root;
   type Root2 is new Root;                   --  FLAG
   type Root_1 is new Integer;               --  FLAG

   --  derived type via subtype
   subtype Type_Sub1 is Root;                --  NOFLAG
   subtype Type_Sub2 is Type_Sub1;           --  NOFLAG
   type Root_3 is new Type_Sub2;             --  NOFLAG
   type Root4 is new Type_Sub2;              --  FLAG

   --  concurrent type
   task type Type_Conc_Task is               --  NOFLAG
      entry E;
   end Type_Task;

   task type My_Task is                      --  FLAG
      entry E;
   end My_Task;

   protected type Type_Conc_Protected is     --  NOFLAG
      entry E;
   end Type_Protected;

   protected type My_Protected is            --  FLAG
      entry E;
   end My_Protected;

   type Type_Conc_Rec is null record;        --  FLAG
   type Type_Conc_Der is new Integer;        --  FLAG

   Const_Or_Node : Integer := 1;             --  FLAG
private
   type Priv is new Integer;                 --  NOFLAG: completion
   Private_Const : constant Priv := 0;       --  NOFLAG: completion
end Prefix;

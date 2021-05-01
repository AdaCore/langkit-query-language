--  +RIdentifier_Prefixes:
--  Type=Type_,Constant=Const_,Exception=X_,Enum=E_,Derived=Prefix.Root:Root_,
--  Class_Access=CA_

package Prefix is
   --  type
   type Root;                                 --  NO FLAG
   type Root is new integer;                  --  FLAG
   type Priv is private;                      --  FLAG

   --  class_access
   type Type_Tag is tagged null record;      --  NO FLAG
   type CA_1 is access all Tag'Class;        --  NO FLAG
   type Type_CW is access all Tag'Class;     --  FLAG
   subtype CA_2 is CA_1;                     --  NO FLAG
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

   --  exception
   X_Exc_1 : exception;
   Exc_2   : exception;                      --  FLAG

   --  derived type
   type Root_1 is new Root;
   type Root2 is new Root;                   --  FLAG

   --  derived type via subtype
   subtype Type_Sub1 is Root;                --  NO FLAG
   subtype Type_Sub2 is Type_Sub1;           --  NO FLAG
   type Root_3 is new Type_Sub2;             --  NO FLAG
   type Root4 is new Type_Sub2;              --  FLAG

   --  concurrent type
   task type Type_Task is                    --  NO FLAG
      entry E;
   end Type_Task;

   task type My_Task is                      --  FLAG
      entry E;
   end My_Task;

   protected type Type_Protected is          --  NO FLAG
      entry E;
   end Type_Protected;

   protected type My_Protected is            --  FLAG
      entry E;
   end My_Protected;

private
   type Priv is new Integer;                 --  NO FLAG: completion
   Private_Const : constant Priv := 0;       --  NO FLAG: completion
end Prefix;

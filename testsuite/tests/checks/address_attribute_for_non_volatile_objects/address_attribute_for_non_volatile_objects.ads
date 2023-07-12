package Address_Attribute_For_Non_Volatile_Objects is

   Var1 : Integer with Volatile;
   Var2 : Integer;

   X : Integer with Address => Var1'Address;   -- NOFLAG
   Y : Integer with Address => Var2'Address;   --  FLAG

   --  Address attribute specification clause:
   Var_Non_Volatile : Integer;
   for Var_Non_Volatile'Address use Var1'Address;     -- NOFLAG

   --  Constant object:

   Const : constant Integer := 123;
   Var   : Integer with Address => Const'Address;     -- NOFLAG

   --  Dereference:

   type Int_Access is access all Integer;
   Var_1 : aliased Integer;
   Var_2 : Int_Access := Var_1'Access;
   Var_3 : Integer with Address => Var_2.all'Address; -- NOFLAG

   --  Renaming of array slice (not a data object):

   type Arr is array (1 .. 10) of Integer;
   Var_Arr   : Arr;
   Var_Arr_R : Arr renames Var_Arr;
   Var_Arr_5 : Integer renames Var_Arr_R (5);
   V_Int_1 : Integer with Address => Var_Arr_5'Address;  -- NOFLAG

end Address_Attribute_For_Non_Volatile_Objects;

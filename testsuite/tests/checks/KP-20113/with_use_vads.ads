pragma Use_VADS_Size;

package With_Use_VADS is
   type Constr_Arr is array (1 .. 2) of Integer;
   type Constr_MD_Arr is array (1 .. 2, 1 .. 2) of Integer;
   subtype Constr_Sub is Constr_Arr;
   type Constr_Derived is new Constr_Arr;

   Constr_Arr_Size      : Integer := Constr_Arr'Size;       --  FLAG
   Constr_MD_Arr_Size   : Integer := Constr_MD_Arr'Size;    --  FLAG
   Constr_Arr_VADS_Size : Integer := Constr_Arr'VADS_Size;  --  FLAG
   Constr_Sub_Size      : Integer := Constr_Sub'Size;       --  FLAG
   Constr_Derived_Size  : Integer := Constr_Derived'Size;   --  FLAG

   type Unconstr_Arr is array (Integer range <>) of Integer;
   type Unconstr_MD_Arr is
     array (Integer range <>, Integer range <>) of Integer;
   subtype Unconstr_Sub is Unconstr_Arr (1 .. 2);
   subtype Unconstr_Sub_Sub is Unconstr_Sub;
   subtype Unconstr_MD_Sub is Unconstr_MD_Arr (1 .. 2, 1 .. 2);
   type Unconstr_Derived is new Unconstr_Arr (1 .. 2);

   Unconstr_Arr_Size     : Integer := Unconstr_Arr'Size;      --  NOFLAG
   Unconstr_Sub_Size     : Integer := Unconstr_Sub'Size;      --  FLAG
   Unconstr_Sub_Sub_Size : Integer := Unconstr_Sub_Sub'Size;  --  FLAG
   Unconstr_MD_Sub_Size  : Integer := Unconstr_MD_Sub'Size;   --  FLAG
   Unconstr_Derived_Size : Integer := Unconstr_Derived'Size;  --  FLAG

   subtype String_Sub is String (1 .. 2);

   Integer_Size    : Integer := Integer'Size;     --  NOFLAG
   String_Sub_Size : Integer := String_Sub'Size;  --  FLAG

   type Constr_Arr_Priv is private;

   procedure Test (X, Y : Integer);
private
   type Constr_Arr_Priv is array (1 .. 2) of Integer;
end With_Use_VADS;

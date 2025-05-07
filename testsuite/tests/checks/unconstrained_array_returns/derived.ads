package Derived is

   type Constrained is new String (1 .. 6);
   type D_Constrained is new Constrained;

   type Unconstrained is new String;
   type D_Unconstrained is new Unconstrained;

   function Get_Constrained return Constrained is ("123456");      --  NOFLAG
   function Get_D_Constrained return D_Constrained is ("123456");  --  NOFLAG

   function Get_Unconstrained return Unconstrained is ("");        --  FLAG
   function Get_D_Unconstrained return D_Unconstrained is ("");    --  FLAG

end Derived;

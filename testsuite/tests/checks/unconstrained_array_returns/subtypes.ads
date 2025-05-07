package Subtypes is

   subtype Constrained is String (1 .. 6);
   subtype S_Constrained is Constrained;

   subtype Unconstrained is String;
   subtype S_Unconstrained is Unconstrained;

   function Get_Constrained return Constrained is ("123456");      --  NOFLAG
   function Get_S_Constrained return S_Constrained is ("123456");  --  NOFLAG

   function Get_Unconstrained return Unconstrained is ("");        --  FLAG
   function Get_S_Unconstrained return S_Unconstrained is ("");    --  FLAG

end Subtypes;

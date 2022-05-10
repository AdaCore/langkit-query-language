package P is

   op : access function (arg1 : Integer; arg2 : Integer) return Integer  -- FLAG
   with Import => True,
        Convention => C,
        External_Name => "op";

   op2 : access function return Integer; -- FLAG
   pragma Convention (C, op2);

end P;

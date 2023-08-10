package Bool is

   type My_Bool is new Boolean;
   for My_Bool use (True => 0, False => 1);   --  FLAG

   type My_Bool2 is new My_Bool;
   for My_Bool2 use (11, 99);                 --  FLAG

   type My_Bool3 is new Boolean;
   for My_Bool3 use (True => 1, False => 0);  --  NOFLAG

   type My_Bool4 is new My_Bool3;
   for My_Bool4 use (0, 1);                   --  NOFLAG

end;

procedure P is

   Max_Error_Length : constant := 8;
   subtype Str is String (1 .. Max_Error_Length);

   type Rec is record
      Text : Str;
   end record;

   type Arr is array (1 .. 16) of Rec;

   Table : constant Arr :=
     (3 => (Text => "INVALID "), others => (Text => "OTHERS  "));  --  FLAG
   Table2 : constant Arr :=
     (1 => (Text => "  VALID "), 2 => (Text => "OTHERS  "));       --  NOFLAG

end;

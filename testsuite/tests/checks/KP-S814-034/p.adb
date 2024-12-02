procedure P is
   --  Ensure aggregates with no type don't trigger any error
   type Example_Enum is (A, B, C);
   for Example_Enum use (A => 0, B => 1, C => 2);                 --  NOFLAG

   Max_Error_Length : constant := 8;
   subtype Str is String (1 .. Max_Error_Length);

   S_Init : constant Str := "INIT    ";

   type Rec is tagged record
      Text : Str;
   end record;

   type D_Rec is new Rec with record
      I : Integer;
   end record;

   R_Init : constant Rec := (others => S_Init);

   type Arr is array (1 .. 16) of Rec;
   type D_Arr is array (1 .. 16) of D_Rec;
   type M_Arr is array (1 .. 2, 1 .. 2) of Rec;

   type Rec_Arr is record
      A : Arr := (others => (others => S_Init));                   --  FLAG
      M_A : M_Arr := (others => (others => R_Init));               --  FLAG
      D_A : D_Arr := (others => (Text => S_Init, I => 2));         --  FLAG
   end record;

   Table : constant Arr :=
     (3 => (Text => "INVALID "), others => (Text => "OTHERS  "));  --  FLAG
   Table2 : constant Arr :=
     (1 => (Text => "  VALID "), 2 => (Text => "OTHERS  "));       --  NOFLAG
   Table3 : constant Arr :=
     (1 => (others => "  VALID "), 2 => (others => "OTHERS  "));   --  NOFLAG
begin
   null;
end P;

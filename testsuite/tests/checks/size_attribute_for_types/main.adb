with Pack; use Pack;
procedure Main is
   I : Integer := Pack.Rec'Size;            --  FLAG
   J : Integer := Pack.S_Int'Size;          --  FLAG
   K : Integer := Pack.Var_Rec'Size;        -- NOFLAG
   L : Integer := Pack.Var_Rec.C1'Size;     -- NOFLAG
   M : Integer := Pack.Var_Int'Size;        -- NOFLAG
begin
   I := Rec'Size;            --  FLAG
   J := S_Int'Size;          --  FLAG
   K := Var_Rec'Size;        -- NOFLAG
   L := Var_Rec.C1'Size;     -- NOFLAG
   M := Var_Int'Size;        -- NOFLAG
end;

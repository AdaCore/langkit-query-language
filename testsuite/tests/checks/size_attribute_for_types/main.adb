with Pack; use Pack;
procedure Main is
   I : Integer := Pack.Rec'Size;            --  FLAG
   J : Integer := Pack.S_Int'Size;          --  FLAG
   K : Integer := Pack.Var_Rec'Size;        --  NO FLAG
   L : Integer := Pack.Var_Rec.C1'Size;     --  NO FLAG
   M : Integer := Pack.Var_Int'Size;        --  NO FLAG
begin
   I := Rec'Size;            --  FLAG
   J := S_Int'Size;          --  FLAG
   K := Var_Rec'Size;        --  NO FLAG
   L := Var_Rec.C1'Size;     --  NO FLAG
   M := Var_Int'Size;        --  NO FLAG
end;

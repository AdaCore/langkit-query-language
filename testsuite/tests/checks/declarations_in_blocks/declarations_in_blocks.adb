procedure Declaration_In_Blocks is
    I, J : Integer := 0;
begin
    if I /= J then
       declare                       --  FLAG
          Tmp : Integer;
       begin
          Tmp := I;
          I   := J;
          J   := Tmp;
       end;
    end if;
end Declaration_In_Blocks;

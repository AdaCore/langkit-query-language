with P; use P;
procedure Proc is
   I : Integer := Create;     --  FLAG

   package Inner is
      J : Integer := Create;  --  FLAG
   end Inner;

begin
   I := Create;               --  FLAG
   Create;
end;

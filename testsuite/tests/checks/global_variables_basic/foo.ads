package Foo is
    Const1 : constant Integer := 1;  -- NOFLAG
    Var1 : Integer;                  --  FLAG
    procedure Proc;
private
    Var2 : Boolean;                  --  FLAG
end Foo;


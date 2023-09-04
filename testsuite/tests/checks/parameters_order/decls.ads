package Decls is
    procedure Proc_Valid (B : Boolean; I : In out Integer);
    procedure Proc_Invalid_1 (Flag : in out Integer; B : Boolean);  --  FLAG
    procedure Proc_Invalid_2 (Flag: Boolean := True; B2 : Boolean);  --  FLAG
end Decls;

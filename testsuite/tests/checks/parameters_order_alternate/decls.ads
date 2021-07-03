package Decls is
    procedure Proc_Valid   --  NO FLAG
      (A : Boolean;
       B : Boolean := True;
       C : in out Integer;
       D : access Integer;
       E : out Integer);

    procedure Proc_Invalid_1
      (Flag : in out Integer;   --  FLAG
       B    : Boolean);

    procedure Proc_Invalid_2
      (Flag : Boolean := True;  --  FLAG
       B2   : Boolean);
end Decls;

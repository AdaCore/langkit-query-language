package Address_Specifications_For_Initialized_Objects is

    I : Integer := 0;
    Var0 : Integer with Address => I'Address;

    Var1 : Integer := 10;
    for Var1'Address use Var0'Address;                      --  FLAG

    Var2 : Integer := 30 with Var1'Address => Var0'Address; --  FLAG

end Address_Specifications_For_Initialized_Objects;

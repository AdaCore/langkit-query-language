package Address_Attribute_For_Non_Volatile_Objects is

    Var1 : Integer with Volatile;
    Var2 : Integer;

    X : Integer with Address => Var1'Address;
    Y : Integer with Address => Var2'Address;   --  FLAG

end Address_Attribute_For_Non_Volatile_Objects;

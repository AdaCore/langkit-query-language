with Interfaces, System, System.Storage_Elements;

package Foo is 
    Variable: Interfaces.Unsigned_8
        with Address => System.Storage_Elements.to_Address (0), Volatile;

    Variable1: Interfaces.Unsigned_8                                --  FLAG
        with Volatile;

    type My_Int is range 1 .. 32 with Volatile;

    Variable3 : My_Int;                                             --  FLAG

    Variable4 : My_Int
        with Address => Variable3'Address;
end Foo;

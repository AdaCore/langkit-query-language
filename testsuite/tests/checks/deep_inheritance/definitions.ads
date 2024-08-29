package Definitions is
    type I0 is interface;
    type I1 is interface and I0;
    type I2 is interface and I1;

    type T0 is tagged null record;
    type T1 is new T0 and I0 with null record;
    type T2 is new T0 and I1 with null record;
    type T3 is new T0 and I2 with null record; --  FLAG (if rule parameter is 2)
end Definitions;

procedure Main is
   type A_Int is access Integer;      --  FLAG (2)
   type Access_Int is access Integer; --  FLAG

   type My_Base is new Integer;       --  FLAG (2)

   type Derived is new My_Base;        --  FLAG (2)
   type Based_Derived is new My_Base;  --  FLAG

   X : Integer := 50;                  --  NOFLAG
begin
   null;
end Main;

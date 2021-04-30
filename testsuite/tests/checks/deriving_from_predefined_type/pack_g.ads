generic
   type FT1 is private;
   type FT2 is private;
   type FT3 is private;
package Pack_G is
   type T1 is new FT1;   --  FLAG
   type T2 is new FT2;   --  FLAG
   type T3 is private;
private
   type T3 is new FT3;   --  FLAG
end Pack_G;

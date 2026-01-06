procedure Main is
   package Test is
      type T1 is new Integer; -- NOFLAG
      type T2 is private;  -- FLAG
      type T3 is private;  -- NOFLAG
   private
      type T2 is new Integer;
      type T3 is new String;
      type T4 is new Integer;
   end Test;
begin
   null;
end Main;

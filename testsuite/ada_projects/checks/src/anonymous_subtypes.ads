package Anonymous_Subtypes is

   type T is range 0 .. 10; -- NO FLAG
   subtype S is Integer range 0 .. 10; -- NO FLAG

   function F (I : Integer)
               return Boolean is (I in 0 .. 10); -- FLAG
   function F2 (I : Integer)
                return Boolean is (case I is
                                      when 0 .. 10 => True, -- FLAG
                                      when others => False);
   function F3 (I : Integer)
                return Boolean is (for some J in 0 .. 10 => J > I); -- FLAG

end Anonymous_Subtypes;

package Anonymous_Subtypes is

   function F (I : Integer)
               return Boolean is (I in 0 .. 10); -- FLAG
   function F2 (I : Integer)
                return Boolean is (case I is
                                      when 0 .. 10 => True, -- FLAG
                                      when others => False);
   function F3 (I : Integer)
                return Boolean is (for some J in 0 .. 10 => J > I); -- FLAG

end Anonymous_Subtypes;
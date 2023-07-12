package Anonymous_Subtypes is

   type T is range 0 .. 10; -- NOFLAG
   type A_T is access Integer range 0 .. 10; -- FLAG
   subtype S is Integer range 0 .. 10; -- NOFLAG
   type T2 is new T range 1 .. 3; -- FLAG

   function F (I : Integer)
               return Boolean is (I in 0 .. 10); -- FLAG
   function F2 (I : Integer)
                return Boolean is (case I is
                                      when 0 .. 10 => True, -- FLAG
                                      when others => False);
   function F3 (I : Integer)
                return Boolean is (for some J in 0 .. 10 => J > I); -- FLAG

   type R (I : Integer) is record
      N : String (1 .. I);        -- NOFLAG
      N2 : Integer range 1 .. 10; -- FLAG

      case I is
      when 1 .. 10 => -- FLAG
         N3 : Integer;
      when others => null;
      end case;
   end record;

   type TA is array (Integer range 0 .. 10) of Integer; -- FLAG
   type TA_U is array (Integer range <>) of Integer; -- NOFLAG
   subtype TA_C is TA_U (1 .. 10); -- FLAG: index_constraint which is not a subtype_mark

   Var_Disc : R (1); -- FLAG
   subtype T_Disc is R (2); -- NOFLAG

   --  Self-referenced data structure:
   type T3;
   type T_False_Detection (Discrim : access T3) is null record;

   type T3 is limited record
      Y : T_False_Detection (T3'Access);         -- NOFLAG
   end record;

end Anonymous_Subtypes;

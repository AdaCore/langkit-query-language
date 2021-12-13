procedure Loop3 is
   function Junk (L, R : Integer) return Boolean is (True);
   function Junk (L, R : Integer) return Integer is (L - R);
begin
   Non_Predefined_Relation : declare
      function "<" (L, R : Integer) return Boolean is (L <= R);
      I : Integer := 1;
   begin
      while I < 10 loop                                          --  NO FLAG
         I := I - 1;
      end loop;
   end Non_Predefined_Relation;

   Non_Predefined_Increment : declare
      function "+" (L, R : Integer) return Integer is (L * R);
      I : Integer := 1;
   begin
      while I < 10 loop                                         --  NO FLAG
         I := I + 1;
      end loop;
   end Non_Predefined_Increment;

   Renamed_Relation : declare
      function "<" (L, R : Integer) return Boolean renames Junk;
      I : Integer := 1;
   begin
      while I < 10 loop                                          --  NO FLAG
         I := I - 1;
      end loop;
   end Renamed_Relation;

   Renamed_Increment : declare
      function "+" (L, R : Integer) return Integer renames Junk;
      I : Integer := 1;
   begin
      while I < 10 loop                                         --  NO FLAG
         I := I + 1;
      end loop;
   end Renamed_Increment;

end Loop3;

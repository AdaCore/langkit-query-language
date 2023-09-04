procedure Nested is   --  level 0
begin
   declare  --  level 1
      package P is   --  level 2
         package P1 is   --  level 3
            task type T1 is   --  FLAG level 4
            end T1;
            task body T1 is   --  FLAG level 4
            begin
               null;
            end T1;
         end P1;
      end P;
   begin
      begin  --  level 2
         begin  --  level 3
            begin  --  FLAG level 4
               null;
            end;
         end;
      end;
   end;
end;

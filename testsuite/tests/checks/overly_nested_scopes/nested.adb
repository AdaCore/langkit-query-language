procedure Nested is   --  level 0
begin
   declare  --  level 1
      package P is   --  level 2
         package P1 is   --  level 3
            task type T1 is   --  level 4: FLAG
            end T1;
            task body T1 is   --  level 4: FLAG
            begin
               null;
            end T1;
         end P1;
      end P;
   begin
      begin  --  level 2
         begin  --  level 3
            begin  --  level 4: FLAG
               null;
            end;
         end;
      end;
   end;
end;

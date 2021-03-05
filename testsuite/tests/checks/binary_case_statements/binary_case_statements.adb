package body Binary_Case_Statements is
    procedure body P is
    begin
        case 1 is                   --  FLAG
           when 1 =>
              null;
           when others =>
              null;
        end case;
    end P;
end Binary_Case_Statements;

with Ada.Text_IO; use Ada.Text_IO;  --  FLAG (3)

procedure Main is                   --  FLAG (3)
begin
    Put_Line ("Hello!");
    goto lbl                        --  FLAG (2)

    <<lbl>>
end Main;

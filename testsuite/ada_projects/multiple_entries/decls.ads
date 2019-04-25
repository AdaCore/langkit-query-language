package P is

protected P0 is
    entry Get (I: Integer);
    entry Put (I: out Integer);
    procedure Reset;
    function Check return Boolean;
private
    Val : Integer := 0;
end P0;

end P;  

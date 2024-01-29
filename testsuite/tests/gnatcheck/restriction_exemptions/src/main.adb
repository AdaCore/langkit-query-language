package body Main is

    --------------------
    -- Protected_Type --
    --------------------

    protected body Protected_Type is
        entry Hello when True is
        begin
            null;
        end Hello;
    end Protected_Type;
end Main;

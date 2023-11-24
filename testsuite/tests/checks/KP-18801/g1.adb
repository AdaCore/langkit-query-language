package body G1 is

  function Get return String is
  begin
     return "";
  end Get;

  function Get return Natural is
  begin
    if Get = "" then           --  FLAG
      return 0;
    elsif "=" ("", Get) then   --  FLAG
      return 0;
    else
      return 1;
    end if;
  end Get;

  function Get return Q.S is
  begin
    return Q.None;
  end Get;

end G1;

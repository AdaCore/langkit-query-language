--  User-defined relation and equality operators, nothing should be flagged
--  here.

procedure User is
   package P is
      type T is private;
      function "=" (L, R : T) return Boolean;
   private
      type T is new Boolean;
   end P;

   package body P is
      Var : T;
      K   : Integer;
      function "=" (L, R : T) return Boolean is (True);
   begin
      if Var = True then  --  NO FLAG
         K := 1;
      end if;
   end P;

   I : Integer;
   function "<" (L, R : Integer) return Boolean is (L <= R);
begin
   if not (I < 1) then    --  NO FLAG
      null;
   end if;
end User;

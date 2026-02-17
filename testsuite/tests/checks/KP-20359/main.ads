package Main is
   type T is tagged null record;
   type T_Not_Tagged is record
      I : Integer;
   end record;

   type D_T is new T with record
      I : Integer;
   end record;

   type D_D_T is new D_T with record
      J : Integer;
   end record;

   subtype S_D_T is D_T;

   type P_D_T is private;

   type D_T_Not_Tagged is new T_Not_Tagged;

   procedure Test;
private
   type P_D_T is new T with record
      I : Integer;
   end record;
end Main;

function P return Boolean is
   type Event is interface;

   type Registered_Event is new Event with record
      A : Integer := 5;
   end record;

   function Get_Event return Event'Class with Import;

   Pp : Event'Class := Get_Event;

begin
   return Pp in Registered_Event'Class;   --  NOFLAG
   return Pp in Registered_Event;         --  FLAG
end P;

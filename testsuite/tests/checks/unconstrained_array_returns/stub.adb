procedure Stub is
   --  Function body stubs are flagged, unless they have a
   --  separate specification (as Subp1).

   function Subp1 (I : Integer) return String; -- FLAG
   function Subp1 (I : Integer) return String is separate; -- NO FLAG

   function Subp2 (I : Integer) return String is separate; -- FLAG

begin
   null;
end Stub;

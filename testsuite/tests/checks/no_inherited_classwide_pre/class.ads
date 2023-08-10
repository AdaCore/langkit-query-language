package Class is

   type Int is interface;
   function Test (X : Int) return Boolean is abstract;
   procedure Proc (I : in out Int) is abstract with Pre'Class => Test (I);

   type Int1 is interface;
   procedure Proc (I : in out Int1) is abstract;

   type T is tagged private;

   type NT1 is new T and Int with private;
   function Test (X : NT1) return Boolean;        --  FLAG
   procedure Proc (X : in out NT1);               --  NOFLAG

   type NT2 is new T and Int1 with private;
   procedure Proc (X : in out NT2);               --  FLAG

private
   type T is tagged record
      I : Integer;
   end record;

   type NT1 is new T and Int with null record;
   type NT2 is new T and Int1 with null record;

end Class;

procedure Main is
   type T is null record
     with Iterable =>  --  FLAG
       (First => First, Next => Next, Has_Element => Has_Element);
   function First (X : T) return T is (X);
   function Next (X : T; Y : T) return T is (X);
   function Has_Element (X : T; Y : T) return Boolean is (False);
begin
   goto Test;  --  FLAG
   <<Test>>
end Main;

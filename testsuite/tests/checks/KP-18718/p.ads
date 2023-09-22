package P is
   type Enum is (Blue, Yellow);
   type Base (Content : Enum := Blue) is record
      case Content is
         when Blue =>
            null;
         when Yellow =>
            Data : Integer;
      end case;
   end record;
   subtype Discr is Base;

   function Work return Discr;
   function Work return Integer with Inline;

end P;

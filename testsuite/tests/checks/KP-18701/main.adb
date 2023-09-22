procedure Main is

  type Something (Discriminate : Boolean := False) is limited null record;

  type Something2 (Discriminate : Boolean := False) is record
     Field : Something;
  end record;

  type Op is record -- implicitly limited because of limited element
    Element : Something;
  end record;

  type Op2 is record -- implicitly limited because of limited element
    Element : Something2;
  end record;

  Con  : constant Op  := (Element => (Discriminate => True));               --  FLAG
  Con2 : constant Op2 := (Element => (Discriminate => True, Field => <>));  --  FLAG

begin
  null;
end Main;

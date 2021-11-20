package Small is

  type T is private;

private

  type T is delta 0.1 range -1.0 .. 1.0 with Small => 0.1;

end Small;

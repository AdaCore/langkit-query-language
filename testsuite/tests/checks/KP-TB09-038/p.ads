package P is

  type Biased_T is range 1 .. 2 ** 6;      --  FLAG
  for Biased_T'Size use 6;

  type Non_Biased_T is range 1 .. 2 ** 6;  --  NO FLAG
  for Non_Biased_T'Size use 7;

end P;

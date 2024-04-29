with P;

package Q is
   type T is range 1 .. 5;

   subtype U is T;

   X : constant P.T := 2;
end Q;

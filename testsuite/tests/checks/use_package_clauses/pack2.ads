with Pack1;
use Pack1;   --  FLAG

package Pack2 is
   use Pack1;          --  FLAG
   use type Pack1.T;   -- NOFLAG
end Pack2;

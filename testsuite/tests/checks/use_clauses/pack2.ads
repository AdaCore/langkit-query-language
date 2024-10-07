with Pack, Operator_Pack;
use Pack, Pack;             --  FLAG (2)
use Operator_Pack;          --  NOFLAG because Exempt_Operator_Packages is set

with Ada.Text_IO; use Ada.Text_IO;  --  NOFLAG because allowed by the rule param
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;  --  NOFLAG

with Ada.Strings.Wide_Unbounded; use Ada.Strings.Wide_Unbounded;  --  FLAG

package Pack2 is
end Pack2;

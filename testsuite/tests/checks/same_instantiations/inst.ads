with Gen;

package Inst1 is
   package Inst_1 is new Gen (Integer, 2);  --  FLAG
   package Inst_2 is new Gen (Integer, 3);  --  NOFLAG
end Inst1;

with Gen2;
package Inst3 is
    package Inst_A is new Gen2 (Integer, Y => 2);  -- NOFLAG
    package Inst_B is new Gen2 (Integer, 2);       -- NOFLAG
end;

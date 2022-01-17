with Gen2;
package Inst3 is
    package Inst_A is new Gen2 (Integer, Y => 2);  --  NO FLAG
    package Inst_B is new Gen2 (Integer, 2);       --  NO FLAG
end;

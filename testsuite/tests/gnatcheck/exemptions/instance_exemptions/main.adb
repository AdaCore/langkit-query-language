procedure Main is
   type T is new Integer;          --  FLAG
   type T_T is new Integer;        --  NOFLAG

   C : constant Integer := 0;      --  FLAG
   C_C : constant Integer := 0;    --  NOFLAG

   pragma Annotate (Gnatcheck, Exempt_On, "ID_SUF: constant", "testing");
   type T_2 is new Integer;        --  FLAG
   C_2 : constant Integer := 0;    --  NOFLAG (exempted)
   pragma Annotate (Gnatcheck, Exempt_On, "ID_SUF", "testing");                  --  FLAG (already exempted with params)
   pragma Annotate (Gnatcheck, Exempt_On, "ID_SUF: constant", "testing");        --  FLAG (already exempted with same params)
   pragma Annotate (Gnatcheck, Exempt_On, "ID_SUF: constant, type", "testing");  --  FLAG (already exempted with the "constant" param)
   pragma Annotate (Gnatcheck, Exempt_On, "ID_SUF: type", "testing");
   type T_3 is new Integer;        --  NOFLAG (exempted)
   pragma Annotate (Gnatcheck, Exempt_Off, "ID_SUF: type");
   pragma Annotate (Gnatcheck, Exempt_Off, "ID_SUF: constant");
begin
   null;
   null;                           --  FLAG

   goto lbl;                       --  FLAG (2)
   if True then goto lbl; end if;  --  FLAG

   if (True) then null; end if;    --  FLAG

   --## rule off Not_Existant ## testing  --  FLAG (wrong exempted name)

   --## rule off STYLE ## testing         --  FLAG (cannot exempt compiler based instance)

   --## rule off Redundant_Null_Statements ## testing
   null;  --  NOFLAG (exempted)
   --## rule on Redundant_Null_Statements

   --## rule off NULL ## testing
   null;  --  NOFLAG (exempted)
   --## rule on NULL

   --## rule off Goto_Statements ## testing
   goto lbl;                       --  NOFLAG (exempted)
   if True then goto lbl; end if;  --  NOFLAG (exempted)
   --## rule on Goto_Statements

   --## rule off GOTO ## testing
   goto lbl;                       --  FLAG (1 real, 1 exempted)
   if True then goto lbl; end if;  --  NOFLAG (exempted)
   --## rule on GOTO

   --## rule off UNCOND_GOTO ## testing
   goto lbl;                       --  FLAG (1 real, 1 exempted)
   if True then goto lbl; end if;  --  FLAG
   --## rule on UNCOND_GOTO

   --## rule off style_checks ## testing
   if (True) then null; end if;  --  NOFLAG (exempted)
   --## rule on style_checks

   --## rule off Goto_Statements ## testing
   --## rule off GOTO ## testing         --  FLAG (rule already exempted)
   --## rule off UNCOND_GOTO ## testing  --  FLAG (rule already exempted)
   --## rule on Goto_Statements          --  FLAG (no detection)

   --## rule off GOTO ## testing
   --## rule off Goto_Statements ## testing
   --## rule off GOTO ## testing  --  FLAG (instance already exempted)
   --## rule on Goto_Statements   --  FLAG (no detection)
   --## rule on GOTO              --  FLAG (no detection)

   pragma Annotate (Gnatcheck, Exempt_On, "Style_Checks: xz", "testing");
   if (True) then null; end if;                                            --  NOFLAG (exempted)
   pragma Annotate (Gnatcheck, Exempt_On, "Style_Checks", "testing");      --  FLAG (already exempted with params)
   pragma Annotate (Gnatcheck, Exempt_On, "Style_Checks: x", "testing");   --  FLAG (already exempted with the "x" param)
   pragma Annotate (Gnatcheck, Exempt_On, "Style_Checks: xz", "testing");  --  FLAG (already exempted with the same params)
   pragma Annotate (Gnatcheck, Exempt_Off, "Style_Checks: xz");

   <<lbl>>
end Main;

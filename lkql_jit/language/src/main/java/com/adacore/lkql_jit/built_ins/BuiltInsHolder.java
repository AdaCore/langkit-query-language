/*----------------------------------------------------------------------------
--                             L K Q L   J I T                              --
--                                                                          --
--                     Copyright (C) 2022-2023, AdaCore                     --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/.>                                          --
----------------------------------------------------------------------------*/

package com.adacore.lkql_jit.built_ins;

import com.adacore.lkql_jit.built_ins.functions.*;
import com.adacore.lkql_jit.built_ins.methods.*;
import com.adacore.lkql_jit.built_ins.selectors.*;
import com.adacore.lkql_jit.utils.LKQLTypesHelper;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * This class is a data class helper containing all built-ins of LKQL.
 *
 * @author Hugo GUERRIER
 */
public final class BuiltInsHolder {
    /** The built-in function list. */
    public final List<BuiltInFunctionValue> builtInFunctions =
            List.of(
                    PrintFunction.getValue(),
                    ImgFunction.getValue(),
                    BaseNameFunction.getValue(),
                    ConcatFunction.getValue(),
                    ReduceFunction.getValue(),
                    MapFunction.getValue(),
                    UniqueFunction.getValue(),
                    DocFunction.getValue(),
                    ProfileFunction.getValue(),
                    HelpFunction.getValue(),
                    UnitsFunction.getValue(),
                    SpecifiedUnitsFunction.getValue(),
                    PatternFunction.getValue(),
                    NodeCheckerFunction.getValue(),
                    UnitCheckerFunction.getValue());

    /** The built-in selector list. */
    public final List<BuiltInSelector> builtInSelectors =
            List.of(
                    ChildrenSelector.getInstance(),
                    ParentSelector.getInstance(),
                    NextSiblingsSelector.getInstance(),
                    PrevSiblingsSelector.getInstance(),
                    SuperTypesSelector.getInstance());

    /** The built-in method list. */
    public final Map<String, Map<String, BuiltInFunctionValue>> builtInMethods =
            Map.ofEntries(
                    create(LKQLTypesHelper.LKQL_LIST, ListMethods.methods),
                    create(LKQLTypesHelper.LKQL_STRING, StrMethods.methods),
                    create(LKQLTypesHelper.ADA_NODE, NodeMethods.methods),
                    create(LKQLTypesHelper.ANALYSIS_UNIT, AnalysisUnitMethods.methods),
                    create(LKQLTypesHelper.TOKEN, TokenMethods.methods),
                    create(LKQLTypesHelper.LKQL_LAZY_LIST, IterableMethods.methods),
                    create(LKQLTypesHelper.LKQL_SELECTOR_LIST, IterableMethods.methods),
                    create(LKQLTypesHelper.LKQL_UNIT, null),
                    create(LKQLTypesHelper.LKQL_BOOLEAN, null),
                    create(LKQLTypesHelper.LKQL_INTEGER, null),
                    create(LKQLTypesHelper.LKQL_FUNCTION, null),
                    create(LKQLTypesHelper.LKQL_PROPERTY_REF, null),
                    create(LKQLTypesHelper.LKQL_SELECTOR, null),
                    create(LKQLTypesHelper.LKQL_TUPLE, null),
                    create(LKQLTypesHelper.LKQL_OBJECT, null),
                    create(LKQLTypesHelper.LKQL_NAMESPACE, null));

    public final Map<String, BuiltInFunctionValue> commonMethods =
            Map.of(
                    ImgFunction.NAME, ImgFunction.getValue(),
                    PrintFunction.NAME, PrintFunction.getValue(),
                    DocFunction.NAME, DocFunction.getValue());

    public static Map<String, BuiltInFunctionValue> combine(
            Map<String, BuiltInFunctionValue> m1, Map<String, BuiltInFunctionValue> m2) {
        var res = new HashMap<String, BuiltInFunctionValue>();
        res.putAll(m1);
        res.putAll(m2);
        return res;
    }

    private static Map.Entry<String, Map<String, BuiltInFunctionValue>> create(
            String name, Map<String, BuiltInFunctionValue> vals) {
        if (vals == null) {
            vals = new HashMap<>();
        }
        return Map.entry(name, vals);
    }

    private static BuiltInsHolder instance = null;

    public static BuiltInsHolder get() {
        if (instance == null) {
            instance = new BuiltInsHolder();
        }
        return instance;
    }
}

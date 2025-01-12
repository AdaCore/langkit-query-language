//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.built_ins;

import com.adacore.lkql_jit.built_ins.functions.*;
import com.adacore.lkql_jit.built_ins.methods.*;
import com.adacore.lkql_jit.checker.built_ins.NodeCheckerFunction;
import com.adacore.lkql_jit.checker.built_ins.UnitCheckerFunction;
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
                    DocumentBuiltins.getValue(),
                    DocumentNamespace.getValue(),
                    HelpFunction.getValue(),
                    UnitsFunction.getValue(),
                    SpecifiedUnitsFunction.getValue(),
                    PatternFunction.getValue(),
                    NodeCheckerFunction.getValue(),
                    UnitCheckerFunction.getValue());

    /** The built-in method list. */
    public final Map<String, Map<String, BuiltInMethodFactory>> builtInMethods =
            Map.ofEntries(
                    create(LKQLTypesHelper.LKQL_LIST, ListMethods.methods),
                    create(LKQLTypesHelper.LKQL_STRING, StrMethods.methods),
                    create(LKQLTypesHelper.ADA_NODE, NodeMethods.methods),
                    create(LKQLTypesHelper.ANALYSIS_UNIT, AnalysisUnitMethods.methods),
                    create(LKQLTypesHelper.TOKEN, TokenMethods.methods),
                    create(LKQLTypesHelper.LKQL_LAZY_LIST, IterableMethods.methods),
                    create(LKQLTypesHelper.LKQL_SELECTOR_LIST, IterableMethods.methods),
                    create(LKQLTypesHelper.REWRITING_CONTEXT, RewritingContextMethods.methods),
                    create(LKQLTypesHelper.REWRITING_NODE, RewritingNodeMethods.methods),
                    create(LKQLTypesHelper.LKQL_UNIT, null),
                    create(LKQLTypesHelper.LKQL_BOOLEAN, null),
                    create(LKQLTypesHelper.LKQL_INTEGER, null),
                    create(LKQLTypesHelper.LKQL_FUNCTION, null),
                    create(LKQLTypesHelper.LKQL_PROPERTY_REF, null),
                    create(LKQLTypesHelper.LKQL_SELECTOR, null),
                    create(LKQLTypesHelper.LKQL_TUPLE, null),
                    create(LKQLTypesHelper.LKQL_OBJECT, null),
                    create(LKQLTypesHelper.LKQL_NAMESPACE, null));

    public final Map<String, BuiltInMethodFactory> commonMethods =
            Map.of(
                    ImgFunction.NAME,
                    BuiltInMethodFactory.fromFunctionValue(ImgFunction.getValue(), true),
                    PrintFunction.NAME,
                    BuiltInMethodFactory.fromFunctionValue(PrintFunction.getValue(), false),
                    DocFunction.NAME,
                    BuiltInMethodFactory.fromFunctionValue(DocFunction.getValue(), true));

    public static Map<String, BuiltInMethodFactory> combine(
            Map<String, BuiltInMethodFactory> m1, Map<String, BuiltInMethodFactory> m2) {
        var res = new HashMap<String, BuiltInMethodFactory>();
        res.putAll(m1);
        res.putAll(m2);
        return res;
    }

    private static Map.Entry<String, Map<String, BuiltInMethodFactory>> create(
            String name, Map<String, BuiltInMethodFactory> vals) {
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

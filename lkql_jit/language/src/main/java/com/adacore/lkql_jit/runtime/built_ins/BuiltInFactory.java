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

package com.adacore.lkql_jit.runtime.built_ins;

import com.adacore.lkql_jit.langkit_translator.passes.framing_utils.ScriptFramesBuilder;
import com.adacore.lkql_jit.runtime.GlobalScope;
import com.adacore.lkql_jit.runtime.built_ins.functions.*;
import com.adacore.lkql_jit.runtime.built_ins.methods.*;
import com.adacore.lkql_jit.runtime.built_ins.selectors.*;
import java.util.ArrayList;
import java.util.List;

/**
 * This class is a helper to add all built-ins of LKQL in the context and scope.
 *
 * @author Hugo GUERRIER
 */
public final class BuiltInFactory {

    // ----- Attributes -----

    /** The only instance of the built-in factory. */
    private static BuiltInFactory instance = null;

    /** The built-in function list. */
    private final List<BuiltInFunctionValue> builtInFunctions;

    /** The built-in selector list. */
    private final List<BuiltInSelector> builtInSelectors;

    /** The built-in method list. */
    private final List<BuiltInMethods> builtInMethods;

    // ----- Static initializer -----

    /** Create a build in factory as a singleton. */
    private BuiltInFactory() {
        this.builtInFunctions = new ArrayList<>();
        this.builtInSelectors = new ArrayList<>();
        this.builtInMethods = new ArrayList<>();

        this.initializeFunctions();
        this.initializeSelectors();
        this.initializeMethods();
    }

    /**
     * Get the only built-in factory instance.
     *
     * @return The factory instance.
     */
    public static BuiltInFactory getInstance() {
        if (instance == null) {
            instance = new BuiltInFactory();
        }
        return instance;
    }

    /** Initialize the built-in functions. */
    private void initializeFunctions() {
        this.builtInFunctions.add(PrintFunction.getValue());
        this.builtInFunctions.add(ImgFunction.getValue());
        this.builtInFunctions.add(BaseNameFunction.getValue());
        this.builtInFunctions.add(ConcatFunction.getValue());
        this.builtInFunctions.add(ReduceFunction.getValue());
        this.builtInFunctions.add(MapFunction.getValue());
        this.builtInFunctions.add(UniqueFunction.getValue());
        this.builtInFunctions.add(DocFunction.getValue());
        this.builtInFunctions.add(ProfileFunction.getValue());
        this.builtInFunctions.add(HelpFunction.getValue());
        this.builtInFunctions.add(UnitsFunction.getValue());
        this.builtInFunctions.add(SpecifiedUnitsFunction.getValue());
        this.builtInFunctions.add(PatternFunction.getValue());
        this.builtInFunctions.add(NodeCheckerFunction.getValue());
        this.builtInFunctions.add(UnitCheckerFunction.getValue());
    }

    /** Initialize the built-in selectors. */
    private void initializeSelectors() {
        this.builtInSelectors.add(ChildrenSelector.getInstance());
        this.builtInSelectors.add(ParentSelector.getInstance());
        this.builtInSelectors.add(NextSiblingsSelector.getInstance());
        this.builtInSelectors.add(PrevSiblingsSelector.getInstance());
        this.builtInSelectors.add(SuperTypesSelector.getInstance());
    }

    /** Initialize the built-in methods. */
    private void initializeMethods() {
        this.builtInMethods.add(UnitMethods.getInstance());
        this.builtInMethods.add(BoolMethods.getInstance());
        this.builtInMethods.add(IntMethods.getInstance());
        this.builtInMethods.add(StrMethods.getInstance());
        this.builtInMethods.add(FunctionMethods.getInstance());
        this.builtInMethods.add(PropertyRefMethods.getInstance());
        this.builtInMethods.add(SelectorMethods.getInstance());
        this.builtInMethods.add(TupleMethods.getInstance());
        this.builtInMethods.add(ListMethods.getInstance());
        this.builtInMethods.add(SelectorListMethods.getInstance());
        this.builtInMethods.add(LazyListMethods.getInstance());
        this.builtInMethods.add(ObjectMethods.getInstance());
        this.builtInMethods.add(NamespaceMethods.getInstance());
        this.builtInMethods.add(NodeMethods.getInstance());
        this.builtInMethods.add(TokenMethods.getInstance());
        this.builtInMethods.add(AnalysisUnitMethods.getInstance());
    }

    // ----- Getters -----

    /**
     * Get the number of built-in occupied slots (functions + selectors).
     *
     * @return The number of reserved slots.
     */
    public int getNbBuiltInFunctions() {
        return this.builtInFunctions.size() + this.builtInSelectors.size();
    }

    // ----- Instance methods -----

    /**
     * Add all LKQL built-ins to the given context global values.
     *
     * @param globalValues The global value object to put the built-ins in.
     */
    public void addBuiltIns(GlobalScope globalValues) {
        // Add the built-in functions
        for (int i = 0; i < this.builtInFunctions.size(); i++) {
            BuiltInFunctionValue function = this.builtInFunctions.get(i);
            globalValues.setBuiltIn(i, function);
        }

        // Add the built-in selectors
        for (int i = 0; i < this.builtInSelectors.size(); i++) {
            BuiltInSelector selector = this.builtInSelectors.get(i);
            globalValues.setBuiltIn(i + this.builtInFunctions.size(), selector.getValue());
        }

        // Add the built-in methods
        for (BuiltInMethods builtInMethods : this.builtInMethods) {
            globalValues.putMetaTable(builtInMethods.getType(), builtInMethods.getMethods());
        }
    }

    /**
     * Add the built ins to the script frames builder.
     *
     * @param scriptFramesBuilder The script frames builder to add the built-ins in.
     */
    public void addBuiltIns(final ScriptFramesBuilder scriptFramesBuilder) {
        // Add the built-in functions
        for (BuiltInFunctionValue function : this.builtInFunctions) {
            scriptFramesBuilder.addBuiltIn(function.getName());
        }

        // Add the built-in selectors
        for (BuiltInSelector selector : this.builtInSelectors) {
            scriptFramesBuilder.addBuiltIn(selector.getName());
        }
    }
}

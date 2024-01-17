//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.built_ins.selectors;

import com.adacore.lkql_jit.built_ins.BuiltInSelectorValue;
import com.adacore.lkql_jit.built_ins.values.LKQLSelector;
import com.adacore.lkql_jit.nodes.Identifier;
import com.adacore.lkql_jit.nodes.declarations.selector.SelectorArm;
import com.adacore.lkql_jit.nodes.declarations.selector.SelectorExpr;
import com.adacore.lkql_jit.nodes.expressions.dot.DotAccess;
import com.adacore.lkql_jit.nodes.expressions.dot.DotAccessNodeGen;
import com.adacore.lkql_jit.nodes.expressions.literals.UnitLiteral;
import com.adacore.lkql_jit.nodes.patterns.UniversalPattern;
import com.adacore.lkql_jit.nodes.patterns.node_patterns.NodeKindPattern;

/**
 * This class represents the "parent" built-in selector.
 *
 * @author Hugo GUERRIER
 */
public final class ParentSelector implements BuiltInSelector {

    // ----- Attributes -----

    /** The only instance of the "parent" built-in selector. */
    private static ParentSelector instance = null;

    /** The name of the selector. */
    public static final String NAME = "parent";

    /** The arms representing the "parent" selector execution. */
    public final SelectorArm[] arms;

    // ----- Constructors -----

    /** Private constructor. */
    private ParentSelector() {
        this.arms = this.createArms();
    }

    /**
     * Get the only instance of the built-in selector.
     *
     * @return The instance of the selector.
     */
    public static ParentSelector getInstance() {
        if (instance == null) {
            instance = new ParentSelector();
        }
        return instance;
    }

    // ----- Override methods -----

    /**
     * @see BuiltInSelector#getName()
     */
    @Override
    public String getName() {
        return NAME;
    }

    /**
     * @see BuiltInSelector#getValue()
     */
    @Override
    public LKQLSelector getValue() {
        return new BuiltInSelectorValue(
                NAME, "Yields the parents (ancestors) of the given node in the tree\n", this.arms);
    }

    // ----- Class methods -----

    /**
     * Create the selector arms and return them.
     *
     * @return The selector arms.
     */
    private SelectorArm[] createArms() {
        // Prepare the result
        SelectorArm[] res = new SelectorArm[2];

        // Create the children path
        DotAccess toUnpack =
                DotAccessNodeGen.create(
                        null, new Identifier(null, "parent"), new ReadBuiltInThis());
        SelectorArm parentPath =
                new SelectorArm(
                        null,
                        new NodeKindPattern(null, "AdaNode"),
                        new SelectorExpr(null, SelectorExpr.Mode.REC, toUnpack, false));
        res[0] = parentPath;

        // Create the universal path
        SelectorArm universalPath =
                new SelectorArm(
                        null,
                        new UniversalPattern(null),
                        new SelectorExpr(
                                null, SelectorExpr.Mode.DEFAULT, new UnitLiteral(null), false));
        res[1] = universalPath;

        // Return the result
        return res;
    }
}

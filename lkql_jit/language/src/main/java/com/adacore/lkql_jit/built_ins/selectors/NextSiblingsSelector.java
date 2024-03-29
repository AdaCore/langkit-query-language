//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.built_ins.selectors;

import com.adacore.lkql_jit.built_ins.BuiltInSelectorValue;
import com.adacore.lkql_jit.built_ins.values.LKQLSelector;
import com.adacore.lkql_jit.nodes.Identifier;
import com.adacore.lkql_jit.nodes.arguments.Arg;
import com.adacore.lkql_jit.nodes.arguments.ArgList;
import com.adacore.lkql_jit.nodes.declarations.selector.SelectorArm;
import com.adacore.lkql_jit.nodes.declarations.selector.SelectorExpr;
import com.adacore.lkql_jit.nodes.expressions.FunCall;
import com.adacore.lkql_jit.nodes.expressions.FunCallNodeGen;
import com.adacore.lkql_jit.nodes.expressions.dot.DotAccess;
import com.adacore.lkql_jit.nodes.expressions.dot.DotAccessNodeGen;
import com.adacore.lkql_jit.nodes.expressions.literals.UnitLiteral;
import com.adacore.lkql_jit.nodes.patterns.UniversalPattern;
import com.adacore.lkql_jit.nodes.patterns.node_patterns.NodeKindPattern;

/**
 * This class represents the "next_siblings" built-in selector.
 *
 * @author Hugo GUERRIER
 */
public final class NextSiblingsSelector implements BuiltInSelector {

    // ----- Attributes -----

    /** The only instance of the "next_siblings" built-in selector. */
    private static NextSiblingsSelector instance = null;

    /** The name of the selector. */
    public static final String NAME = "next_siblings";

    /** The arms representing the "next_siblings" selector execution. */
    public final SelectorArm[] arms;

    // ----- Constructors -----

    /** Private constructor. */
    private NextSiblingsSelector() {
        this.arms = createArms();
    }

    /**
     * Get the only instance of the built-in selector.
     *
     * @return The instance of the selector.
     */
    public static NextSiblingsSelector getInstance() {
        if (instance == null) {
            instance = new NextSiblingsSelector();
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
                NAME, "Yields the siblings following the given node in the tree\n", this.arms);
    }

    // ----- Class methods -----

    /**
     * Create the selector arms and return them.
     *
     * @return The selector arms.
     */
    private static SelectorArm[] createArms() {
        // Create the selector arms
        SelectorArm[] res = new SelectorArm[2];

        // Create the next sibling path
        DotAccess propertyAccess =
                DotAccessNodeGen.create(
                        null, new Identifier(null, "next_sibling"), new ReadBuiltInThis());
        FunCall funCall =
                FunCallNodeGen.create(
                        null, false, null, new ArgList(null, new Arg[0]), propertyAccess);
        SelectorArm nextSiblingPath =
                new SelectorArm(
                        null,
                        new NodeKindPattern(null, "AdaNode"),
                        new SelectorExpr(null, SelectorExpr.Mode.REC, funCall));
        res[0] = nextSiblingPath;

        // Create the universal pattern
        SelectorArm universalPath =
                new SelectorArm(
                        null,
                        new UniversalPattern(null),
                        new SelectorExpr(null, SelectorExpr.Mode.DEFAULT, new UnitLiteral(null)));
        res[1] = universalPath;

        // Return the result
        return res;
    }
}

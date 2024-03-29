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
import com.adacore.lkql_jit.nodes.expressions.Unpack;
import com.adacore.lkql_jit.nodes.expressions.dot.DotAccess;
import com.adacore.lkql_jit.nodes.expressions.dot.DotAccessNodeGen;
import com.adacore.lkql_jit.nodes.expressions.literals.UnitLiteral;
import com.adacore.lkql_jit.nodes.patterns.UniversalPattern;
import com.adacore.lkql_jit.nodes.patterns.node_patterns.NodeKindPattern;

/**
 * This class represents the "super_types" built-in selector.
 *
 * @author Hugo GUERRIER
 */
public final class SuperTypesSelector implements BuiltInSelector {

    // ----- Attributes -----

    /** The only instance of the "super_types" built-in selector. */
    private static SuperTypesSelector instance = null;

    /** The name of the selector. */
    public static final String NAME = "super_types";

    /** The arms representing the "super_types" selector execution. */
    public final SelectorArm[] arms;

    // ----- Constructors -----

    /** Private constructors. */
    private SuperTypesSelector() {
        this.arms = createArms();
    }

    /**
     * Get the only instance of the built-in selector.
     *
     * @return The instance of the selector.
     */
    public static SuperTypesSelector getInstance() {
        if (instance == null) {
            instance = new SuperTypesSelector();
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
                NAME, "Given a TypeDecl node, yields all the super types of the type\n", this.arms);
    }

    // ----- Class methods -----

    /**
     * Create the selector arms and return them.
     *
     * @return The selector arms.
     */
    private static SelectorArm[] createArms() {
        // Create the arms
        SelectorArm[] res = new SelectorArm[2];

        // Create the base type path
        DotAccess propertyAccess =
                DotAccessNodeGen.create(
                        null, new Identifier(null, "p_base_types"), new ReadBuiltInThis());
        FunCall propertyCall =
                FunCallNodeGen.create(
                        null, false, null, new ArgList(null, new Arg[0]), propertyAccess);
        SelectorArm baseTypePath =
                new SelectorArm(
                        null,
                        new NodeKindPattern(null, "BaseTypeDecl"),
                        new SelectorExpr(
                                null, SelectorExpr.Mode.REC, new Unpack(null, propertyCall)));
        res[0] = baseTypePath;

        // Create the universal path
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

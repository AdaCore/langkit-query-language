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

package com.adacore.lkql_jit.runtime.built_ins.selectors;

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
import com.adacore.lkql_jit.runtime.built_ins.BuiltInSelectorValue;
import com.adacore.lkql_jit.runtime.values.SelectorValue;

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
     * @see com.adacore.lkql_jit.runtime.built_ins.selectors.BuiltInSelector#getName()
     */
    @Override
    public String getName() {
        return NAME;
    }

    /**
     * @see com.adacore.lkql_jit.runtime.built_ins.selectors.BuiltInSelector#getValue()
     */
    @Override
    public SelectorValue getValue() {
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

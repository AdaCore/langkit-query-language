/*----------------------------------------------------------------------------
--                             L K Q L   J I T                              --
--                                                                          --
--                     Copyright (C) 2022, AdaCore                          --
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
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
-----------------------------------------------------------------------------*/

package com.adacore.lkql_jit.runtime.built_ins.selectors;

import com.adacore.lkql_jit.nodes.Identifier;
import com.adacore.lkql_jit.nodes.expressions.dot.DotAccess;
import com.adacore.lkql_jit.nodes.expressions.literals.UnitLiteral;
import com.adacore.lkql_jit.nodes.patterns.node_patterns.NodeKindPattern;
import com.adacore.lkql_jit.nodes.declarations.selectors.SelectorArm;
import com.adacore.lkql_jit.nodes.declarations.selectors.SelectorExpr;
import com.adacore.lkql_jit.nodes.expressions.dot.DotAccessNodeGen;
import com.adacore.lkql_jit.nodes.patterns.UniversalPattern;
import com.adacore.lkql_jit.runtime.built_ins.BuiltInSelectorValue;
import com.adacore.lkql_jit.runtime.values.SelectorValue;


/**
 * This class represents the "previous_siblings" built-in selector
 *
 * @author Hugo GUERRIER
 */
public final class PrevSiblingsSelector implements BuiltInSelector {

    // ----- Attributes -----

    /** The only instance of the "previous_siblings" built-in selector */
    private static PrevSiblingsSelector instance = null;

    /** The name of the selector */
    public static final String NAME = "prev_siblings";

    /** The arms representing the "previous_siblings" selector execution */
    public final SelectorArm[] arms;

    // ----- Constructors -----

    /**
     * Private constructor
     */
    private PrevSiblingsSelector() {
        this.arms = createArms();
    }

    /**
     * Get the only instance of the built-in selector
     *
     * @return The instance of the selector
     */
    public static PrevSiblingsSelector getInstance() {
        if(instance == null) {
            instance = new PrevSiblingsSelector();
        }
        return instance;
    }

    // ----- Override methods -----

    /** @see com.adacore.lkql_jit.runtime.built_ins.selectors.BuiltInSelector#getName() */
    @Override
    public String getName() {
        return NAME;
    }

    /** @see com.adacore.lkql_jit.runtime.built_ins.selectors.BuiltInSelector#getValue() */
    @Override
    public SelectorValue getValue() {
        return new BuiltInSelectorValue(
                NAME,
                "Yields the siblings preceding the given node in the tree\n",
                this.arms
        );
    }

    // ----- Class methods -----

    /**
     * Create the selector arms and return them
     *
     * @return The selector arms
     */
    private static SelectorArm[] createArms() {
        // Prepare the result
        SelectorArm[] res = new SelectorArm[2];

        // Create the previous path
        DotAccess propertyAccess = DotAccessNodeGen.create(
                null,
                new Identifier(null, "previous_sibling"),
                new ReadBuiltInThis()
        );
        SelectorArm prevSiblingPath = new SelectorArm(
                null,
                new NodeKindPattern(null, "AdaNode"),
                new SelectorExpr(null, SelectorExpr.Mode.REC, propertyAccess)
        );
        res[0] = prevSiblingPath;


        // Create the universal path
        SelectorArm universalPath = new SelectorArm(
                null,
                new UniversalPattern(null),
                new SelectorExpr(null, SelectorExpr.Mode.DEFAULT, new UnitLiteral(null))
        );
        res[1] = universalPath;

        // Return the result
        return res;
    }

}

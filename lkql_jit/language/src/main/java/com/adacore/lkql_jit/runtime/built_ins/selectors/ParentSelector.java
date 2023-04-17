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
import com.adacore.lkql_jit.nodes.declarations.selectors.SelectorArm;
import com.adacore.lkql_jit.nodes.declarations.selectors.SelectorExpr;
import com.adacore.lkql_jit.nodes.expressions.dot.DotAccess;
import com.adacore.lkql_jit.nodes.expressions.dot.DotAccessNodeGen;
import com.adacore.lkql_jit.nodes.expressions.literals.UnitLiteral;
import com.adacore.lkql_jit.nodes.patterns.UniversalPattern;
import com.adacore.lkql_jit.nodes.patterns.node_patterns.NodeKindPattern;
import com.adacore.lkql_jit.runtime.built_ins.BuiltInSelectorValue;
import com.adacore.lkql_jit.runtime.values.SelectorValue;


/**
 * This class represents the "parent" built-in selector
 *
 * @author Hugo GUERRIER
 */
public final class ParentSelector implements BuiltInSelector {

    // ----- Attributes -----

    /**
     * The only instance of the "parent" built-in selector
     */
    private static ParentSelector instance = null;

    /**
     * The name of the selector
     */
    public static final String NAME = "parent";

    /**
     * The arms representing the "parent" selector execution
     */
    public final SelectorArm[] arms;

    // ----- Constructors -----

    /**
     * Private constructor
     */
    private ParentSelector() {
        this.arms = this.createArms();
    }

    /**
     * Get the only instance of the built-in selector
     *
     * @return The instance of the selector
     */
    public static ParentSelector getInstance() {
        if (instance == null) {
            instance = new ParentSelector();
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
            NAME,
            "Yields the parents (ancestors) of the given node in the tree\n",
            this.arms
        );
    }

    // ----- Class methods -----

    /**
     * Create the selector arms and return them
     *
     * @return The selector arms
     */
    private SelectorArm[] createArms() {
        // Prepare the result
        SelectorArm[] res = new SelectorArm[2];

        // Create the children path
        DotAccess toUnpack = DotAccessNodeGen.create(
            null,
            new Identifier(null, "parent"),
            new ReadBuiltInThis()
        );
        SelectorArm parentPath = new SelectorArm(
            null,
            new NodeKindPattern(null, "AdaNode"),
            new SelectorExpr(
                null,
                SelectorExpr.Mode.REC,
                toUnpack
            )
        );
        res[0] = parentPath;

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
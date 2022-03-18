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

package com.adacore.lkql_jit.runtime.built_ins.functions;

import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.nodes.expressions.literals.BooleanLiteral;
import com.adacore.lkql_jit.utils.LKQLTypesHelper;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.UnexpectedResultException;
import com.adacore.lkql_jit.LKQLTypeSystemGen;
import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.adacore.lkql_jit.runtime.built_ins.BuiltInExpr;
import com.adacore.lkql_jit.runtime.built_ins.BuiltInFunctionValue;
import com.adacore.lkql_jit.runtime.values.Pattern;


/**
 * This class represents the "pattern" built-in function in the LKQL language
 *
 * @author Hugo GUERRIER
 */
public final class PatternFunction implements BuiltInFunction {

    // ----- Attributes -----

    /** The only instance of the "pattern" built-in */
    private static PatternFunction instance = null;

    /** The name of the built-in */
    public static final String NAME = "pattern";

    /** The expression that represents the "pattern" function execution */
    private final PatternExpr patternExpr;

    // ----- Constructors -----

    /**
     * Private constructor
     */
    private PatternFunction() {
        this.patternExpr = new PatternExpr();
    }

    /**
     * Get the only instance of the built-in function
     *
     * @return The only instance
     */
    public static PatternFunction getInstance() {
        if(instance == null) {
            instance = new PatternFunction();
        }
        return instance;
    }

    // ----- Override methods -----

    /** @see com.adacore.lkql_jit.runtime.built_ins.functions.BuiltInFunction#getName() */
    @Override
    public String getName() {
        return NAME;
    }

    /** @see com.adacore.lkql_jit.runtime.built_ins.functions.BuiltInFunction#getValue() */
    @Override
    public BuiltInFunctionValue getValue() {
        return new BuiltInFunctionValue(
                NAME,
                "Given a regex pattern string, create a pattern object",
                new String[]{"regex", "case_sensitive"},
                new Expr[]{null, new BooleanLiteral(null, true)},
                this.patternExpr
        );
    }

    // ----- Inner classes -----

    /**
     * Expression of the "pattern" function
     */
    public final static class PatternExpr extends BuiltInExpr {
        @Override
        public Object executeGeneric(VirtualFrame frame) {
            // Get the string parameter
            String regexString;
            try {
                regexString = LKQLTypeSystemGen.expectString(frame.getArguments()[0]);
            } catch (UnexpectedResultException e) {
                throw LKQLRuntimeException.wrongType(
                        LKQLTypesHelper.LKQL_STRING,
                        LKQLTypesHelper.fromJava(e.getResult()),
                        this.callNode.getArgList().getArgs()[0]
                );
            }

            // Get the case sensitiveness parameter
            boolean caseSensitive;
            try {
                caseSensitive = LKQLTypeSystemGen.expectBoolean(frame.getArguments()[1]);
            } catch (UnexpectedResultException e) {
                throw LKQLRuntimeException.wrongType(
                        LKQLTypesHelper.LKQL_BOOLEAN,
                        LKQLTypesHelper.fromJava(e.getResult()),
                        this.callNode.getArgList().getArgs()[1]
                );
            }

            // Create the pattern and return it
            return new Pattern(this.callNode, regexString, caseSensitive);
        }
    }

}

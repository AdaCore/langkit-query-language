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

import com.adacore.lkql_jit.LKQLLanguage;
import com.adacore.lkql_jit.LKQLTypeSystemGen;
import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.adacore.lkql_jit.nodes.expressions.literals.BooleanLiteral;
import com.adacore.lkql_jit.runtime.built_ins.BuiltInExpr;
import com.adacore.lkql_jit.runtime.built_ins.BuiltInFunctionValue;
import com.adacore.lkql_jit.runtime.values.UnitValue;
import com.adacore.lkql_jit.utils.LKQLTypesHelper;
import com.adacore.lkql_jit.utils.util_functions.ObjectUtils;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.UnexpectedResultException;


/**
 * This class represents the "print" built-in function in the LKQL language.
 *
 * @author Hugo GUERRIER
 */
public final class PrintFunction implements BuiltInFunction {

    // ----- Attributes -----

    /**
     * The only instance for the "print" built-in.
     */
    private static PrintFunction instance = null;

    /**
     * The name of the function.
     */
    public static final String NAME = "print";

    /**
     * The expression that represents the "print" function execution.
     */
    private final PrintExpr printExpr;

    // ----- Constructors -----

    /**
     * Private constructor.
     */
    private PrintFunction() {
        this.printExpr = new PrintExpr();
    }

    /**
     * Get the instance of the built-in function.
     *
     * @return The only instance.
     */
    public static PrintFunction getInstance() {
        if (instance == null) {
            instance = new PrintFunction();
        }
        return instance;
    }

    // ----- Override methods -----

    /**
     * @see com.adacore.lkql_jit.runtime.built_ins.functions.BuiltInFunction#getName()
     */
    @Override
    public String getName() {
        return NAME;
    }

    /**
     * @see com.adacore.lkql_jit.runtime.built_ins.functions.BuiltInFunction#getValue()
     */
    @Override
    public BuiltInFunctionValue getValue() {
        return new BuiltInFunctionValue(
            NAME,
            "Built-in print function. Prints whatever is passed as an argument",
            new String[]{"val", "new_line"},
            new Expr[]{null, new BooleanLiteral(null, true)},
            this.printExpr
        );
    }

    // ----- Inner classes -----

    /**
     * Expression of the "print" function.
     */
    public final static class PrintExpr extends BuiltInExpr {
        @Override
        public Object executeGeneric(VirtualFrame frame) {
            // Get the arguments
            Object toPrint = frame.getArguments()[0];

            boolean newLine;
            try {
                newLine = LKQLTypeSystemGen.expectBoolean(frame.getArguments()[1]);
            } catch (UnexpectedResultException e) {
                throw LKQLRuntimeException.wrongType(
                    LKQLTypesHelper.LKQL_BOOLEAN,
                    LKQLTypesHelper.fromJava(e.getResult()),
                    this.callNode.getArgList().getArgs()[1]
                );
            }

            // Print the value
            if (newLine) {
                LKQLLanguage.getContext(this.callNode).println(ObjectUtils.toString(toPrint));
            } else {
                LKQLLanguage.getContext(this.callNode).print(ObjectUtils.toString(toPrint));
            }

            // Return the unit value
            return UnitValue.getInstance();
        }
    }

}

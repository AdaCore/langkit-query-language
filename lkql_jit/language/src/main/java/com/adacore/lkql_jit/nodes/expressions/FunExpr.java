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

package com.adacore.lkql_jit.nodes.expressions;

import com.adacore.lkql_jit.utils.source_location.SourceLocation;
import com.adacore.lkql_jit.utils.util_classes.Closure;
import com.oracle.truffle.api.frame.FrameDescriptor;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.adacore.lkql_jit.nodes.declarations.ParameterDecl;
import com.adacore.lkql_jit.runtime.values.FunctionValue;

import java.util.Arrays;


/**
 * This node represents a function expression in the LKQL language
 *
 * @author Hugo GUERRIER
 */
public final class FunExpr extends Expr {

    // ----- Attributes -----

    /** The frame descriptor for the callable function node creation */
    private final FrameDescriptor frameDescriptor;

    /** The limit for the closure */
    private final int closureLimit;

    /** The slots to put the arguments in */
    private final int[] slots;

    /** The name of the arguments */
    private final String[] names;

    /** The default values of the arguments */
    private final Expr[] values;

    // ----- Children -----

    /** The body of the function */
    @Child
    @SuppressWarnings("FieldMayBeFinal")
    private Expr body;

    // ----- Constructors -----

    /**
     * Create a new function expression node
     *
     * @param location The location of the node in the source
     * @param frameDescriptor The frame descriptor for the function root node
     * @param closureLimit The limit of the closure
     * @param parameters The parameters of the function
     * @param body The body of the function
     */
    public FunExpr(
            SourceLocation location,
            FrameDescriptor frameDescriptor,
            int closureLimit,
            ParameterDecl[] parameters,
            Expr body
    ) {
        super(location);
        this.frameDescriptor = frameDescriptor;
        this.closureLimit = closureLimit;
        this.slots = new int[parameters.length];
        this.names = new String[parameters.length];
        this.values = new Expr[parameters.length];
        this.body = body;

        this.initParams(parameters);
    }

    /**
     * Initialize the parameter fields
     */
    private void initParams(ParameterDecl[] parameters) {
        for (int i = 0; i < parameters.length; i++) {
            this.slots[i] = parameters[i].getSlot();
            this.names[i] = parameters[i].getName();
            this.values[i] = parameters[i].getDefaultValue();
        }
    }

    // ----- Execution methods -----

    /** @see com.adacore.lkql_jit.nodes.LKQLNode#executeGeneric(com.oracle.truffle.api.frame.VirtualFrame) */
    @Override
    public Object executeGeneric(VirtualFrame frame) {
        return this.executeFunction(frame);
    }

    /** @see com.adacore.lkql_jit.nodes.expressions.Expr#executeFunction(com.oracle.truffle.api.frame.VirtualFrame) */
    @Override
    public FunctionValue executeFunction(VirtualFrame frame) {
        return new FunctionValue(
                this.frameDescriptor,
                new Closure(frame.materialize(), this.closureLimit),
                false,
                "lambda",
                "",
                this.slots,
                this.names,
                this.values,
                this.body
        );
    }

    // ----- Override methods -----

    /** @see com.adacore.lkql_jit.nodes.LKQLNode#toString(int) */
    @Override
    public String toString(int indentLevel) {
        return this.nodeRepresentation(
                indentLevel,
                new String[]{"names", "slots"},
                new Object[]{Arrays.toString(this.names), Arrays.toString(this.slots)}
        );
    }

}

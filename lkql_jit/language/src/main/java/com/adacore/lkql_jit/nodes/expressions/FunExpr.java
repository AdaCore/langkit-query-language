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

import com.adacore.lkql_jit.LKQLLanguage;
import com.adacore.lkql_jit.nodes.declarations.ParameterDeclaration;
import com.adacore.lkql_jit.nodes.root_nodes.FunctionRootNode;
import com.adacore.lkql_jit.runtime.Closure;
import com.adacore.lkql_jit.runtime.values.FunctionValue;
import com.adacore.lkql_jit.utils.Constants;
import com.adacore.lkql_jit.utils.source_location.SourceLocation;
import com.adacore.lkql_jit.utils.ClosureDescriptor;
import com.oracle.truffle.api.frame.FrameDescriptor;
import com.oracle.truffle.api.frame.VirtualFrame;

import java.util.Arrays;


/**
 * This node represents a function expression in the LKQL language.
 *
 * @author Hugo GUERRIER
 */
public final class FunExpr extends Expr {

    // ----- Attributes -----

    /**
     * Closure descriptor of the function.
     */
    private final ClosureDescriptor closureDescriptor;

    /**
     * The root node representing the function execution.
     */
    private final FunctionRootNode functionRootNode;

    /**
     * The names of the parameters.
     */
    private final String[] parameterNames;

    /**
     * The default values of the parameters.
     */
    private final Expr[] parameterValues;

    // ----- Children -----

    /**
     * The body of the function.
     */
    @Child
    @SuppressWarnings("FieldMayBeFinal")
    private Expr body;

    // ----- Constructors -----

    /**
     * Create a new function expression node.
     *
     * @param location          The location of the node in the source.
     * @param frameDescriptor   The frame descriptor for the function root node.
     * @param closureDescriptor The closure descriptor for the function.
     * @param parameters        The parameters of the function.
     * @param body              The body of the function.
     */
    public FunExpr(
        final SourceLocation location,
        final FrameDescriptor frameDescriptor,
        final ClosureDescriptor closureDescriptor,
        final ParameterDeclaration[] parameters,
        final Expr body
    ) {
        super(location);
        this.closureDescriptor = closureDescriptor;
        this.functionRootNode = new FunctionRootNode(
            LKQLLanguage.getLanguage(this),
            frameDescriptor,
            false,
            body
        );
        this.parameterNames = new String[parameters.length];
        this.parameterValues = new Expr[parameters.length];
        this.body = body;

        this.initParams(parameters);
    }

    /**
     * Initialize the parameter fields
     */
    private void initParams(ParameterDeclaration[] parameters) {
        for (int i = 0; i < parameters.length; i++) {
            this.parameterNames[i] = parameters[i].getName();
            this.parameterValues[i] = parameters[i].getDefaultValue();
        }
    }

    // ----- Execution methods -----

    /**
     * @see com.adacore.lkql_jit.nodes.LKQLNode#executeGeneric(com.oracle.truffle.api.frame.VirtualFrame)
     */
    @Override
    public Object executeGeneric(VirtualFrame frame) {
        return this.executeFunction(frame);
    }

    /**
     * @see com.adacore.lkql_jit.nodes.expressions.Expr#executeFunction(com.oracle.truffle.api.frame.VirtualFrame)
     */
    @Override
    public FunctionValue executeFunction(VirtualFrame frame) {
        return new FunctionValue(
            this.functionRootNode,
            Closure.create(frame.materialize(), this.closureDescriptor),
            Constants.FUNCTION_DEFAULT_NAME,
            Constants.FUNCTION_DEFAULT_DOC,
            this.parameterNames,
            this.parameterValues
        );
    }

    // ----- Override methods -----

    /**
     * @see com.adacore.lkql_jit.nodes.LKQLNode#toString(int)
     */
    @Override
    public String toString(int indentLevel) {
        return this.nodeRepresentation(
            indentLevel,
            new String[]{"names"},
            new Object[]{Arrays.toString(this.parameterNames)}
        );
    }

}

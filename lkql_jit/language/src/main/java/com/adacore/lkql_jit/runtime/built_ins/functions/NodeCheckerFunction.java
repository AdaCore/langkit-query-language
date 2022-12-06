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
import com.adacore.lkql_jit.utils.LKQLTypesHelper;
import com.adacore.lkql_jit.utils.util_functions.CheckerUtils;
import com.adacore.lkql_jit.utils.util_functions.ObjectUtils;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.UnexpectedResultException;
import com.adacore.libadalang.Libadalang;
import org.graalvm.collections.Pair;
import com.adacore.lkql_jit.LKQLContext;
import com.adacore.lkql_jit.LKQLTypeSystemGen;
import com.adacore.lkql_jit.exception.LangkitException;
import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.nodes.dispatchers.FunctionDispatcher;
import com.adacore.lkql_jit.nodes.dispatchers.FunctionDispatcherNodeGen;
import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.adacore.lkql_jit.runtime.built_ins.BuiltInExpr;
import com.adacore.lkql_jit.runtime.built_ins.BuiltInFunctionValue;
import com.adacore.lkql_jit.runtime.values.FunctionValue;
import com.adacore.lkql_jit.runtime.values.ObjectValue;
import com.adacore.lkql_jit.runtime.values.UnitValue;

import java.util.ArrayList;
import java.util.List;


/**
 * This class represents the "node_checker" built-in function in the LKQL language
 *
 * @author Hugo GUERRIER
 */
public final class NodeCheckerFunction implements BuiltInFunction {

    // ----- Attributes -----

    /** The only instance of the "node_checker" built-in */
    private static NodeCheckerFunction instance = null;

    /** The name of the built-in */
    public static final String NAME = "node_checker";

    /** The expression that represents the "node_checker" function execution */
    private final NodeCheckerExpr nodeCheckerExpr;

    // ----- Constructors -----

    /**
     * Private constructor
     */
    private NodeCheckerFunction() {
        this.nodeCheckerExpr = new NodeCheckerExpr();
    }

    /**
     * Get the only instance of the built-in function
     *
     * @return The only instance
     */
    public static NodeCheckerFunction getInstance() {
        if(instance == null) {
            instance = new NodeCheckerFunction();
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
                "Given a root, execute all node checker while traverse the tree",
                new String[]{"root"},
                new Expr[]{null},
                this.nodeCheckerExpr
        );
    }

    // ----- Inner classes -----

    /**
     * This class is the expression of the "node_checker" built-in
     * This expression contains the traversing logic to checker the nodes
     */
    private static final class NodeCheckerExpr extends BuiltInExpr {

        /** The dispatcher for the rule functions */
        @Child
        @SuppressWarnings("FieldMayBeFinal")
        private FunctionDispatcher dispatcher = FunctionDispatcherNodeGen.create();

        /** @see com.adacore.lkql_jit.runtime.built_ins.BuiltInExpr#executeGeneric(com.oracle.truffle.api.frame.VirtualFrame) */
        @Override
        public Object executeGeneric(VirtualFrame frame) {
            // Get the arguments
            LKQLContext context = LKQLLanguage.getContext(this);
            Libadalang.AdaNode root;
            ObjectValue[] checkers = context.getGlobalValues().getNodeChecker();

            try {
                root = LKQLTypeSystemGen.expectAdaNode(frame.getArguments()[0]);
            } catch (UnexpectedResultException e) {
                throw LKQLRuntimeException.wrongType(
                        LKQLTypesHelper.ADA_NODE,
                        LKQLTypesHelper.fromJava(e.getResult()),
                        this.callNode.getArgList().getArgs()[0]
                );
            }

            // Traverse the tree
            // Create the list of node to explore with the generic instantiation info
            List<Pair<Libadalang.AdaNode, Boolean>> visitList = new ArrayList<>();
            visitList.add(Pair.create(root, false));

            // Iterate over all nodes of the tree
            while(!visitList.isEmpty()) {
                // Get the current values
                Pair<Libadalang.AdaNode, Boolean> currentStep = visitList.remove(0);
                Libadalang.AdaNode currentNode = currentStep.getLeft();
                boolean inGenericInstantiation = currentStep.getRight();

                // Verify if the node is a generic instantiation
                if(currentNode instanceof Libadalang.GenericInstantiation genInst) {
                    Libadalang.BasicDecl genDecl = genInst.pDesignatedGenericDecl();
                    visitList.add(Pair.create(genDecl, true));
                    Libadalang.BodyNode genBody = genInst.pBodyPartForDecl(false);
                    if(genBody != null && !genBody.isNull()) visitList.add(Pair.create(genBody, true));
                }

                // Iterate over rules and apply them
                for(ObjectValue rule : checkers) {
                    if(context.getRule() != null && !ObjectUtils.equals(rule.get("name"), context.getRule())) continue;
                    if(!inGenericInstantiation || (boolean) rule.get("follow_generic_instantiations")) {
                        try {
                            this.applyNodeRule(frame, rule, currentNode, context);
                        } catch (LangkitException e) {
                            this.reportException(rule, e);
                        }
                    }
                }

                // Add the children to the visit list
                for(int i = currentNode.getChildrenCount() - 1 ; i >= 0 ; i--) {
                    Libadalang.AdaNode child = currentNode.getChild(i);
                    if(child != null && !child.isNull()) {
                        visitList.add(0, Pair.create(child, inGenericInstantiation));
                    }
                }
            }

            // Return the unit instance
            return UnitValue.getInstance();
        }

        /**
         * Apply the rule on the given node
         *
         * @param frame The frame to execute the default arg value
         * @param rule The rule to apply
         * @param node The node to apply the rule on
         * @param context The LKQL context
         */
        private void applyNodeRule(VirtualFrame frame, ObjectValue rule, Libadalang.AdaNode node, LKQLContext context) {
            // Get the function for the checker
            FunctionValue functionValue = (FunctionValue) rule.get("function");

            // Prepare the arguments
            Object[] arguments = new Object[functionValue.getParamNames().length];
            arguments[0] = node;
            for(int i = 1 ; i < functionValue.getDefaultValues().length ; i++) {
                arguments[i] = functionValue.getDefaultValues()[i].executeGeneric(frame);
            }

            // Put the namespace
            if(functionValue.getNamespace() != null) {
                context.getGlobalValues().pushNamespace(functionValue.getNamespace());
            }

            // Call the rule
            boolean ruleResult;
            try {
                ruleResult = LKQLTypeSystemGen.expectBoolean(this.dispatcher.executeDispatch(functionValue, arguments));
            } catch (UnexpectedResultException e) {
                throw LKQLRuntimeException.wrongType(
                        LKQLTypesHelper.LKQL_BOOLEAN,
                        LKQLTypesHelper.fromJava(e.getResult()),
                        functionValue.getBody()
                );
            } finally {
                // Remove the namespace
                if(functionValue.getNamespace() != null) {
                    context.getGlobalValues().finalizeScope();
                }
            }

            if(ruleResult) {
                this.reportViolation(rule, node);
            }
        }

        /**
         * Report a rule violation with the node that violate it
         *
         * @param rule The violated rule
         * @param node The node that violated the rule
         */
        @CompilerDirectives.TruffleBoundary
        private void reportViolation(ObjectValue rule, Libadalang.AdaNode node) {
            if(node instanceof Libadalang.BasicDecl basicDecl) {
                CheckerUtils.printRuleViolation((String) rule.get("message"), basicDecl.pDefiningName());
            } else {
                CheckerUtils.printRuleViolation((String) rule.get("message"), node);
            }
        }

        /**
         * Report the langkit exception raised by a rule
         *
         * @param rule The rule which caused the exception
         * @param e The exception to report
         */
        @CompilerDirectives.TruffleBoundary
        private void reportException(ObjectValue rule, LangkitException e) {
            System.out.println("TODO : Report exception : " + e.getMsg());
        }

    }

}

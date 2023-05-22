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

import com.adacore.libadalang.Libadalang;
import com.adacore.lkql_jit.LKQLContext;
import com.adacore.lkql_jit.LKQLLanguage;
import com.adacore.lkql_jit.LKQLTypeSystemGen;
import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.exception.LangkitException;
import com.adacore.lkql_jit.nodes.dispatchers.FunctionDispatcher;
import com.adacore.lkql_jit.nodes.dispatchers.FunctionDispatcherNodeGen;
import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.adacore.lkql_jit.runtime.built_ins.BuiltInExpr;
import com.adacore.lkql_jit.runtime.built_ins.BuiltInFunctionValue;
import com.adacore.lkql_jit.runtime.values.FunctionValue;
import com.adacore.lkql_jit.runtime.values.ObjectValue;
import com.adacore.lkql_jit.runtime.values.UnitValue;
import com.adacore.lkql_jit.utils.LKQLTypesHelper;
import com.adacore.lkql_jit.utils.util_functions.CheckerUtils;
import com.adacore.lkql_jit.utils.util_functions.StringUtils;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.UnexpectedResultException;

import java.util.LinkedList;


/**
 * This class represents the "node_checker" built-in function in the LKQL language
 *
 * @author Hugo GUERRIER
 */
public final class NodeCheckerFunction implements BuiltInFunction {

    // ----- Attributes -----

    /**
     * The only instance of the "node_checker" built-in
     */
    private static NodeCheckerFunction instance = null;

    /**
     * The name of the built-in
     */
    public static final String NAME = "node_checker";

    /**
     * The expression that represents the "node_checker" function execution
     */
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
        if (instance == null) {
            instance = new NodeCheckerFunction();
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

        /**
         * The dispatcher for the rule functions
         */
        @Child
        @SuppressWarnings("FieldMayBeFinal")
        private FunctionDispatcher dispatcher = FunctionDispatcherNodeGen.create();

        /**
         * @see com.adacore.lkql_jit.runtime.built_ins.BuiltInExpr#executeGeneric(com.oracle.truffle.api.frame.VirtualFrame)
         */
        @Override
        public Object executeGeneric(VirtualFrame frame) {
            // Get the arguments
            final LKQLContext context = LKQLLanguage.getContext(this);
            final Libadalang.AdaNode root;
            final ObjectValue[] checkers = context.getNodeCheckersFiltered();
            final boolean mustFollowInstantiations = context.mustFollowInstantiations();

            try {
                root = LKQLTypeSystemGen.expectAdaNode(frame.getArguments()[0]);
            } catch (UnexpectedResultException e) {
                throw LKQLRuntimeException.wrongType(
                    LKQLTypesHelper.ADA_NODE,
                    LKQLTypesHelper.fromJava(e.getResult()),
                    this.callNode.getArgList().getArgs()[0]
                );
            }

            // Initialize the cache that will contain decoded source lines of all needed units
            CheckerUtils.SourceLinesCache linesCache = new CheckerUtils.SourceLinesCache();

            // Traverse the tree
            // Create the list of node to explore with the generic instantiation info
            final LinkedList<VisitStep> visitList = new LinkedList<>();
            visitList.add(new VisitStep(root, false));

            // Iterate over all nodes of the tree
            while (!visitList.isEmpty()) {
                // Get the current values
                final VisitStep currentStep = visitList.remove(0);
                final Libadalang.AdaNode currentNode = currentStep.node();
                final boolean inGenericInstantiation = currentStep.inGenericInstantiation();

                try {
                    if (mustFollowInstantiations && currentNode instanceof Libadalang.GenericInstantiation genInst) {
                        // If the node is a generic instantiation, traverse the instantiated generic
                        final Libadalang.BasicDecl genDecl = genInst.pDesignatedGenericDecl();
                        final Libadalang.BodyNode genBody = genDecl.pBodyPartForDecl(false);

                        if (!genBody.isNone()) {
                            visitList.addFirst(new VisitStep(genBody, true));
                        }
                        visitList.addFirst(new VisitStep(genDecl, true));
                    } else if (inGenericInstantiation && currentNode instanceof Libadalang.BodyStub stub) {
                        // If this node is a body stub and we are currently traversing a generic instantiation,
                        // we should also traverse the stub's completion.
                        final Libadalang.BasicDecl stubBody = stub.pNextPartForDecl(false);
                        visitList.addFirst(new VisitStep(stubBody, true));
                    }
                } catch (Libadalang.LangkitException e) {
                    context.println(StringUtils.concat(
                        "Error during generic instantiation walking: ",
                        e.getMessage()
                    ));
                    continue;
                }

                // Iterate over rules and apply them
                for (ObjectValue rule : checkers) {
                    if (!inGenericInstantiation || (boolean) rule.get("follow_generic_instantiations")) {
                        try {
                            this.applyNodeRule(frame, rule, currentNode, context, linesCache);
                        } catch (LangkitException e) {
                            reportException(context, rule, e);
                        } catch (LKQLRuntimeException e) {
                            reportException(context, e);
                        }
                    }
                }

                // Add the children to the visit list
                for (int i = currentNode.getChildrenCount() - 1; i >= 0; i--) {
                    final Libadalang.AdaNode child = currentNode.getChild(i);
                    if (!child.isNone()) {
                        visitList.addFirst(
                            new VisitStep(child, inGenericInstantiation)
                        );
                    }
                }
            }

            // Return the unit instance
            return UnitValue.getInstance();
        }

        /**
         * Apply the rule on the given node
         *
         * @param frame      The frame to execute the default arg value
         * @param rule       The rule to apply
         * @param node       The node to apply the rule on
         * @param context    The LKQL context
         * @param linesCache The cache of all units' source text lines
         */
        private void applyNodeRule(
            VirtualFrame frame,
            ObjectValue rule,
            Libadalang.AdaNode node,
            LKQLContext context,
            CheckerUtils.SourceLinesCache linesCache
        ) {
            // Get the function for the checker
            FunctionValue functionValue = (FunctionValue) rule.get("function");
            String lowerRuleName = StringUtils.toLowerCase((String) rule.get("name"));

            // Prepare the arguments
            Object[] arguments = new Object[functionValue.getParamNames().length];
            arguments[0] = node;
            for (int i = 1; i < functionValue.getDefaultValues().length; i++) {
                String paramName = functionValue.getParamNames()[i];
                Object userDefinedArg = context.getRuleArg(
                    lowerRuleName,
                    StringUtils.toLowerCase(paramName)
                );
                arguments[i] = userDefinedArg == null ?
                    functionValue.getDefaultValues()[i].executeGeneric(frame) :
                    userDefinedArg;
            }

            // Put the namespace
            if (functionValue.getNamespace() != null) {
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
                if (functionValue.getNamespace() != null) {
                    context.getGlobalValues().popNamespace();
                }
            }

            if (ruleResult) {
                reportViolation(context, rule, node, linesCache);
            }
        }

        /**
         * Report a rule violation with the node that violate it
         *
         * @param context    The context to output the message
         * @param rule       The violated rule
         * @param node       The node that violated the rule
         * @param linesCache The cache of all units' source text lines
         */
        @CompilerDirectives.TruffleBoundary
        private static void reportViolation(
            LKQLContext context,
            ObjectValue rule,
            Libadalang.AdaNode node,
            CheckerUtils.SourceLinesCache linesCache
        ) {
            if (node instanceof Libadalang.BasicDecl basicDecl) {
                Libadalang.AdaNode definingName = basicDecl.pDefiningName();
                node = definingName.isNone() ? node : definingName;
            }
            context.getDiagnosticEmitter().emit(
                (String) rule.get("name"),
                (String) rule.get("message"),
                node.getSourceLocationRange(),
                node.getUnit(),
                node.pGenericInstantiations(),
                linesCache,
                context
            );
        }

        /**
         * Report the langkit exception raised by a rule
         *
         * @param rule The rule which caused the exception
         * @param e    The exception to report
         */
        @CompilerDirectives.TruffleBoundary
        private static void reportException(LKQLContext context, ObjectValue rule, LangkitException e) {
            context.println("TODO : Report exception : " + e.getMsg());
        }

        /**
         * Report the LQKL exception
         *
         * @param e The LKQL exception
         */
        @CompilerDirectives.TruffleBoundary
        private static void reportException(LKQLContext context, LKQLRuntimeException e) {
            context.println("Exception in the LKQL code :");
            context.println(e.getMessage());
        }

        // ----- Inner classes -----

        /**
         * This record contains the information for a visiting step
         *
         * @param node                   The node to visit
         * @param inGenericInstantiation If the visit is currently in a generic instantiation
         */
        private record VisitStep(
            Libadalang.AdaNode node,
            boolean inGenericInstantiation
        ) {
        }

    }

}

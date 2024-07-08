//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.built_ins.methods;

import static com.adacore.lkql_jit.built_ins.BuiltInMethodFactory.createMethod;

import com.adacore.libadalang.Libadalang;
import com.adacore.lkql_jit.LKQLTypeSystemGen;
import com.adacore.lkql_jit.built_ins.AbstractBuiltInFunctionBody;
import com.adacore.lkql_jit.built_ins.BuiltInMethodFactory;
import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.adacore.lkql_jit.nodes.utils.RewritingNodeConverter;
import com.adacore.lkql_jit.nodes.utils.RewritingNodeConverterNodeGen;
import com.adacore.lkql_jit.utils.LKQLTypesHelper;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.UnexpectedResultException;
import java.util.Map;
import java.util.function.BiConsumer;

/** This class contains all methods for the rewriting context type. */
public final class RewritingContextMethods {

    public static final Map<String, BuiltInMethodFactory> methods =
            Map.ofEntries(
                    createMethod(
                            "replace",
                            "Replace old node by the new one",
                            new String[] {"old", "new"},
                            new Expr[] {null, null},
                            new ReplaceExpr()),
                    createMethod(
                            "set_child",
                            "Set the node child, following the given member reference, to the new "
                                    + "value",
                            new String[] {"node", "member_ref", "new_value"},
                            new Expr[] {null, null, null},
                            new SetChildExpr()),
                    createMethod(
                            "insert_before",
                            "Given a node, insert the new one before it in its parent (this"
                                    + " function expects this parent to be a list node, raises a"
                                    + " runtime error otherwise)",
                            new String[] {"node", "new_node"},
                            new Expr[] {null, null},
                            new InsertExpr(Libadalang.RewritingNode::insertBefore)),
                    createMethod(
                            "insert_after",
                            "Given a node, insert the new one after it in its parent (this function"
                                + " expects this parent to be a list node, raises a runtime error"
                                + " otherwise)",
                            new String[] {"node", "new_node"},
                            new Expr[] {null, null},
                            new InsertExpr(Libadalang.RewritingNode::insertAfter)),
                    createMethod(
                            "add_first",
                            "Insert the given new node at the beginning of the given list node "
                                    + "(raises a runtime error if it is not a list node)",
                            new String[] {"list_node", "new_node"},
                            new Expr[] {null, null},
                            new InsertExpr(Libadalang.RewritingNode::insertFirst)),
                    createMethod(
                            "add_last",
                            "Insert the given new node at the end of the given list node"
                                    + "(raises a runtime error if it is not a list node)",
                            new String[] {"list_node", "new_node"},
                            new Expr[] {null, null},
                            new InsertExpr(Libadalang.RewritingNode::insertLast)),
                    createMethod(
                            "remove",
                            "Delete the given node from its parent (this function expects this "
                                    + "parent to be a list node, raises a runtime error otherwise)",
                            new String[] {"to_remove"},
                            new Expr[] {null},
                            new RemoveExpr()));

    /** Body for the "replace" method. */
    public static final class ReplaceExpr extends AbstractBuiltInFunctionBody {
        @Child RewritingNodeConverter argToRewritingNode = RewritingNodeConverterNodeGen.create();

        @Override
        public Object executeGeneric(VirtualFrame frame) {
            // Get the associated rewriting context
            Libadalang.RewritingContext ctx =
                    LKQLTypeSystemGen.asRewritingContext(frame.getArguments()[0]);

            // Get the method arguments
            final var toReplace =
                    argToRewritingNode.execute(
                            frame.getArguments()[1],
                            false,
                            this.callNode.getArgList().getArgs()[0]);
            final var newNode =
                    argToRewritingNode.execute(
                            frame.getArguments()[2], true, this.callNode.getArgList().getArgs()[1]);

            // Replace the given node and return the rewriting context
            try {
                toReplace.replace(newNode);
            } catch (Libadalang.LangkitException e) {
                throw LKQLRuntimeException.fromJavaException(e, this.callNode);
            }
            return ctx;
        }
    }

    /** Body for the "set_child" method. */
    public static final class SetChildExpr extends AbstractBuiltInFunctionBody {
        @Child RewritingNodeConverter argToRewritingNode = RewritingNodeConverterNodeGen.create();

        @Override
        public Object executeGeneric(VirtualFrame frame) {
            // Get the method arguments
            final var node =
                    argToRewritingNode.execute(
                            frame.getArguments()[1],
                            false,
                            this.callNode.getArgList().getArgs()[0]);
            final Libadalang.MemberReference memberRef;
            try {
                memberRef = LKQLTypeSystemGen.expectMemberReference(frame.getArguments()[2]);
            } catch (UnexpectedResultException e) {
                throw LKQLRuntimeException.wrongType(
                        LKQLTypesHelper.MEMBER_REFERENCE,
                        LKQLTypesHelper.fromJava(e.getResult()),
                        this.callNode.getArgList().getArgs()[1]);
            }
            final var newNode =
                    argToRewritingNode.execute(
                            frame.getArguments()[3], true, this.callNode.getArgList().getArgs()[2]);

            // Call the child replacement
            node.setChild(memberRef, newNode);

            return LKQLTypeSystemGen.asRewritingContext(frame.getArguments()[0]);
        }
    }

    /** Body of insert kind methods. */
    public static final class InsertExpr extends AbstractBuiltInFunctionBody {
        @Child RewritingNodeConverter argToRewritingNode = RewritingNodeConverterNodeGen.create();

        private final BiConsumer<Libadalang.RewritingNode, Libadalang.RewritingNode> insertOp;

        public InsertExpr(BiConsumer<Libadalang.RewritingNode, Libadalang.RewritingNode> insertOp) {
            this.insertOp = insertOp;
        }

        @Override
        public Object executeGeneric(VirtualFrame frame) {
            // Get the associated rewriting context
            Libadalang.RewritingContext ctx =
                    LKQLTypeSystemGen.asRewritingContext(frame.getArguments()[0]);

            // Get the method arguments
            final var listNode =
                    argToRewritingNode.execute(
                            frame.getArguments()[1],
                            false,
                            this.callNode.getArgList().getArgs()[0]);
            final var newNode =
                    argToRewritingNode.execute(
                            frame.getArguments()[2], true, this.callNode.getArgList().getArgs()[1]);

            // Apply tbe insertion operation and return the context
            try {
                this.insertOp.accept(listNode, newNode);
            } catch (Libadalang.LangkitException e) {
                throw LKQLRuntimeException.fromJavaException(e, this.callNode);
            }
            return ctx;
        }
    }

    /** Body of the "remove" method. */
    public static final class RemoveExpr extends AbstractBuiltInFunctionBody {
        @Child RewritingNodeConverter argToRewritingNode = RewritingNodeConverterNodeGen.create();

        @Override
        public Object executeGeneric(VirtualFrame frame) {
            // Get the associated rewriting context
            Libadalang.RewritingContext ctx =
                    LKQLTypeSystemGen.asRewritingContext(frame.getArguments()[0]);

            // Get the method arguments
            final var toRemove =
                    argToRewritingNode.execute(
                            frame.getArguments()[1],
                            false,
                            this.callNode.getArgList().getArgs()[0]);

            // Call the removing method and return the context
            try {
                toRemove.removeFromParent();
            } catch (Libadalang.LangkitException e) {
                throw LKQLRuntimeException.fromJavaException(e, this.callNode);
            }
            return ctx;
        }
    }
}

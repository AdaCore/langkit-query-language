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

package com.adacore.lkql_jit.built_ins.methods;

import static com.adacore.lkql_jit.built_ins.BuiltInFunctionValue.create;

import com.adacore.libadalang.Libadalang;
import com.adacore.lkql_jit.LKQLTypeSystemGen;
import com.adacore.lkql_jit.built_ins.BuiltInFunctionValue;
import com.adacore.lkql_jit.built_ins.BuiltinFunctionBody;
import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.adacore.lkql_jit.nodes.utils.RewritingNodeConverter;
import com.adacore.lkql_jit.nodes.utils.RewritingNodeConverterNodeGen;
import com.adacore.lkql_jit.utils.LKQLTypesHelper;
import com.oracle.truffle.api.frame.VirtualFrame;
import java.util.Map;
import java.util.function.BiConsumer;

/** This class contains all methods for the rewriting context type. */
public final class RewritingContextMethods {

    public static final Map<String, BuiltInFunctionValue> methods =
            Map.ofEntries(
                    create(
                            "replace",
                            "Replace the first node by the second one",
                            new String[] {"ctx", "to_replace", "new_node"},
                            new Expr[] {null, null, null},
                            new ReplaceExpr()),
                    create(
                            "insertBefore",
                            "Insert the given new node before the other provided node in its"
                                    + " parent",
                            new String[] {"ctx", "node", "new_node"},
                            new Expr[] {null, null, null},
                            new InsertExpr(Libadalang.RewritingNode::insertBefore)),
                    create(
                            "insertAfter",
                            "Insert the given new node after the other provided node in its parent",
                            new String[] {"ctx", "node", "new_node"},
                            new Expr[] {null, null, null},
                            new InsertExpr(Libadalang.RewritingNode::insertAfter)),
                    create(
                            "insertFirst",
                            "Insert the given new node at the beginning of the given list node",
                            new String[] {"ctx", "list_node", "new_node"},
                            new Expr[] {null, null, null},
                            new InsertExpr(Libadalang.RewritingNode::insertFirst)),
                    create(
                            "insertLast",
                            "Insert the given new node at the end of the given list node",
                            new String[] {"ctx", "list_node", "new_node"},
                            new Expr[] {null, null, null},
                            new InsertExpr(Libadalang.RewritingNode::insertLast)),
                    create(
                            "delete",
                            "Delete the given node from its list node parent",
                            new String[] {"ctx", "to_remove"},
                            new Expr[] {null, null},
                            new DeleteExpr()));

    /** Body for the replace method. */
    public static final class ReplaceExpr extends BuiltinFunctionBody {
        @Child RewritingNodeConverter argToRewritingNode = RewritingNodeConverterNodeGen.create();

        @Override
        public Object executeGeneric(VirtualFrame frame) {
            // Get the associated rewriting context
            Libadalang.RewritingContext ctx =
                    LKQLTypeSystemGen.asRewritingContext(frame.getArguments()[0]);

            // Get the method arguments
            final var toReplaceObject = frame.getArguments()[1];
            if (!LKQLTypeSystemGen.isAdaNode(toReplaceObject)) {
                throw LKQLRuntimeException.wrongType(
                        LKQLTypesHelper.ADA_NODE,
                        LKQLTypesHelper.fromJava(toReplaceObject),
                        this.callNode.getArgList().getArgs()[0]);
            }
            final var toReplace = LKQLTypeSystemGen.asAdaNode(toReplaceObject);
            final var newNode =
                    argToRewritingNode.execute(
                            frame.getArguments()[2], this.callNode.getArgList().getArgs()[1]);

            // Replace the given node and return the rewriting context
            try {
                toReplace.getRewritingNode().replace(newNode);
            } catch (Libadalang.LangkitException e) {
                throw LKQLRuntimeException.fromJavaException(e, this.callNode);
            }
            return ctx;
        }
    }

    /** Body of the insert method. */
    public static final class InsertExpr extends BuiltinFunctionBody {
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
            final var listNodeObject = frame.getArguments()[1];
            if (!LKQLTypeSystemGen.isAdaNode(listNodeObject)) {
                throw LKQLRuntimeException.wrongType(
                        LKQLTypesHelper.ADA_NODE,
                        LKQLTypesHelper.fromJava(listNodeObject),
                        this.callNode.getArgList().getArgs()[0]);
            }
            final var listNode = LKQLTypeSystemGen.asAdaNode(listNodeObject);
            final var newNode =
                    argToRewritingNode.execute(
                            frame.getArguments()[2], this.callNode.getArgList().getArgs()[1]);

            // Apply tbe insertion operation and return the context
            try {
                this.insertOp.accept(listNode.getRewritingNode(), newNode);
            } catch (Libadalang.LangkitException e) {
                throw LKQLRuntimeException.fromJavaException(e, this.callNode);
            }
            return ctx;
        }
    }

    /** Body of the delete method. */
    public static final class DeleteExpr extends BuiltinFunctionBody {
        @Child RewritingNodeConverter argToRewritingNode = RewritingNodeConverterNodeGen.create();

        @Override
        public Object executeGeneric(VirtualFrame frame) {
            // Get the associated rewriting context
            Libadalang.RewritingContext ctx =
                    LKQLTypeSystemGen.asRewritingContext(frame.getArguments()[0]);

            // Get the method arguments
            final var toRemove =
                    argToRewritingNode.execute(
                            frame.getArguments()[1], this.callNode.getArgList().getArgs()[0]);

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

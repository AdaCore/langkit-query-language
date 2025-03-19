//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.built_ins.methods;

import com.adacore.libadalang.Libadalang;
import com.adacore.libadalang.Libadalang.GrammarRule;
import com.adacore.libadalang.Libadalang.MemberReference;
import com.adacore.libadalang.Libadalang.RewritingContext;
import com.adacore.libadalang.Libadalang.RewritingNode;
import com.adacore.lkql_jit.annotations.BuiltInMethod;
import com.adacore.lkql_jit.annotations.BuiltinMethodContainer;
import com.adacore.lkql_jit.built_ins.BuiltInBody;
import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.nodes.utils.RewritingNodeConverter;
import com.adacore.lkql_jit.nodes.utils.RewritingNodeConverterNodeGen;
import com.adacore.lkql_jit.runtime.values.lists.LKQLList;
import com.adacore.lkql_jit.utils.LKQLTypesHelper;
import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;

/** This class contains all methods for the rewriting context type. */
@BuiltinMethodContainer(targetTypes = { LKQLTypesHelper.REWRITING_CONTEXT })
public final class RewritingContextMethods {

    public abstract static class BaseRewritingContextExpr extends BuiltInBody {

        @Child
        RewritingNodeConverter argToRewritingNode = RewritingNodeConverterNodeGen.create();

        public RewritingNode convert(VirtualFrame frame, Object node, boolean ensureTied) {
            return argToRewritingNode.execute(node, ensureTied, this.callNode);
        }
    }

    @BuiltInMethod(name = "replace", doc = "Replace old node by the new one")
    public abstract static class ReplaceExpr extends BaseRewritingContextExpr {

        @Specialization
        public Object executeGeneric(
            VirtualFrame frame,
            RewritingContext ctx,
            Object oldNode,
            Object newNode
        ) {
            // Get the method arguments
            final var toReplace = convert(frame, oldNode, false);
            final var byNode = convert(frame, newNode, true);

            // Replace the given node and return the rewriting context
            try {
                toReplace.replace(byNode);
            } catch (Libadalang.LangkitException e) {
                throw LKQLRuntimeException.fromJavaException(e, this.callNode);
            }
            return ctx;
        }
    }

    @BuiltInMethod(
        name = "set_child",
        doc = "Set the node child, following the given member reference, to the new value"
    )
    public abstract static class SetChildExpr extends BaseRewritingContextExpr {

        @Specialization
        public Object executeGeneric(
            VirtualFrame frame,
            RewritingContext ctx,
            Object node,
            MemberReference memberRef,
            Object newValue
        ) {
            // Get the method arguments
            final var nod = convert(frame, node, false);
            final var newNode = convert(frame, newValue, true);

            // Call the child replacement
            nod.setChild(memberRef, newNode);

            return ctx;
        }
    }

    @BuiltInMethod(
        name = "insert_before",
        doc = "Insert `new_node` before `node` (`node`'s parent needs to be a list node)"
    )
    public abstract static class InsertBefore extends BaseRewritingContextExpr {

        @Specialization
        public Object execute(
            VirtualFrame frame,
            RewritingContext ctx,
            Object node,
            Object newNode
        ) {
            try {
                convert(frame, node, false).insertBefore(convert(frame, newNode, false));
            } catch (Libadalang.LangkitException e) {
                throw LKQLRuntimeException.fromJavaException(e, this.callNode);
            }
            return ctx;
        }
    }

    @BuiltInMethod(
        name = "insert_after",
        doc = "Insert `new_node` after `node` (`node`'s parent needs to be a list node)"
    )
    public abstract static class InsertAfter extends BaseRewritingContextExpr {

        @Specialization
        public Object execute(
            VirtualFrame frame,
            RewritingContext ctx,
            Object node,
            Object newNode
        ) {
            try {
                convert(frame, node, false).insertAfter(convert(frame, newNode, false));
            } catch (Libadalang.LangkitException e) {
                throw LKQLRuntimeException.fromJavaException(e, this.callNode);
            }
            return ctx;
        }
    }

    @BuiltInMethod(name = "add_first", doc = "Insert `new_node` at the beginning of `list_node`")
    public abstract static class AddFirst extends BaseRewritingContextExpr {

        @Specialization
        public Object execute(
            VirtualFrame frame,
            RewritingContext ctx,
            Object node,
            Object newNode
        ) {
            try {
                convert(frame, node, false).insertFirst(convert(frame, newNode, false));
            } catch (Libadalang.LangkitException e) {
                throw LKQLRuntimeException.fromJavaException(e, this.callNode);
            }
            return ctx;
        }
    }

    @BuiltInMethod(name = "add_last", doc = "Insert `new_node` at the end of `list_node`")
    public abstract static class AddLast extends BaseRewritingContextExpr {

        @Specialization
        public Object execute(
            VirtualFrame frame,
            RewritingContext ctx,
            Object node,
            Object newNode
        ) {
            try {
                convert(frame, node, false).insertLast(convert(frame, newNode, false));
            } catch (Libadalang.LangkitException e) {
                throw LKQLRuntimeException.fromJavaException(e, this.callNode);
            }
            return ctx;
        }
    }

    @BuiltInMethod(
        name = "remove",
        doc = "Delete the given node from its parent (parent needs to be a list node)"
    )
    public abstract static class RemoveExpr extends BaseRewritingContextExpr {

        @Specialization
        public Object executeGeneric(VirtualFrame frame, RewritingContext ctx, Object objToRemove) {
            // Call the removing method and return the context
            try {
                convert(frame, objToRemove, false).removeFromParent();
            } catch (Libadalang.LangkitException e) {
                throw LKQLRuntimeException.fromJavaException(e, this.callNode);
            }
            return ctx;
        }
    }

    @BuiltInMethod(
        name = "create_from_template",
        doc = "Create a new node from the provided template, filling '{}' with provided" +
        " argument, and parsing the template with the specified grammar rule"
    )
    public abstract static class CreateFromTemplateExpr extends BaseRewritingContextExpr {

        @TruffleBoundary
        private static GrammarRule ruleFromString(String ruleName) {
            return GrammarRule.valueOf(ruleName.toUpperCase());
        }

        @Specialization
        public RewritingNode doGeneric(
            VirtualFrame frame,
            RewritingContext ctx,
            String template,
            String grammarRule,
            LKQLList arguments
        ) {
            // Translate the provided LKQL list into a rewriting node list
            final var args = new RewritingNode[(int) arguments.size()];
            for (int i = 0; i < args.length; i++) {
                args[i] = convert(frame, arguments.get(i), true);
            }

            // Then call the internal function to process the template
            try {
                return ctx.createFromTemplate(template, ruleFromString(grammarRule), args);
            } catch (Exception e) {
                throw LKQLRuntimeException.fromJavaException(e, this.callNode);
            }
        }
    }
}

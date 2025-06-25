//
//  Copyright (C) 2005-2025, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.built_ins.methods;

import com.adacore.langkit_support.LangkitSupport.MemberReferenceInterface;
import com.adacore.langkit_support.LangkitSupport.RewritingContextInterface;
import com.adacore.langkit_support.LangkitSupport.RewritingNodeInterface;
import com.adacore.libadalang.Libadalang;
import com.adacore.lkql_jit.annotations.BuiltInMethod;
import com.adacore.lkql_jit.annotations.BuiltinMethodContainer;
import com.adacore.lkql_jit.built_ins.BuiltInBody;
import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.nodes.utils.RewritingNodeConverter;
import com.adacore.lkql_jit.nodes.utils.RewritingNodeConverterNodeGen;
import com.adacore.lkql_jit.runtime.values.lists.LKQLList;
import com.adacore.lkql_jit.utils.LKQLTypesHelper;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;

/** This class contains all methods for the rewriting context type. */
@BuiltinMethodContainer(targetTypes = { LKQLTypesHelper.REWRITING_CONTEXT })
public final class RewritingContextMethods {

    public abstract static class BaseRewritingContextExpr extends BuiltInBody {

        @Child
        RewritingNodeConverter argToRewritingNode = RewritingNodeConverterNodeGen.create();

        public RewritingNodeInterface convert(VirtualFrame frame, Object node, boolean ensureTied) {
            return argToRewritingNode.execute(node, ensureTied, this.callNode);
        }
    }

    @BuiltInMethod(name = "replace", doc = "Replace old node by the new one")
    public abstract static class ReplaceExpr extends BaseRewritingContextExpr {

        @Specialization
        public Object executeGeneric(
            VirtualFrame frame,
            RewritingContextInterface ctx,
            Object oldNode,
            Object newNode
        ) {
            // Get the method arguments
            final var toReplace = convert(frame, oldNode, false);
            final var byNode = convert(frame, newNode, true);

            // Replace the given node and return the rewriting context
            try {
                if (byNode != null) {
                    toReplace.replace(byNode);
                }
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
            RewritingContextInterface ctx,
            Object node,
            MemberReferenceInterface memberRef,
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
            RewritingContextInterface ctx,
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
            RewritingContextInterface ctx,
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
            RewritingContextInterface ctx,
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
            RewritingContextInterface ctx,
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
        public Object executeGeneric(
            VirtualFrame frame,
            RewritingContextInterface ctx,
            Object objToRemove
        ) {
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
        doc = """
        Create a new node from the provided template, filling '{}' with provided
        argument, and parsing the template with the specified grammar rule. Example:

        .. code-block:: lkql

          # Create a new BinOp node with OpAdd as operator, representing the addition of the value
          # expressed by `my_other_node`, and "42".
          ctx.create_from_template(
              "{} + 42",
              "expr_rule",
              [my_other_node]
          )
        """
    )
    public abstract static class CreateFromTemplateExpr extends BaseRewritingContextExpr {

        @Specialization
        public RewritingNodeInterface doGeneric(
            VirtualFrame frame,
            RewritingContextInterface ctx,
            String template,
            String grammarRule,
            LKQLList arguments
        ) {
            // Translate the provided LKQL list into a rewriting node list
            final var args = new RewritingNodeInterface[(int) arguments.size()];
            for (int i = 0; i < args.length; i++) {
                args[i] = convert(frame, arguments.get(i), true);
            }

            // Then call the internal function to process the template
            try {
                return ctx.createFromTemplate(template, grammarRule, args);
            } catch (Exception e) {
                throw LKQLRuntimeException.fromJavaException(e, this.callNode);
            }
        }
    }
}

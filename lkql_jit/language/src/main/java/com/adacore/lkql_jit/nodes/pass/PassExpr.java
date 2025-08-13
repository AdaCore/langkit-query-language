//
//  Copyright (C) 2005-2025, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.pass;

import com.adacore.lkql_jit.LKQLLanguage;
import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.adacore.lkql_jit.nodes.expressions.value_read.ReadArgument;
import com.adacore.lkql_jit.runtime.values.DynamicAdaNode;
import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.NodeChild;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.MaterializedFrame;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.source.SourceSection;
import java.util.*;

/**
 * This class is a runtime instance of a rewriting pass.
 * At the runtime passes are represented as functions AST -> AST.
 * A pass expr is the body of the synthesized function and contains
 * all the logic needed to execute a pass.
 */
@NodeChild(value = "readInput", type = ReadArgument.class)
public abstract class PassExpr extends Expr {

    @Child
    private AddBlock add;

    @Child
    private DelBlock del;

    @Child
    private RewriteBlock rewrite;

    /**
     * the previous pass slot, None if this is the first pass in the chain
     */
    private final Optional<Integer> previousSlot;

    protected PassExpr(
        SourceSection location,
        Optional<Integer> previousSlot,
        AddBlock add,
        DelBlock del,
        RewriteBlock rewrite
    ) {
        super(location);
        this.previousSlot = previousSlot;
        this.add = add;
        this.del = del;
        this.rewrite = rewrite;
    }

    // TODO insert dynamic checks to verify AST integrity wrt. add / del blocks
    // this can be removed if pattern matching exhaustivity is pre-checked
    @Specialization
    public Object onDynamicAdaNode(VirtualFrame frame, DynamicAdaNode input) {
        final var typingContext = LKQLLanguage.getContext(this).getTypingContext();
        for (var c : add.classes) {
            typingContext.add(c.name);
        }
        final var updatedTree = getUpdatedTree(input, frame.materialize());
        typingContext.removeAll(del.classes);
        return updatedTree;
    }

    @Fallback
    public Object onOther(VirtualFrame frame, Object _obj1) {
        throw LKQLRuntimeException.shouldNotExecute(this);
    }

    @Override
    public String toString(int indentLevel) {
        return this.nodeRepresentation(
                indentLevel,
                new String[] { "previousSlot" },
                new Object[] { previousSlot }
            );
    }

    // getters

    public AddBlock getAdd() {
        return add;
    }

    public DelBlock getDel() {
        return del;
    }

    public RewriteBlock getRewrite() {
        return rewrite;
    }

    public Optional<Integer> getPreviousSlot() {
        return previousSlot;
    }

    // core logic

    // bottom up rewriting of the tree
    @TruffleBoundary
    private DynamicAdaNode getUpdatedTree(DynamicAdaNode tree, MaterializedFrame frame) {
        // recurse on children first (bottom up)
        for (var child : List.copyOf(tree.children.entrySet())) {
            tree.children.put(child.getKey(), getUpdatedTree(child.getValue(), frame));
        }

        // then try to rewrite the current node
        for (var arm : rewrite.getArms()) {
            var rewriteResult = arm.executeArm(frame, tree);
            if (rewriteResult != null) {
                return (DynamicAdaNode) rewriteResult;
            }
        }

        return tree;
    }
}

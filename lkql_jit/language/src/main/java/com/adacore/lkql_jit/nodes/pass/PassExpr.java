//
//  Copyright (C) 2005-2025, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.pass;

import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.adacore.lkql_jit.nodes.expressions.value_read.ReadArgument;
import com.adacore.lkql_jit.runtime.values.DynamicAdaNode;
import com.adacore.lkql_jit.runtime.values.LKQLFunction;
import com.adacore.lkql_jit.utils.functions.FrameUtils;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.NodeChild;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.interop.ArityException;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.interop.UnsupportedTypeException;
import com.oracle.truffle.api.source.SourceSection;
import java.util.HashMap;
import java.util.Optional;

/**
 * This class is a runtime instance of a rewriting pass.
 * At the runtime passes are represented as functions AST -> AST
 * which allows to call them recursively and pass an AST as argument.
 * A pass expr is the body of the synthesized function and contains
 * all the logic needed to execute a pass.
 */
@NodeChild(value = "readContext", type = ReadArgument.class)
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

    @Specialization
    public Object onDynamicAdaNode(VirtualFrame frame, DynamicAdaNode input) {
        if (previousSlot.isEmpty()) {
            // First pass in the chain
            // Execution differs from the others
            System.out.println("first rewriting pass in chain");
            return input;
        }

        // NB: in this branch previous slot is always present

        // start by calling previous pass in the chain
        final var previousPass = (LKQLFunction) FrameUtils.readLocal(frame, previousSlot.get());
        try {
            InteropLibrary.getUncached().execute(previousPass, input);
        } catch (UnsupportedTypeException | ArityException | UnsupportedMessageException e) {
            e.printStackTrace();
        }

        // 1) iterate over nodes and test match arms
        var updatedTree = getUpdatedTree(input, frame);
        // 2) TODO iterate over nodes and verify add / delete ? (optional)

        return updatedTree;
    }

    @Fallback
    public Object onOther(VirtualFrame frame, Object _obj1, Object _obj2) {
        throw LKQLRuntimeException.shouldNotExecute(this);
    }

    @Override
    public String toString(int indentLevel) {
        return this.nodeRepresentation(indentLevel, new String[] {}, new Object[] {});
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
    DynamicAdaNode getUpdatedTree(DynamicAdaNode tree, VirtualFrame frame) {
        // new node instanciated by shallow copy
        DynamicAdaNode updatedNode = new DynamicAdaNode(tree.kind, new HashMap<>(), tree.fields);

        // recurse on children first (bottom up)
        for (var child : tree.children.entrySet()) {
            var updatedChild = getUpdatedTree(child.getValue(), frame);
            updatedNode.children.put(child.getKey(), updatedChild);
        }

        // then try to rewrite the current node
        for (var arm : rewrite.arms) {
            var rewriteResult = arm.executeArm(frame, tree);
            if (rewriteResult != null) return (DynamicAdaNode) rewriteResult;
        }

        // if node doesnt need to be rewriten just return it
        return updatedNode;
    }
}

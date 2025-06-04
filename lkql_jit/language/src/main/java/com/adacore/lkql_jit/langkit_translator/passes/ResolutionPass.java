//
//  Copyright (C) 2005-2025, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.langkit_translator.passes;

import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.nodes.declarations.FunctionDeclaration;
import com.adacore.lkql_jit.nodes.expressions.DynamicConstructorCall;
import com.adacore.lkql_jit.nodes.pass.PassExpr;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.nodes.NodeVisitor;
import java.util.ArrayList;
import java.util.Map;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * This pass is run on the lowered truffle AST before executing any code.
 * Its role is to typecheck rewriting passes.
 */
public final class ResolutionPass {

    /**
     * This is the main entry point of the pass and the only
     * public method that should be called
     */
    public void passEntry(Node treeRoot) {
        Map<Integer, PassExpr> slotMap = collectOfType(FunctionDeclaration.class, treeRoot)
            .stream()
            .flatMap(fn -> {
                if (
                    fn.getFunctionExpression().getFunctionRootNode().getBody() instanceof
                    PassExpr passExpr
                ) {
                    return Stream.of(Map.entry(fn.slot, passExpr));
                } else {
                    return Stream.empty();
                }
            })
            .collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue));

        final var roots = slotMap
            .values()
            .stream()
            .filter(e -> e.getPreviousSlot().isEmpty())
            .toList();

        for (PassExpr root : roots) checkRewritingPass(root, PassContext.initial());
    }

    /**
     * checks validity of a pass, for now only constructor name and
     * arity are checked but this could be extended in the future
     * @param passExpr
     * @param ctx
     */
    private void checkRewritingPass(PassExpr passExpr, PassContext ctx) {
        ctx.update(passExpr.getAdd());

        for (var constructor : collectOfType(DynamicConstructorCall.class, passExpr.getRewrite())) {
            final var clazz = ctx.env.get(constructor.nodeKind);
            if (clazz == null) throw LKQLRuntimeException.fromMessage("class does not exist");
            if (
                clazz.fields().size() != constructor.arity()
            ) throw LKQLRuntimeException.fromMessage("wrong arity");
        }

        ctx.update(passExpr.getDel());
    }

    private <T extends Node> ArrayList<T> collectOfType(Class<T> clazz, Node tree) {
        var collector = new NodeVisitor() {
            ArrayList<T> collected = new ArrayList<>();

            @Override
            public boolean visit(Node node) {
                if (clazz.isInstance(node)) {
                    collected.add(clazz.cast(node));
                }
                return true;
            }
        };
        tree.accept(collector);
        return collector.collected;
    }
}

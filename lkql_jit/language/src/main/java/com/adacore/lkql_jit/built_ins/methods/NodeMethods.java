//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.built_ins.methods;

import static com.adacore.lkql_jit.built_ins.BuiltInMethodFactory.createAttribute;
import static com.adacore.lkql_jit.built_ins.BuiltInMethodFactory.createMethod;

import com.adacore.libadalang.Libadalang;
import com.adacore.lkql_jit.LKQLLanguage;
import com.adacore.lkql_jit.LKQLTypeSystemGen;
import com.adacore.lkql_jit.built_ins.AbstractBuiltInFunctionBody;
import com.adacore.lkql_jit.built_ins.BuiltInMethodFactory;
import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.adacore.lkql_jit.runtime.values.LKQLNull;
import com.adacore.lkql_jit.runtime.values.LKQLUnit;
import com.adacore.lkql_jit.runtime.values.lists.LKQLList;
import com.adacore.lkql_jit.utils.LKQLTypesHelper;
import com.adacore.lkql_jit.utils.functions.ObjectUtils;
import com.adacore.lkql_jit.utils.functions.ReflectionUtils;
import com.adacore.lkql_jit.utils.functions.StringUtils;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.UnexpectedResultException;
import java.util.ArrayList;
import java.util.Map;

/**
 * This class contains all built-in methods for the node type in the LKQL language.
 *
 * @author Hugo GUERRIER
 */
public final class NodeMethods {

    public static final Map<String, BuiltInMethodFactory> methods =
            Map.ofEntries(
                    createAttribute(
                            "children_count",
                            "Given a node, return the count of its children",
                            new ChildrenCountExpr()),
                    createAttribute(
                            "children",
                            "Given a node, get the list of all its children",
                            new ChildrenExpr()),
                    createAttribute(
                            "parent", "Given a node, get the parent of it", new ParentExpr()),
                    createAttribute(
                            "dump",
                            "Given an ast node, return a structured dump of the subtree",
                            new DumpExpr()),
                    createAttribute("text", "Given an ast node, return its text", new TextExpr()),
                    createAttribute(
                            "image", "Given an ast node, return its image", new ImageExpr()),
                    createAttribute(
                            "unit", "Given an ast node, return its analysis unit", new UnitExpr()),
                    createAttribute(
                            "kind", "Return the kind of this node, as a string", new KindExpr()),
                    createAttribute(
                            "tokens",
                            "Given a node, return an iterator on its tokens",
                            new TokensExpr()),
                    createMethod(
                            "same_tokens",
                            "Return whether two nodes have the same tokens, ignoring trivias",
                            new String[] {"other"},
                            new Expr[] {null},
                            new SameTokensExpr()));

    /** Expression of the "children" method. */
    public static final class ChildrenExpr extends AbstractBuiltInFunctionBody {
        @Override
        public Object executeGeneric(VirtualFrame frame) {
            // Get the node
            Libadalang.AdaNode node = LKQLTypeSystemGen.asAdaNode(frame.getArguments()[0]);

            // Prepare the result
            int childrenCount = node.getChildrenCount();
            Libadalang.AdaNode[] res = new Libadalang.AdaNode[childrenCount];
            for (int i = 0; i < childrenCount; i++) {
                Libadalang.AdaNode child = node.getChild(i);
                res[i] = (child.isNone() ? LKQLNull.INSTANCE : child);
            }

            // Return the list value
            return new LKQLList(res);
        }
    }

    /** Expression of the "parent" method. */
    public static final class ParentExpr extends AbstractBuiltInFunctionBody {
        @Override
        public Object executeGeneric(VirtualFrame frame) {
            Libadalang.AdaNode parent =
                    LKQLTypeSystemGen.asAdaNode(frame.getArguments()[0]).parent();
            return parent.isNone() ? LKQLNull.INSTANCE : parent;
        }
    }

    /** Expression of the "children_count" method. */
    public static final class ChildrenCountExpr extends AbstractBuiltInFunctionBody {
        @Override
        public Object executeGeneric(VirtualFrame frame) {
            return (long) LKQLTypeSystemGen.asAdaNode(frame.getArguments()[0]).getChildrenCount();
        }
    }

    /** Expression of the "dump" method. */
    public static final class DumpExpr extends AbstractBuiltInFunctionBody {
        @Override
        public Object executeGeneric(VirtualFrame frame) {
            LKQLLanguage.getContext(this)
                    .print(LKQLTypeSystemGen.asAdaNode(frame.getArguments()[0]).dumpTree());
            return LKQLUnit.INSTANCE;
        }
    }

    /** Expression of the "text" method. */
    public static final class TextExpr extends AbstractBuiltInFunctionBody {
        @Override
        public Object executeGeneric(VirtualFrame frame) {
            return LKQLTypeSystemGen.asAdaNode(frame.getArguments()[0]).getText();
        }
    }

    /** Expression of the "image" method. */
    public static final class ImageExpr extends AbstractBuiltInFunctionBody {
        @Override
        public Object executeGeneric(VirtualFrame frame) {
            return LKQLTypeSystemGen.asAdaNode(frame.getArguments()[0]).getImage();
        }
    }

    /** Expression of the "unit" method. */
    public static final class UnitExpr extends AbstractBuiltInFunctionBody {
        @Override
        public Object executeGeneric(VirtualFrame frame) {
            return LKQLTypeSystemGen.asAdaNode(frame.getArguments()[0]).getUnit();
        }
    }

    /** Expression of the "kind" method. */
    public static final class KindExpr extends AbstractBuiltInFunctionBody {
        @Override
        public Object executeGeneric(VirtualFrame frame) {
            return ReflectionUtils.getClassSimpleName(frame.getArguments()[0]);
        }
    }

    /** Expression of the "tokens" method. */
    public static final class TokensExpr extends AbstractBuiltInFunctionBody {
        @Override
        public Object executeGeneric(VirtualFrame frame) {
            // Get the node
            Libadalang.AdaNode node = LKQLTypeSystemGen.asAdaNode(frame.getArguments()[0]);

            // Prepare the result
            ArrayList<Libadalang.Token> resList = new ArrayList<>();
            Libadalang.Token startToken = node.tokenStart();
            Libadalang.Token endToken = node.tokenEnd();
            resList.add(startToken);
            while (!startToken.equals(endToken)) {
                startToken = startToken.next();
                resList.add(startToken);
            }

            // Return the result
            return new LKQLList(resList.toArray(new Libadalang.Token[0]));
        }
    }

    /** Expression of the "same_tokens" method. */
    public static final class SameTokensExpr extends AbstractBuiltInFunctionBody {
        @Override
        public Object executeGeneric(VirtualFrame frame) {
            // Get the nodes to compare
            Libadalang.AdaNode leftNode = LKQLTypeSystemGen.asAdaNode(frame.getArguments()[0]);
            Libadalang.AdaNode rightNode;
            try {
                rightNode = LKQLTypeSystemGen.expectAdaNode(frame.getArguments()[1]);
            } catch (UnexpectedResultException e) {
                throw LKQLRuntimeException.wrongType(
                        LKQLTypesHelper.ADA_NODE,
                        LKQLTypesHelper.fromJava(e.getResult()),
                        this.callNode.getArgList().getArgs()[0]);
            }

            // Get the tokens
            Libadalang.Token leftToken = leftNode.tokenStart();
            Libadalang.Token rightToken = rightNode.tokenStart();

            Libadalang.Token leftEnd = leftNode.tokenEnd();
            Libadalang.Token rightEnd = rightNode.tokenEnd();

            // Compare all the node's tokens
            while (!leftToken.isNone() && !rightToken.isNone()) {
                if (leftToken.kind != rightToken.kind) return false;
                if (leftToken.kind == Libadalang.TokenKind.ADA_IDENTIFIER) {
                    if (!ObjectUtils.equals(
                            StringUtils.toLowerCase(leftToken.getText()),
                            StringUtils.toLowerCase(rightToken.getText()))) return false;
                } else if (!ObjectUtils.equals(leftToken.getText(), rightToken.getText())) {
                    return false;
                }

                if (leftToken.equals(leftEnd)) {
                    return rightToken.equals(rightEnd);
                } else if (rightToken.equals(rightEnd)) {
                    return false;
                }

                leftToken = next(leftToken);
                rightToken = next(rightToken);
            }

            // The default return value
            return true;
        }

        /**
         * Get the next token from the given one ignoring the trivias
         *
         * @param t The token to get the next from
         * @return The next token
         */
        private static Libadalang.Token next(Libadalang.Token t) {
            Libadalang.Token res = t.next();
            while (!res.isNone() && res.triviaIndex != 0) {
                res = res.next();
            }
            return res;
        }
    }
}

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

import com.adacore.libadalang.Libadalang;
import com.adacore.lkql_jit.LKQLLanguage;
import com.adacore.lkql_jit.LKQLTypeSystemGen;
import com.adacore.lkql_jit.built_ins.BuiltInFunctionValue;
import com.adacore.lkql_jit.built_ins.BuiltinFunctionBody;
import com.adacore.lkql_jit.built_ins.values.LKQLNull;
import com.adacore.lkql_jit.built_ins.values.LKQLUnit;
import com.adacore.lkql_jit.built_ins.values.lists.LKQLArrayList;
import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.adacore.lkql_jit.utils.LKQLTypesHelper;
import com.adacore.lkql_jit.utils.functions.ObjectUtils;
import com.adacore.lkql_jit.utils.functions.ReflectionUtils;
import com.adacore.lkql_jit.utils.functions.StringUtils;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.UnexpectedResultException;
import java.util.ArrayList;

/**
 * This class contains all built-in methods for the node type in the LKQL language.
 *
 * @author Hugo GUERRIER
 */
public final class NodeMethods extends CommonMethods {

    // ----- Attributes -----

    /** The only instance of the method collection. */
    private static NodeMethods instance = null;

    // ----- Constructors -----

    /** Private constructors. */
    private NodeMethods() {
        super();
    }

    /**
     * Get the only instance of the method collection.
     *
     * @return The instance of the node methods.
     */
    public static NodeMethods getInstance() {
        if (instance == null) {
            instance = new NodeMethods();
        }
        return instance;
    }

    /**
     * @see CommonMethods#initMethods()
     */
    @Override
    protected void initMethods() {
        super.initMethods();
        this.methods.put(
                "children_count",
                new BuiltInFunctionValue(
                        "children_count",
                        "Given a node, return the count of its children",
                        new String[] {"node"},
                        new Expr[] {null},
                        new ChildrenCountExpr()));
        this.methods.put(
                "children",
                new BuiltInFunctionValue(
                        "children",
                        "Given a node, get the list of all its children",
                        new String[] {"node"},
                        new Expr[] {null},
                        new ChildrenExpr()));
        this.methods.put(
                "parent",
                new BuiltInFunctionValue(
                        "parent",
                        "Given a node, get the parent of it",
                        new String[] {"node"},
                        new Expr[] {null},
                        new ParentExpr()));
        this.methods.put(
                "dump",
                new BuiltInFunctionValue(
                        "dump",
                        "Given an ast node, return a structured dump of the subtree",
                        new String[] {"node"},
                        new Expr[] {null},
                        new DumpExpr()));
        this.methods.put(
                "text",
                new BuiltInFunctionValue(
                        "text",
                        "Given an ast node, return its text",
                        new String[] {"node"},
                        new Expr[] {null},
                        new TextExpr()));
        this.methods.put(
                "image",
                new BuiltInFunctionValue(
                        "image",
                        "Given an ast node, return its image",
                        new String[] {"node"},
                        new Expr[] {null},
                        new ImageExpr()));
        this.methods.put(
                "unit",
                new BuiltInFunctionValue(
                        "unit",
                        "Given an ast node, return its analysis unit",
                        new String[] {"node"},
                        new Expr[] {null},
                        new UnitExpr()));
        this.methods.put(
                "kind",
                new BuiltInFunctionValue(
                        "kind",
                        "Return the kind of this node, as a string",
                        new String[] {"node"},
                        new Expr[] {null},
                        new KindExpr()));
        this.methods.put(
                "tokens",
                new BuiltInFunctionValue(
                        "tokens",
                        "Given a node, return an iterator on its tokens",
                        new String[] {"node"},
                        new Expr[] {null},
                        new TokensExpr()));
        this.methods.put(
                "same_tokens",
                new BuiltInFunctionValue(
                        "same_tokens",
                        "Return whether two nodes have the same tokens, ignoring trivias",
                        new String[] {"node", "other"},
                        new Expr[] {null, null},
                        new SameTokensExpr()));
    }

    // ----- Override methods -----

    /**
     * @see BuiltInMethods#getType()
     */
    @Override
    public String getType() {
        return LKQLTypesHelper.ADA_NODE;
    }

    // ----- Inner classes -----

    /** Expression of the "children" method. */
    public static final class ChildrenExpr extends BuiltinFunctionBody {
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
            return new LKQLArrayList(res);
        }
    }

    /** Expression of the "parent" method. */
    public static final class ParentExpr extends BuiltinFunctionBody {
        @Override
        public Object executeGeneric(VirtualFrame frame) {
            Libadalang.AdaNode parent =
                    LKQLTypeSystemGen.asAdaNode(frame.getArguments()[0]).parent();
            return parent.isNone() ? LKQLNull.INSTANCE : parent;
        }
    }

    /** Expression of the "children_count" method. */
    public static final class ChildrenCountExpr extends BuiltinFunctionBody {
        @Override
        public Object executeGeneric(VirtualFrame frame) {
            return (long) LKQLTypeSystemGen.asAdaNode(frame.getArguments()[0]).getChildrenCount();
        }
    }

    /** Expression of the "dump" method. */
    public static final class DumpExpr extends BuiltinFunctionBody {
        @Override
        public Object executeGeneric(VirtualFrame frame) {
            LKQLLanguage.getContext(this)
                    .print(LKQLTypeSystemGen.asAdaNode(frame.getArguments()[0]).dumpAST());
            return LKQLUnit.INSTANCE;
        }
    }

    /** Expression of the "text" method. */
    public static final class TextExpr extends BuiltinFunctionBody {
        @Override
        public Object executeGeneric(VirtualFrame frame) {
            return LKQLTypeSystemGen.asAdaNode(frame.getArguments()[0]).getText();
        }
    }

    /** Expression of the "image" method. */
    public static final class ImageExpr extends BuiltinFunctionBody {
        @Override
        public Object executeGeneric(VirtualFrame frame) {
            return LKQLTypeSystemGen.asAdaNode(frame.getArguments()[0]).getImage();
        }
    }

    /** Expression of the "unit" method. */
    public static final class UnitExpr extends BuiltinFunctionBody {
        @Override
        public Object executeGeneric(VirtualFrame frame) {
            return LKQLTypeSystemGen.asAdaNode(frame.getArguments()[0]).getUnit();
        }
    }

    /** Expression of the "kind" method. */
    public static final class KindExpr extends BuiltinFunctionBody {
        @Override
        public Object executeGeneric(VirtualFrame frame) {
            return ReflectionUtils.getClassSimpleName(frame.getArguments()[0]);
        }
    }

    /** Expression of the "tokens" method. */
    public static final class TokensExpr extends BuiltinFunctionBody {
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
            return new LKQLArrayList(resList.toArray(new Libadalang.Token[0]));
        }
    }

    /** Expression of the "same_tokens" method. */
    public static final class SameTokensExpr extends BuiltinFunctionBody {
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

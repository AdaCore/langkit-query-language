//
//  Copyright (C) 2005-2025, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.built_ins.methods;

import com.adacore.langkit_support.LangkitSupport;
import com.adacore.langkit_support.LangkitSupport.AnalysisUnit;
import com.adacore.langkit_support.LangkitSupport.NodeInterface;
import com.adacore.libadalang.Libadalang;
import com.adacore.lkql_jit.LKQLLanguage;
import com.adacore.lkql_jit.annotations.BuiltInMethod;
import com.adacore.lkql_jit.annotations.BuiltinMethodContainer;
import com.adacore.lkql_jit.built_ins.BuiltInBody;
import com.adacore.lkql_jit.runtime.values.LKQLNull;
import com.adacore.lkql_jit.runtime.values.LKQLUnit;
import com.adacore.lkql_jit.runtime.values.lists.LKQLList;
import com.adacore.lkql_jit.utils.LKQLTypesHelper;
import com.adacore.lkql_jit.utils.functions.ObjectUtils;
import com.adacore.lkql_jit.utils.functions.ReflectionUtils;
import com.adacore.lkql_jit.utils.functions.StringUtils;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Specialization;
import java.util.ArrayList;

/**
 * This class contains all built-in methods for the node type in the LKQL language.
 *
 * @author Hugo GUERRIER
 */
@BuiltinMethodContainer(targetTypes = { LKQLTypesHelper.NODE_INTERFACE })
public final class NodeMethods {

    @BuiltInMethod(name = "children", doc = "Return the node's children", isProperty = true)
    abstract static class ChildrenExpr extends BuiltInBody {

        @Specialization
        @CompilerDirectives.TruffleBoundary
        public LKQLList onNode(NodeInterface self) {
            int childrenCount = self.getChildrenCount();
            NodeInterface[] res = new NodeInterface[childrenCount];
            for (int i = 0; i < childrenCount; i++) {
                NodeInterface child = self.getChild(i);
                res[i] = (child.isNone() ? LKQLNull.INSTANCE : child);
            }
            return new LKQLList(res);
        }
    }

    @BuiltInMethod(name = "parent", doc = "Return the node's parent", isProperty = true)
    abstract static class ParentExpr extends BuiltInBody {

        @Specialization
        public NodeInterface onNode(NodeInterface self) {
            NodeInterface parent = self.parent();
            return parent.isNone() ? LKQLNull.INSTANCE : parent;
        }
    }

    @BuiltInMethod(
        name = "children_count",
        doc = "Return the node's children count",
        isProperty = true
    )
    abstract static class ChildrenCountExpr extends BuiltInBody {

        @Specialization
        public long onNode(NodeInterface self) {
            return (long) self.getChildrenCount();
        }
    }

    @BuiltInMethod(
        name = "dump",
        doc = "Dump the node's content in a structured tree",
        isProperty = true
    )
    abstract static class DumpExpr extends BuiltInBody {

        @Specialization
        public LKQLUnit onNode(NodeInterface self) {
            LKQLLanguage.getContext(this).print(self.dumpTree());
            return LKQLUnit.INSTANCE;
        }
    }

    @BuiltInMethod(name = "text", doc = "Return the node's text", isProperty = true)
    abstract static class TextExpr extends BuiltInBody {

        @Specialization
        public String onNode(NodeInterface self) {
            return self.getText();
        }
    }

    @BuiltInMethod(name = "image", doc = "Return the node's image", isProperty = true)
    abstract static class ImageExpr extends BuiltInBody {

        @Specialization
        public String onNode(NodeInterface self) {
            return self.getImage();
        }
    }

    @BuiltInMethod(name = "unit", doc = "Return the node's analysis unit", isProperty = true)
    abstract static class UnitExpr extends BuiltInBody {

        @Specialization
        public AnalysisUnit onNode(NodeInterface self) {
            return self.getUnit();
        }
    }

    @BuiltInMethod(name = "kind", doc = "Return the node's kind", isProperty = true)
    abstract static class KindExpr extends BuiltInBody {

        @Specialization
        public String onNode(NodeInterface self) {
            return ReflectionUtils.getClassSimpleName(self);
        }
    }

    @BuiltInMethod(name = "tokens", doc = "Return the node's tokens", isProperty = true)
    abstract static class TokensExpr extends BuiltInBody {

        @Specialization
        public LKQLList onNode(NodeInterface self) {
            // Prepare the result
            ArrayList<LangkitSupport.TokenInterface> resList = new ArrayList<>();
            LangkitSupport.TokenInterface startToken = self.tokenStart();
            LangkitSupport.TokenInterface endToken = self.tokenEnd();
            resList.add(startToken);
            while (!startToken.equals(endToken)) {
                startToken = startToken.next();
                resList.add(startToken);
            }

            // Return the result
            return new LKQLList(resList.toArray(new LangkitSupport.TokenInterface[0]));
        }
    }

    @BuiltInMethod(
        name = "same_tokens",
        doc = "Return whether two nodes have the same tokens, ignoring trivias"
    )
    abstract static class SameTokensExpr extends BuiltInBody {

        @Specialization
        protected boolean onNode(NodeInterface leftNode, NodeInterface rightNode) {
            // Get the tokens
            LangkitSupport.TokenInterface leftToken = leftNode.tokenStart();
            LangkitSupport.TokenInterface rightToken = rightNode.tokenStart();

            LangkitSupport.TokenInterface leftEnd = leftNode.tokenEnd();
            LangkitSupport.TokenInterface rightEnd = rightNode.tokenEnd();

            // Compare all the node's tokens
            while (!leftToken.isNone() && !rightToken.isNone()) {
                if (leftToken.getKind() != rightToken.getKind()) return false;
                if (leftToken.getKind() == Libadalang.TokenKind.ADA_IDENTIFIER) {
                    if (
                        !ObjectUtils.equals(
                            StringUtils.toLowerCase(leftToken.getText()),
                            StringUtils.toLowerCase(rightToken.getText())
                        )
                    ) return false;
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

        /** Get the next token from the given one ignoring the trivias. */
        private static LangkitSupport.TokenInterface next(LangkitSupport.TokenInterface t) {
            LangkitSupport.TokenInterface res = t.next();
            while (!res.isNone() && res.isTrivia()) {
                res = res.next();
            }
            return res;
        }
    }
}

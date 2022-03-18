/*----------------------------------------------------------------------------
--                             L K Q L   J I T                              --
--                                                                          --
--                     Copyright (C) 2022, AdaCore                          --
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
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
-----------------------------------------------------------------------------*/

package com.adacore.lkql_jit.runtime.built_ins.methods;

import com.adacore.lkql_jit.LKQLLanguage;
import com.adacore.lkql_jit.utils.LKQLTypesHelper;
import com.adacore.lkql_jit.utils.util_functions.ReflectionUtils;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.adacore.libadalang.Libadalang;
import com.adacore.lkql_jit.LKQLTypeSystemGen;
import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.adacore.lkql_jit.runtime.built_ins.BuiltInExpr;
import com.adacore.lkql_jit.runtime.built_ins.BuiltInFunctionValue;
import com.adacore.lkql_jit.runtime.values.ListValue;
import com.adacore.lkql_jit.runtime.values.NullValue;
import com.adacore.lkql_jit.runtime.values.UnitValue;


/**
 * This class contains all built-in methods for the node type in the LKQL language
 *
 * @author Hugo GUERRIER
 */
public final class NodeMethods extends CommonMethods {

    // ----- Attributes -----

    /** The only instance of the method collection */
    private static NodeMethods instance = null;

    // ----- Constructors -----

    /**
     * Private constructors
     */
    private NodeMethods() {
        super();
    }

    /**
     * Get the only instance of the method collection
     *
     * @return The instance
     */
    public static NodeMethods getInstance() {
        if(instance == null) {
            instance = new NodeMethods();
        }
        return instance;
    }

    /** @see com.adacore.lkql_jit.runtime.built_ins.methods.CommonMethods#initMethods()*/
    @Override
    protected void initMethods() {
        super.initMethods();
        this.methods.put("children_count", new BuiltInFunctionValue(
                "children_count",
                "Given a node, return the count of its children",
                new String[]{"node"},
                new Expr[]{null},
                new ChildrenCountExpr()
        ));
        this.methods.put("children", new BuiltInFunctionValue(
                "children",
                "Given a node, get the list of all its children",
                new String[]{"node"},
                new Expr[]{null},
                new ChildrenExpr()
        ));
        this.methods.put("parent", new BuiltInFunctionValue(
                "parent",
                "Given a node, get the parent of it",
                new String[]{"node"},
                new Expr[]{null},
                new ParentExpr()
        ));
        this.methods.put("next_sibling", new BuiltInFunctionValue(
                "next_sibling",
                "Given a node, get the next sibling of it",
                new String[]{"node"},
                new Expr[]{null},
                new NextSiblingExpr()
        ));
        this.methods.put("previous_sibling", new BuiltInFunctionValue(
                "previous_sibling",
                "Given a node, get the previous sibling of it",
                new String[]{"node"},
                new Expr[]{null},
                new PreviousSiblingExpr()
        ));
        this.methods.put("dump", new BuiltInFunctionValue(
                "dump",
                "Given an ast node, return a structured dump of the subtree",
                new String[]{"node"},
                new Expr[]{null},
                new DumpExpr()
        ));
        this.methods.put("text", new BuiltInFunctionValue(
                "text",
                "Given an ast node, return its text",
                new String[]{"node"},
                new Expr[]{null},
                new TextExpr()
        ));
        this.methods.put("image", new BuiltInFunctionValue(
                "image",
                "Given an ast node, return its image",
                new String[]{"node"},
                new Expr[]{null},
                new ImageExpr()
        ));
        this.methods.put("unit", new BuiltInFunctionValue(
                "unit",
                "Given an ast node, return its analysis unit",
                new String[]{"node"},
                new Expr[]{null},
                new UnitExpr()
        ));
        this.methods.put("kind", new BuiltInFunctionValue(
                "kind",
                "Return the kind of this node, as a string",
                new String[]{"node"},
                new Expr[]{null},
                new KindExpr()
        ));
    }

    // ----- Override methods -----

    /** @see com.adacore.lkql_jit.runtime.built_ins.methods.BuiltInMethods#getType() */
    @Override
    public String getType() {
        return LKQLTypesHelper.ADA_NODE;
    }

    // ----- Inner classes -----

    /**
     * Expression of the "children" method
     */
    public final static class ChildrenExpr extends BuiltInExpr {
        @Override
        public Object executeGeneric(VirtualFrame frame) {
            // Get the node
            Libadalang.AdaNode node = LKQLTypeSystemGen.asAdaNode(frame.getArguments()[0]);

            // Prepare the result
            int childrenCount = node.getChildrenCount();
            Libadalang.AdaNode[] res = new Libadalang.AdaNode[childrenCount];
            for(int i = 0 ; i < childrenCount ; i++) {
                Libadalang.AdaNode child = node.getChild(i);
                res[i] = (child == null ? NullValue.getInstance() : child);
            }

            // Return the list value
            return new ListValue(res);
        }
    }

    /**
     * Expression of the "parent" method
     */
    public final static class ParentExpr extends BuiltInExpr {
        @Override
        public Object executeGeneric(VirtualFrame frame) {
            Libadalang.AdaNode parent = LKQLTypeSystemGen.asAdaNode(frame.getArguments()[0]).parent();
            return parent == null ? NullValue.getInstance() : parent;
        }
    }

    /**
     * Expression of the "next_sibling" method
     */
    public final static class NextSiblingExpr extends BuiltInExpr {
        @Override
        public Object executeGeneric(VirtualFrame frame) {
            Libadalang.AdaNode next = LKQLTypeSystemGen.asAdaNode(frame.getArguments()[0]).nextSibling();
            return next == null ? NullValue.getInstance() : next;
        }
    }

    /**
     * Expression of the "previous_sibling" method
     */
    public final static class PreviousSiblingExpr extends BuiltInExpr {
        @Override
        public Object executeGeneric(VirtualFrame frame) {
            Libadalang.AdaNode previous = LKQLTypeSystemGen.asAdaNode(frame.getArguments()[0]).previousSibling();
            return previous == null ? NullValue.getInstance() : previous;
        }
    }

    /**
     * Expression of the "children_count" method
     */
    public final static class ChildrenCountExpr extends BuiltInExpr {
        @Override
        public Object executeGeneric(VirtualFrame frame) {
            return (long) LKQLTypeSystemGen.asAdaNode(frame.getArguments()[0]).getChildrenCount();
        }
    }

    /**
     * Expression of the "dump" method
     */
    public final static class DumpExpr extends BuiltInExpr {
        @Override
        public Object executeGeneric(VirtualFrame frame) {
            LKQLLanguage.getContext(this).print(LKQLTypeSystemGen.asAdaNode(frame.getArguments()[0]).dump());
            return UnitValue.getInstance();
        }
    }

    /**
     * Expression of the "text" method
     */
    public final static class TextExpr extends BuiltInExpr {
        @Override
        public Object executeGeneric(VirtualFrame frame) {
            return LKQLTypeSystemGen.asAdaNode(frame.getArguments()[0]).getText();
        }
    }

    /**
     * Expression of the "image" method
     */
    public final static class ImageExpr extends BuiltInExpr {
        @Override
        public Object executeGeneric(VirtualFrame frame) {
            return LKQLTypeSystemGen.asAdaNode(frame.getArguments()[0]).getImage();
        }
    }

    /**
     * Expression of the "unit" method
     */
    public final static class UnitExpr extends BuiltInExpr {
        @Override
        public Object executeGeneric(VirtualFrame frame) {
            return LKQLTypeSystemGen.asAdaNode(frame.getArguments()[0]).unit();
        }
    }

    /**
     * Expression of the "kind" method
     */
    public final static class KindExpr extends BuiltInExpr {
        @Override
        public Object executeGeneric(VirtualFrame frame) {
            return ReflectionUtils.getClassSimpleName(frame.getArguments()[0]);
        }
    }

}

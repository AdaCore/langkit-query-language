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

import com.adacore.lkql_jit.LKQLTypeSystemGen;
import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.adacore.lkql_jit.runtime.built_ins.BuiltInExpr;
import com.adacore.lkql_jit.runtime.built_ins.BuiltInFunctionValue;
import com.adacore.lkql_jit.runtime.built_ins.functions.ReduceFunction;
import com.adacore.lkql_jit.runtime.values.ListValue;
import com.adacore.lkql_jit.runtime.values.interfaces.Iterable;
import com.adacore.lkql_jit.utils.util_classes.Iterator;
import com.oracle.truffle.api.frame.VirtualFrame;

import java.util.LinkedList;
import java.util.List;


/**
 * This class contains all built-in methods for the iterable type in the LKQL language
 *
 * @author Hugo GUERRIER
 */
public abstract class IterableMethods extends CommonMethods {

    /**
     * @see com.adacore.lkql_jit.runtime.built_ins.methods.CommonMethods#initMethods()
     */
    @Override
    protected void initMethods() {
        super.initMethods();

        this.methods.put(ReduceFunction.NAME, ReduceFunction.getInstance().getValue());
        this.methods.put("to_list", new BuiltInFunctionValue(
            "to_list",
            "Transform an iterator into a list",
            new String[]{"iterable"},
            new Expr[]{null},
            new ToListExpr()
        ));
        this.methods.put("length", new BuiltInFunctionValue(
            "length",
            "Get the length of the iterable element",
            new String[]{"iterable"},
            new Expr[]{null},
            new LengthExpr()
        ));
    }

    // ----- Inner classes -----

    /**
     * Expression of the "to_list" method
     */
    public static class ToListExpr extends BuiltInExpr {
        @Override
        public Object executeGeneric(VirtualFrame frame) {
            // Get the iterable receiver
            Iterable receiver = LKQLTypeSystemGen.asIterable(frame.getArguments()[0]);

            // Create a new list from the iterable
            List<Object> resList = new LinkedList<>();
            Iterator iterator = receiver.iterator();
            while (iterator.hasNext()) {
                resList.add(iterator.next());
            }

            // Return the new list value
            return new ListValue(resList.toArray(new Object[0]));
        }
    }

    /**
     * Expression of the "length" method
     */
    public static class LengthExpr extends BuiltInExpr {
        @Override
        public Object executeGeneric(VirtualFrame frame) {
            // Get the iterable receiver
            return LKQLTypeSystemGen.asIterable(frame.getArguments()[0]).size();
        }
    }

}

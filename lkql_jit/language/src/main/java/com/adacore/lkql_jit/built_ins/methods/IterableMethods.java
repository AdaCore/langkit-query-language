//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.built_ins.methods;

import static com.adacore.lkql_jit.built_ins.BuiltInMethodFactory.createAttribute;

import com.adacore.lkql_jit.LKQLTypeSystemGen;
import com.adacore.lkql_jit.built_ins.AbstractBuiltInFunctionBody;
import com.adacore.lkql_jit.built_ins.BuiltInMethodFactory;
import com.adacore.lkql_jit.built_ins.functions.MapFunction;
import com.adacore.lkql_jit.built_ins.functions.ReduceFunction;
import com.adacore.lkql_jit.runtime.values.LKQLTuple;
import com.adacore.lkql_jit.runtime.values.interfaces.Iterable;
import com.adacore.lkql_jit.runtime.values.lists.LKQLList;
import com.adacore.lkql_jit.utils.Iterator;
import com.oracle.truffle.api.frame.VirtualFrame;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

/**
 * This class contains all built-in methods for the iterable type in the LKQL language.
 *
 * @author Hugo GUERRIER
 */
public class IterableMethods {

    public static final Map<String, BuiltInMethodFactory> methods =
            Map.ofEntries(
                    Map.entry(
                            ReduceFunction.NAME,
                            BuiltInMethodFactory.fromFunctionValue(
                                    ReduceFunction.getValue(), false)),
                    Map.entry(
                            MapFunction.NAME,
                            BuiltInMethodFactory.fromFunctionValue(MapFunction.getValue(), false)),
                    createAttribute(
                            "enumerate",
                            "Return the content of the iterable object with each element associated"
                                    + " to its index in a tuple: [(<index>, <elem>), ...]",
                            new EnumerateExpr()),
                    createAttribute(
                            "to_list", "Transform an iterator into a list", new ToListExpr()),
                    createAttribute(
                            "length", "Get the length of the iterable element", new LengthExpr()));

    // ----- Inner classes -----

    /** Expression for the "enumerate" method. */
    public static class EnumerateExpr extends AbstractBuiltInFunctionBody {
        @Override
        public Object executeGeneric(VirtualFrame frame) {
            // Get the iterable receiver
            final var receiver = LKQLTypeSystemGen.asIterable(frame.getArguments()[0]);

            // Create the result array and fill it
            final var resContent = new Object[(int) receiver.size()];
            long index = 1;
            final var iterator = receiver.iterator();
            while (iterator.hasNext()) {
                resContent[(int) index - 1] = new LKQLTuple(new Object[] {index, iterator.next()});
                index++;
            }

            // Return the result
            return new LKQLList(resContent);
        }
    }

    /** Expression of the "to_list" method. */
    public static class ToListExpr extends AbstractBuiltInFunctionBody {
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
            return new LKQLList(resList.toArray(new Object[0]));
        }
    }

    /** Expression of the "length" method. */
    public static class LengthExpr extends AbstractBuiltInFunctionBody {
        @Override
        public Object executeGeneric(VirtualFrame frame) {
            // Get the iterable receiver
            return LKQLTypeSystemGen.asIterable(frame.getArguments()[0]).size();
        }
    }
}

//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.built_ins;

import com.adacore.lkql_jit.LKQLLanguage;
import com.adacore.lkql_jit.LKQLTypeSystemGen;
import com.adacore.lkql_jit.annotations.*;
import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.nodes.utils.ImageNode;
import com.adacore.lkql_jit.runtime.values.*;
import com.adacore.lkql_jit.runtime.values.bases.BasicLKQLValue;
import com.adacore.lkql_jit.runtime.values.interfaces.Indexable;
import com.adacore.lkql_jit.runtime.values.interfaces.Iterable;
import com.adacore.lkql_jit.runtime.values.interfaces.LKQLValue;
import com.adacore.lkql_jit.runtime.values.lists.LKQLList;
import com.adacore.lkql_jit.utils.Constants;
import com.adacore.lkql_jit.utils.Iterator;
import com.adacore.lkql_jit.utils.LKQLTypesHelper;
import com.adacore.lkql_jit.utils.TextWriter;
import com.adacore.lkql_jit.utils.functions.ArrayUtils;
import com.adacore.lkql_jit.utils.functions.StringUtils;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.ArityException;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.interop.UnsupportedTypeException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.strings.TruffleString;
import java.io.StringWriter;
import java.util.Arrays;
import java.util.Comparator;
import java.util.Map;

/**
 * Module where most of the LKQL built-in functions are declared. General purpose built-in functions
 * are declared here.
 */
public class BuiltInFunctions {

    @BuiltInFunction(
        name = "unique",
        doc = "Given a collection, create a list with all duplicates removed"
    )
    @BuiltInMethod(targetTypes = LKQLTypesHelper.LKQL_LIST, isProperty = true)
    abstract static class UniqueExpr extends BuiltInBody {

        @Specialization
        protected LKQLList onIndexable(Indexable indexable) {
            return new LKQLList(ArrayUtils.unique(indexable.getContent()).toArray(new Object[0]));
        }
    }

    @BuiltInFunction(
        name = "pattern",
        doc = "Given a regex pattern string, create a pattern object"
    )
    abstract static class PatternExpr extends BuiltInBody {

        @Specialization
        protected LKQLPattern onValidArgs(
            TruffleString regex,
            @DefaultVal("true") boolean caseSensitive,
            @Cached TruffleString.ToJavaStringNode toJavaStringNode
        ) {
            return new LKQLPattern(getCallNode(), toJavaStringNode.execute(regex), caseSensitive);
        }
    }

    /** Expression of the "print" function. */
    @BuiltInFunction(name = "print", doc = "Built-in print function. Prints the argument")
    @BuiltInMethod(isProperty = true)
    abstract static class PrintExpr extends BuiltInBody {

        @Specialization(limit = Constants.SPECIALIZED_LIB_LIMIT)
        protected LKQLUnit onBoolean(
            Object toPrint,
            @DefaultVal("true") boolean newLine,
            @CachedLibrary("toPrint") InteropLibrary printingLibrary
        ) {
            final var s = printingLibrary.toDisplayString(toPrint);
            if (newLine) {
                LKQLLanguage.getContext(null).println(s);
            } else {
                LKQLLanguage.getContext(null).print(s);
            }
            return LKQLUnit.INSTANCE;
        }
    }

    @BuiltInFunction(name = "img", doc = "Return a string representation of an object")
    @BuiltInMethod(isProperty = true)
    abstract static class ImgExpr extends BuiltInBody {

        @Specialization
        protected TruffleString onAny(Object o, @Cached ImageNode imageNode) {
            return imageNode.execute(o);
        }
    }

    @BuiltInFunction(
        name = "doc",
        doc = "Given any object, return the documentation associated with it"
    )
    @BuiltInMethod(isProperty = true)
    abstract static class DocExpr extends BuiltInBody {

        @Specialization
        protected TruffleString onLKQLValue(
            LKQLValue value,
            @Cached TruffleString.FromJavaStringNode fromJavaStringNode
        ) {
            return fromJavaStringNode.execute(value.lkqlDocumentation(), Constants.STRING_ENCODING);
        }

        @Specialization
        protected TruffleString onAny(Object obj) {
            return Constants.STRING_ENCODING.getEmpty();
        }
    }

    @BuiltInFunction(
        name = "reduce",
        doc = "Given a collection, a reduction function, and an initial value reduce the" +
        " result"
    )
    @BuiltInMethod(
        targetTypes = {
            LKQLTypesHelper.LKQL_SELECTOR_LIST,
            LKQLTypesHelper.LKQL_LAZY_LIST,
            LKQLTypesHelper.LKQL_LIST,
        }
    )
    abstract static class ReduceExpr extends BuiltInBody {

        @Specialization(
            limit = Constants.SPECIALIZED_LIB_LIMIT,
            guards = "function.parameterNames.length == 2"
        )
        protected Object onValidArgs(
            Iterable iterable,
            LKQLFunction function,
            Object initValue,
            @CachedLibrary("function") InteropLibrary functionLibrary
        ) {
            Iterator iterator = iterable.iterator();
            while (iterator.hasNext()) {
                try {
                    initValue = functionLibrary.execute(
                        function,
                        function.closure.getContent(),
                        initValue,
                        iterator.next()
                    );
                } catch (
                    ArityException | UnsupportedTypeException | UnsupportedMessageException e
                ) {
                    // TODO: Implement runtime checks in the LKQLFunction class and base computing
                    // on them (#138)
                    throw LKQLRuntimeException.fromJavaException(e, argNode(1));
                }
            }
            return initValue;
        }
    }

    @BuiltInFunction(
        name = "document_builtins",
        doc = "Return a string in the RsT format containing documentation for all built-ins"
    )
    abstract static class DocumentBuiltinsExpr extends BuiltInBody {

        @CompilerDirectives.TruffleBoundary
        @Specialization
        public static TruffleString exec(
            @Cached TruffleString.FromJavaStringNode fromJavaStringNode
        ) {
            var sw = new StringWriter();
            try (TextWriter writer = new TextWriter(sw)) {
                writer.write("Standard library\n");
                writer.write("----------------\n");
                writer.write("\n");
                writer.write("Builtin functions\n");
                writer.write("^^^^^^^^^^^^^^^^^\n");
                writer.write("\n");

                for (var func : AllBuiltIns.allFunctions()) {
                    writer.write(".. function:: ");
                    writer.write(func.getName());
                    writer.write("(" + String.join(", ", func.parameterNames) + ")");
                    writer.write("\n\n");
                    writer.withIndent(() -> {
                        writer.write(func.documentation);
                    });
                    writer.write("\n");
                    writer.write("\n");
                }

                writer.write("Builtin methods\n");
                writer.write("^^^^^^^^^^^^^^^\n");
                writer.write("\n");

                var sortedBuiltinMethods = new java.util.ArrayList<>(
                    AllBuiltIns.allMethods()
                        .entrySet()
                        .stream()
                        .sorted(Map.Entry.comparingByKey())
                        .toList()
                );

                for (var entry : sortedBuiltinMethods) {
                    var methods = entry
                        .getValue()
                        .entrySet()
                        .stream()
                        .sorted(Map.Entry.comparingByKey())
                        .toList();

                    // Skip type if there are no methods to document
                    if (methods.size() == 0) {
                        continue;
                    }

                    var typeName = entry.getKey();
                    var header = "Methods for `" + typeName + "`";
                    writer.write(header + "\n");
                    writer.write("\"".repeat(header.length()) + "\n");

                    for (var method : methods) {
                        writer.write(".. method:: ");
                        writer.write(typeName + "." + method.getKey());
                        writer.write(
                            "(" +
                            String.join(
                                ", ",
                                Arrays.stream(method.getValue().paramNames).toArray(String[]::new)
                            ) +
                            ")"
                        );
                        writer.write("\n\n");
                        writer.withIndent(() -> {
                            writer.write(method.getValue().documentation);
                        });
                        writer.write("\n");
                        writer.write("\n");
                    }
                }

                return fromJavaStringNode.execute(
                    sw.getBuffer().toString(),
                    Constants.STRING_ENCODING
                );
            } catch (Exception e) {
                throw new RuntimeException(e);
            }
        }
    }

    @BuiltInFunction(
        name = "base_name",
        doc = "Given a string that represents a file name, returns the basename"
    )
    @BuiltInMethod(targetTypes = { LKQLTypesHelper.LKQL_STRING }, isProperty = true)
    abstract static class BaseNameExpr extends BuiltInBody {

        @Specialization
        protected TruffleString executeOnString(
            TruffleString fileName,
            @Cached TruffleString.CodePointLengthNode codePointLengthNode,
            @Cached TruffleString.LastIndexOfStringNode lastIndexOfStringNode,
            @Cached TruffleString.SubstringNode substringNode
        ) {
            final var fileNameLen = codePointLengthNode.execute(
                fileName,
                Constants.STRING_ENCODING
            );
            final var lastSep = lastIndexOfStringNode.execute(
                fileName,
                Constants.PATH_SEP,
                0,
                fileNameLen,
                Constants.STRING_ENCODING
            );
            return substringNode.execute(
                fileName,
                lastSep + 1,
                fileNameLen - lastSep,
                Constants.STRING_ENCODING,
                false
            );
        }
    }

    @BuiltInFunction(
        name = "concat",
        doc = "Given a list of lists or strings, return a concatenated list or string"
    )
    abstract static class ConcatExpr extends BuiltInBody {

        protected static boolean isString(Object o) {
            return LKQLTypeSystemGen.isTruffleString(o);
        }

        protected static boolean isList(Object o) {
            return LKQLTypeSystemGen.isLKQLList(o);
        }

        @Specialization(guards = { "list.size() > 0", "isString(list.get(0))" })
        protected TruffleString onListOfStrings(
            LKQLList list,
            @Cached TruffleString.ConcatNode concatNode
        ) {
            // Create a string builder and add all strings in the list
            TruffleString result = LKQLTypeSystemGen.asTruffleString(list.get(0));
            for (int i = 1; i < list.size(); i++) {
                final Object item = list.get(i);
                if (!isString(item)) {
                    this.invalidElemType(list, item);
                }
                result = concatNode.execute(
                    result,
                    LKQLTypeSystemGen.asTruffleString(item),
                    Constants.STRING_ENCODING,
                    true
                );
            }
            return result;
        }

        @Specialization(guards = { "list.size() > 0", "isList(list.get(0))" })
        protected LKQLList onListOfLists(LKQLList list) {
            Object[] result = LKQLTypeSystemGen.asLKQLList(list.get(0)).getContent();
            for (int i = 1; i < list.size(); i++) {
                final Object item = list.get(i);
                if (!LKQLTypeSystemGen.isLKQLList(item)) {
                    this.invalidElemType(list, item);
                }
                result = ArrayUtils.concat(result, LKQLTypeSystemGen.asLKQLList(item).getContent());
            }
            return new LKQLList(result);
        }

        @Specialization(guards = "notValidElem.size() > 0")
        @CompilerDirectives.TruffleBoundary
        protected LKQLList invalidElemType(
            @SuppressWarnings("unused") LKQLList notValidElem,
            @Cached("notValidElem.get(0)") Object elem
        ) {
            throw LKQLRuntimeException.wrongType(
                LKQLTypesHelper.LKQL_LIST +
                " of " +
                LKQLTypesHelper.typeUnion(LKQLTypesHelper.LKQL_LIST, LKQLTypesHelper.LKQL_STRING),
                LKQLTypesHelper.fromJava(elem) + " element",
                argNode(0)
            );
        }

        @Specialization(guards = "emptyList.size() == 0")
        protected LKQLList onEmptyList(@SuppressWarnings("unused") LKQLList emptyList) {
            return new LKQLList(new Object[0]);
        }
    }

    @BuiltInFunction(name = "map", doc = "Given a collection, a mapping function")
    abstract static class MapExpr extends BuiltInBody {

        @Specialization(
            limit = Constants.SPECIALIZED_LIB_LIMIT,
            guards = "function.parameterNames.length == 1"
        )
        protected LKQLList onValidArgs(
            Iterable iterable,
            LKQLFunction function,
            @CachedLibrary("function") InteropLibrary functionLibrary
        ) {
            Object[] res = new Object[(int) iterable.size()];
            int i = 0;
            Iterator iterator = iterable.iterator();

            while (iterator.hasNext()) {
                try {
                    res[i] = functionLibrary.execute(
                        function,
                        function.closure.getContent(),
                        iterator.next()
                    );
                } catch (
                    ArityException | UnsupportedTypeException | UnsupportedMessageException e
                ) {
                    // TODO: Implement runtime checks in the LKQLFunction class and base computing
                    // on them (#138)
                    throw LKQLRuntimeException.fromJavaException(e, argNode(1));
                }
                i++;
            }

            return new LKQLList(res);
        }
    }

    @BuiltInFunction(
        name = "profile",
        doc = "Given any object, if it is a callable, return its profile as text"
    )
    abstract static class ProfileExpr extends BuiltInBody {

        @Specialization
        protected TruffleString onLKQLValue(
            LKQLValue val,
            @Cached TruffleString.FromJavaStringNode fromJavaStringNode
        ) {
            return fromJavaStringNode.execute(val.lkqlProfile(), Constants.STRING_ENCODING);
        }

        @Specialization
        protected TruffleString onOthers(Object obj) {
            return Constants.STRING_ENCODING.getEmpty();
        }
    }

    @BuiltInFunction(
        name = "document_namespace",
        doc = "Return a string in the RsT format containing documentation for all built-ins"
    )
    abstract static class DocumentNamespaceExpr extends BuiltInBody {

        private static void documentCallable(TextWriter writer, BasicLKQLValue callable) {
            writer.write(".. function:: " + callable.lkqlProfile() + "\n\n");
            writer.withIndent(() -> {
                writer.write(callable.lkqlDocumentation());
            });
            writer.write("\n\n");
        }

        @Specialization
        @CompilerDirectives.TruffleBoundary
        protected TruffleString impl(
            LKQLNamespace namespace,
            TruffleString name,
            @Cached TruffleString.FromJavaStringNode fromJavaStringNode
        ) {
            var sw = new StringWriter();
            try (TextWriter writer = new TextWriter(sw)) {
                var header = name + "'s API doc";
                writer.write(header + "\n");
                writer.write("-".repeat(header.length()));
                writer.write("\n\n");

                writer.write("Functions\n");
                writer.write("^^^^^^^^^\n");

                var functions = namespace
                    .asMap()
                    .values()
                    .stream()
                    .filter(LKQLTypeSystemGen::isLKQLFunction)
                    .map(LKQLTypeSystemGen::asLKQLFunction)
                    .sorted(Comparator.comparing(LKQLFunction::getName));

                for (var func : functions.toList()) {
                    documentCallable(writer, func);
                }

                writer.write("Selectors\n");
                writer.write("^^^^^^^^^\n");

                var selectors = namespace
                    .asMap()
                    .values()
                    .stream()
                    .filter(LKQLTypeSystemGen::isLKQLSelector)
                    .map(LKQLTypeSystemGen::asLKQLSelector)
                    .sorted(Comparator.comparing(LKQLSelector::lkqlProfile));

                for (var sel : selectors.toList()) {
                    documentCallable(writer, sel);
                }

                return fromJavaStringNode.execute(
                    sw.getBuffer().toString(),
                    Constants.STRING_ENCODING
                );
            } catch (Exception e) {
                throw new RuntimeException(e);
            }
        }
    }

    @BuiltInFunction(name = "help", doc = "Print formatted help for the given object")
    @BuiltInMethod(isProperty = true)
    abstract static class HelpExpr extends BuiltInBody {

        @Specialization
        protected Object onLKQLValue(LKQLValue value) {
            LKQLLanguage.getContext(callNode).println(
                StringUtils.concat(value.lkqlProfile(), "\n", value.lkqlDocumentation())
            );
            return LKQLUnit.INSTANCE;
        }
    }

    @BuiltInFunction(name = "units", doc = "Return a list of all units")
    abstract static class UnitsExpr extends BuiltInBody {

        @Specialization
        protected LKQLList alwaysTrue() {
            return new LKQLList(LKQLLanguage.getContext(callNode).getAllUnits());
        }
    }

    @BuiltInFunction(name = "specified_units", doc = "Return a list of units specified by the user")
    abstract static class SpecifiedUnitsExpr extends BuiltInBody {

        @Specialization
        protected LKQLList alwaysTrue() {
            return new LKQLList(LKQLLanguage.getContext(callNode).getSpecifiedUnits());
        }
    }
}

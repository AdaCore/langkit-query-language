//
//  Copyright (C) 2005-2026, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit;

import com.adacore.langkit_support.LangkitSupport;
import com.adacore.liblktlang.Liblktlang;
import java.io.IOException;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.math.BigInteger;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.nio.file.StandardOpenOption;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

/**
 * This class is the main entry point of the LKQL stubs generator. Its role is to take a Lkt spec
 * as input and generate a stub file from it.
 * The generated file contains information required to perform type checking on LKQL scripts that
 * use the Langkit spec generated from the input Lkt spec.
 */
public class StubsGenerator {

    public static void main(String[] args) {
        // Get script arguments
        var outputFile = Paths.get(args[0]);

        // Load the required analysis library
        LangkitSupport.Reflection.Library analysisLib;
        try {
            var analysisLibClass = Class.forName(args[1]);
            var descriptionGetter = analysisLibClass.getMethod("getDescription");
            analysisLib = (LangkitSupport.Reflection.Library) descriptionGetter.invoke(null);
        } catch (ReflectiveOperationException e) {
            throw new RuntimeException(e);
        }

        // Create the string builder used to accumulate the result
        var stubContent = new StringBuilder("# lkql version: 2\n\n");

        // Generate enumeration types
        stubContent.append("# ===== Enumeration types =====\n\n");
        for (var enumEntry : analysisLib
            .enumMap()
            .entrySet()
            .stream()
            .sorted(Map.Entry.comparingByKey())
            .toList()) {
            emitEnum(enumEntry.getValue(), stubContent);
        }

        // Generate struct types
        stubContent.append("# ===== Structure types =====\n\n");
        for (var structEntry : analysisLib
            .structMap()
            .entrySet()
            .stream()
            .filter(e -> e.getValue().isPublic())
            .sorted(Map.Entry.comparingByKey())
            .toList()) {
            emitStruct(structEntry.getValue(), stubContent);
        }

        // Generate class for nodes
        stubContent.append("# ===== Node stubs =====\n\n");
        for (var nodeEntry : analysisLib
            .nodeMap()
            .entrySet()
            .stream()
            .sorted(Map.Entry.comparingByKey())
            .toList()) {
            emitNode(nodeEntry.getValue(), stubContent);
        }

        // Get the output file, create it if required and write the result in it
        try {
            Files.deleteIfExists(outputFile);
            Files.createFile(outputFile);
            Files.writeString(outputFile, stubContent, StandardOpenOption.WRITE);
        } catch (IOException e) {
            throw new RuntimeException(e);
        }

        // Finally, try to parse and type check the emitted file to make sure it is valid
        try (var ctx = Liblktlang.AnalysisContext.create()) {
            var unit = ctx.getUnitFromFile(outputFile.toString());

            // Check parsing diagnostics
            var parsingDiags = unit.getDiagnostics();
            if (parsingDiags.length > 0) {
                throw new RuntimeException(
                    "Diagnostics when parsing the \"" +
                        outputFile +
                        "\" file:\n" +
                        String.join(
                            "\n",
                            Arrays.stream(parsingDiags)
                                .map(
                                    d ->
                                        "  " + makeDiag(d.message.toString(), d.sourceLocationRange)
                                )
                                .toList()
                        )
                );
            }

            // Then check for typing diagnostics
            var typingDiags = unit
                .getRoot()
                .walk()
                .filter(Liblktlang.LktNode::pXrefEntryPoint)
                .flatMap(n -> Arrays.stream(n.pNameresDiagnostics()))
                .map(
                    d ->
                        "  " +
                        makeDiag(
                            LangkitSupport.renderSolverDiag(d),
                            d.location.getSourceLocationRange()
                        )
                )
                .toList();
            if (!typingDiags.isEmpty()) {
                throw new RuntimeException(
                    "Diagnostic when typing the \"" +
                        outputFile +
                        "\" file:\n" +
                        String.join("\n", typingDiags)
                );
            }
        }
    }

    /**
     * Util function to output the Lkt definition of the provided enumeration type in the specified
     * buffer.
     */
    private static void emitEnum(LangkitSupport.Reflection.Enum enumType, StringBuilder output) {
        output.append("enum ").append(enumType.clazz().getSimpleName()).append(" {\n    case ");
        for (var val : enumType.values()) {
            output.append(val.name()).append(", ");
        }
        output.append("\n}\n\n");
    }

    /**
     * Util function to output the Lkt definition of the provided struct type in the specified
     * buffer.
     */
    private static void emitStruct(
        LangkitSupport.Reflection.Struct structType,
        StringBuilder output
    ) {
        output.append("struct ").append(toLktType(structType.clazz())).append(" {\n");
        for (var field : structType.fields()) {
            output.append("    ").append(field.name()).append(": ").append(toLktType(field.type()));
            field.defaultValue().ifPresent(v -> output.append(" = ").append(toLktLiteral(v)));
            output.append('\n');
        }
        output.append("}\n\n");
    }

    /** Util function to emit the Lkt stubs for the provided node type in the output buffer. */
    private static void emitNode(LangkitSupport.Reflection.Node nodeType, StringBuilder output) {
        // Start by emitting the class declaration
        output.append("class ").append(nodeType.className()).append(": ");

        // Now handle the inheritance part
        var superNodeType = nodeType.clazz().getSuperclass();
        if (superNodeType == Object.class) {
            output.append("Node ");
        } else {
            output.append(toLktType(superNodeType)).append(' ');
        }
        output.append("{\n");

        // Compute the list of fields to represent
        var fields = nodeType
            .fieldDescriptions()
            .entrySet()
            .stream()
            .filter(e -> e.getKey().startsWith("f_") || e.getKey().startsWith("p_"))
            .filter(e -> e.getValue().javaMethod().getDeclaringClass() == nodeType.clazz())
            .sorted(Map.Entry.comparingByKey())
            .toList();

        // Emit all fields of the node type
        for (var f : fields) {
            var fieldName = f.getKey();
            var field = f.getValue();
            output.append("    ");

            if (fieldName.startsWith("f_")) {
                // Emit fields for parsing fields
                output.append("@parse_field ");
                if (field.isNullable()) output.append("@nullable ");
                output.append(fieldName);
            } else {
                // Declare the property with its parameters
                output.append("@external() fun ").append(fieldName).append('(');
                for (int i = 0; i < field.params().size(); i++) {
                    var param = field.params().get(i);
                    output.append(param.name()).append(": ").append(toLktType(param.type()));
                    param
                        .defaultValue()
                        .ifPresent(v -> output.append(" = ").append(toLktLiteral(v)));
                    if (i < field.params().size() - 1) {
                        output.append(", ");
                    }
                }
                output.append(')');
            }

            // Then add the field type or the function return type
            output.append(": ").append(toLktType(field.javaMethod().getReturnType())).append('\n');
        }

        // Finally close the node class
        output.append("}\n\n");
    }

    /**
     * Get the Lkt type corresponding to the provided Java class object. This function may raise an
     * exception if the provided Java class cannot be represented in Lkt.
     */
    private static String toLktType(Class<?> clazz) {
        return switch (clazz) {
            case Object _ when clazz == boolean.class || clazz == Boolean.class -> "Bool";
            case Object _ when clazz == int.class || clazz == Integer.class -> "Int";
            case Object _ when clazz == BigInteger.class -> "BigInt";
            case Object _ when (clazz == String.class) -> "String";
            case Object _ when (
                LangkitSupport.CharInterface.class.isAssignableFrom(clazz) ||
                LangkitSupport.SymbolInterface.class.isAssignableFrom(clazz) ||
                LangkitSupport.StructInterface.class.isAssignableFrom(clazz) ||
                LangkitSupport.EnumInterface.class.isAssignableFrom(clazz) ||
                LangkitSupport.AnalysisUnit.class.isAssignableFrom(clazz) ||
                LangkitSupport.TokenInterface.class.isAssignableFrom(clazz) ||
                LangkitSupport.NodeInterface.class.isAssignableFrom(clazz)
            ) -> clazz.getSimpleName();
            case Object _ when (
                LangkitSupport.LangkitIterator.class.isAssignableFrom(clazz)
            ) -> "Iterator[" +
            toLktType(genericArgs(clazz.getGenericInterfaces()[0]).getFirst()) +
            ']';
            case Object _ when clazz.isArray() -> "Array[" + toLktType(clazz.componentType()) + ']';
            default -> throw new RuntimeException("Cannot map " + clazz + " to a Lkt type");
        };
    }

    /**
     * Turn the provided Java value to a Lkt literal, raising an exception if this is not possible.
     */
    private static String toLktLiteral(Object o) {
        return switch (o) {
            case Boolean b -> b.toString();
            case Integer i -> i.toString();
            case String s -> '"' + s + '"';
            case LangkitSupport.EnumInterface e -> toLktType(e.getClass()) +
            "." +
            e.toString().toLowerCase();
            case LangkitSupport.NodeInterface n when n.isNone() -> "null";
            default -> throw new RuntimeException(
                "Cannot get an Lkt literal from " + o + " (" + o.getClass() + ')'
            );
        };
    }

    /** Internal util to get class of actual generic arguments parametrizing the provided type. */
    private static List<Class<?>> genericArgs(Type type) {
        var res = new ArrayList<Class<?>>();
        for (var typeArg : ((ParameterizedType) type).getActualTypeArguments()) {
            res.add((Class<?>) typeArg);
        }
        return res;
    }

    /** Util function to create a textual diagnostic from information. */
    private static String makeDiag(String text, LangkitSupport.SourceLocationRange slocRange) {
        return slocRange.start.line + ":" + slocRange.start.column + ": " + text;
    }
}

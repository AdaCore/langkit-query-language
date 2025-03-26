//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.annotations;

import java.io.IOException;
import java.io.PrintStream;
import java.nio.charset.StandardCharsets;
import java.util.*;
import java.util.stream.Collectors;
import javax.annotation.processing.*;
import javax.lang.model.SourceVersion;
import javax.lang.model.element.*;
import javax.lang.model.type.TypeMirror;
import org.graalvm.collections.Pair;

/**
 * This built-in processor will process every BuiltIn annotation, and create built-in objects, as
 * well as a global factory, that will be used to add built-ins to the LKQL language.
 *
 * <p>This makes it very simple to add built-ins to the language (see usages of
 * BuiltInFunction/BuiltInMethod for examples), and works with native image since it doesn't use
 * reflection.
 *
 * <p>The signature of the built-in function is infered from the parameters of the first
 * specialization of the built-in.
 */
@SupportedAnnotationTypes(
    {
        "com.adacore.lkql_jit.annotations.BuiltInFunction",
        "com.adacore.lkql_jit.annotations.BuiltInMethod",
    }
)
@SupportedSourceVersion(SourceVersion.RELEASE_17)
public class BuiltInProcessor extends AbstractProcessor {

    /** Suffix for generated built-in classes. */
    final String SUFFIX = "BuiltIns";

    /**
     * Set of already processed classes containing built-in declarations. Used as a guard in
     * processBuiltInFactory
     */
    final Set<TypeElement> processedFactories = new HashSet<>();

    /**
     * Set of generated built-in packages. Each element is a pair <packageName, className>. Each
     * package contains a namespace class which contains two static methods.
     */
    final HashSet<Pair<String, String>> builtInPackages = new HashSet<>();

    /** Whether built-ins have been processed already. Global guard. */
    boolean processed = false;

    @Override
    public boolean process(Set<? extends TypeElement> annotations, RoundEnvironment roundEnv) {
        if (processed) {
            return true;
        }

        processed = true;

        for (var annotation : annotations) {
            // Grab all elements annotated with BuiltInFunction/Method
            Set<? extends Element> annotatedElements = roundEnv.getElementsAnnotatedWith(
                annotation
            );

            for (var element : annotatedElements) {
                // Grab the enclosing class, which is the factory class. Note that since a lot of
                // those classes contain several built-ins, this means that we will call
                // processBuiltInFactory several time per factory
                var factoryBase = element.getEnclosingElement();
                try {
                    processBuiltInFactory((TypeElement) factoryBase);
                } catch (IOException e) {
                    throw new RuntimeException(e);
                }
            }
        }

        try (
            final var stream = new PrintStream(
                processingEnv
                    .getFiler()
                    .createSourceFile("com.adacore.lkql_jit.built_ins.AllBuiltIns")
                    .openOutputStream(),
                true,
                StandardCharsets.UTF_8
            )
        ) {
            // Package
            stream.println("package com.adacore.lkql_jit.built_ins;");

            // Imports
            stream.println("import java.util.stream.Stream;");
            stream.println("import java.util.stream.Collectors;");
            stream.println("import java.util.Arrays;");
            stream.println("import java.util.Map;");
            stream.println("import java.util.List;");
            stream.println("import java.util.HashMap;");
            stream.println("import com.adacore.lkql_jit.utils.LKQLTypesHelper;");
            stream.println("import com.adacore.lkql_jit.built_ins.BuiltInFunctionValue;");
            stream.println("import com.adacore.lkql_jit.built_ins.BuiltInMethodFactory;");

            for (var p : builtInPackages) {
                stream.println("import " + p.getLeft() + "." + p.getRight() + ";");
            }
            stream.println("public class AllBuiltIns {");

            stream.println(
                "    static private List<BuiltInFunctionValue> allFunctionsCache = null;"
            );
            stream.println(
                "    static private Map<String, Map<String, BuiltInMethodFactory>>" +
                " allMethodsCache = null;"
            );

            // allFunctions method
            stream.println("    public static List<BuiltInFunctionValue> allFunctions() {");
            stream.println("        if (allFunctionsCache != null) {");
            stream.println("            return allFunctionsCache;");
            stream.println("        }");
            stream.println("        allFunctionsCache = Stream.of(");
            stream.println(
                builtInPackages
                    .stream()
                    .map(p -> "            Arrays.stream(" + p.getRight() + ".getFunctions())")
                    .collect(Collectors.joining(",\n")) +
                ")"
            );
            stream.println("           .reduce(Stream::concat).orElseGet(Stream::empty)");
            stream.println("           .collect(Collectors.toList());");
            stream.println("        return allFunctionsCache;");
            stream.println("    }");

            // allMethods method
            stream.println(
                "    public static Map<String, Map<String, BuiltInMethodFactory>>" +
                " allMethods() {"
            );
            stream.println("        if (allMethodsCache != null) {");
            stream.println("            return allMethodsCache;");
            stream.println("        }");
            stream.println(
                "        allMethodsCache = new HashMap<String, Map<String," +
                " BuiltInMethodFactory>>();"
            );
            for (var p : builtInPackages) {
                stream.println("        for (var pair : " + p.getRight() + ".getMethods()) {");
                stream.println(
                    "       var keys = pair.getLeft().length == 0 ?" +
                    " LKQLTypesHelper.ALL_TYPES : pair.getLeft();"
                );
                stream.println("            for (var key : keys) {");
                stream.println(
                    "            var typeMap = allMethodsCache.computeIfAbsent(key, t ->" +
                    " new HashMap<>());"
                );
                stream.println(
                    "                 typeMap.put(pair.getRight().name, pair.getRight());"
                );
                stream.println("            }");
                stream.println("        }");
            }
            stream.println("        return allMethodsCache;");
            stream.println("    }");

            stream.println("}");
        } catch (IOException e) {
            throw new RuntimeException(e);
        }

        return true;
    }

    private void log(String toLog) {
        System.out.println(toLog);
    }

    private TypeMirror virtualFrameType() {
        return processingEnv
            .getElementUtils()
            .getTypeElement("com.oracle.truffle.api.frame.VirtualFrame")
            .asType();
    }

    /**
     * Return the code string to build the built-in arguments for given built-in. The string
     * contains two function parameters, the names and the default values, joined by a ", ".
     */
    private String builtInArguments(TypeElement builtIn, boolean omitSelfParam) {
        var names = new ArrayList<String>();
        var defaultVals = new ArrayList<String>();

        for (var el : builtIn.getEnclosedElements()) {
            if (!(el instanceof ExecutableElement method)) {
                continue;
            }

            if (
                (method
                        .getAnnotationMirrors()
                        .stream()
                        .noneMatch(e ->
                            e
                                .getAnnotationType()
                                .asElement()
                                .getSimpleName()
                                .toString()
                                .contains("Specialization")
                        ))
            ) {
                continue;
            }

            var params = method
                .getParameters()
                .stream()
                .filter(p -> {
                    return (
                        !processingEnv.getTypeUtils().isSameType(p.asType(), virtualFrameType()) &&
                        // If there is an annotation, it can only be the
                        // "DefaultVal" annotation. Skip other parameters
                        // with annotations.
                        p
                            .getAnnotationMirrors()
                            .stream()
                            .allMatch(a ->
                                Objects.equals(
                                    a.getAnnotationType().asElement().getSimpleName().toString(),
                                    "DefaultVal"
                                )
                            )
                    );
                })
                .skip(omitSelfParam ? 1 : 0)
                .toList();

            for (var p : params) {
                var defaultValAnnot = p.getAnnotation(DefaultVal.class);
                var defaultVal = (defaultValAnnot != null ? defaultValAnnot.value() : null);
                var name = p.getSimpleName().toString();
                var nameSB = new StringBuilder();

                // Adapt parameter names from Java style (aParamName) to LKQL style (a_param_name)
                name
                    .chars()
                    .mapToObj(c -> Character.valueOf((char) c))
                    .forEach(c -> {
                        if (Character.isUpperCase(c)) {
                            nameSB.append("_").append(Character.toLowerCase(c));
                        } else {
                            nameSB.append(c);
                        }
                    });

                names.add("\"" + escapeString(nameSB.toString()) + "\"");
                defaultVals.add(
                    (defaultVal != null ? "\"" + escapeString(defaultVal) + "\"" : "null")
                );
            }

            return (
                "new String[] {" +
                String.join(", ", names) +
                "}, new String[] {" +
                String.join(", ", defaultVals) +
                "}"
            );
        }

        throw new RuntimeException("Couldn't compute profile for " + builtIn.toString());
    }

    /**
     * Process one built-in factory class. This will emit one "<factory name>BuiltIns.java" per
     * factory. This file contains a class that always has the same structure:
     */
    private void processBuiltInFactory(TypeElement builtInFactoryClass) throws IOException {
        // This function is called once per built-in contained by the factory, so potentially
        // several times per built-in. For this reason we keep track of already processed factories
        // and bail out if it has already been processed.
        if (processedFactories.contains(builtInFactoryClass)) {
            return;
        }
        processedFactories.add(builtInFactoryClass);

        // Create output source file
        final var output = processingEnv
            .getFiler()
            .createSourceFile(builtInFactoryClass.getQualifiedName() + SUFFIX, builtInFactoryClass);

        final var packageElement = (PackageElement) builtInFactoryClass.getEnclosingElement();
        final var packageName = packageElement.getQualifiedName().toString();
        final var className = builtInFactoryClass.getSimpleName() + SUFFIX;

        // Collect built-in functions and methods contained in the factory.
        var builtInFunctions = builtInFactoryClass
            .getEnclosedElements()
            .stream()
            .filter(e -> e.getAnnotation(BuiltInFunction.class) != null)
            .map(e -> (TypeElement) e)
            .toList();

        var builtInMethods = builtInFactoryClass
            .getEnclosedElements()
            .stream()
            .filter(e -> e.getAnnotation(BuiltInMethod.class) != null)
            .map(e -> (TypeElement) e)
            .toList();

        // Emit the output source file. The file will contain one package class with two static
        // functions, one which returns the built-in functions, one which returns the built-in
        // methods.
        try (
            final var stream = new PrintStream(
                output.openOutputStream(),
                true,
                StandardCharsets.UTF_8
            )
        ) {
            stream.println("package " + packageName + ";");
            stream.println();

            // Emit imports
            stream.println("import java.util.List;");
            stream.println("import com.adacore.lkql_jit.built_ins.BuiltInFunctionValue;");
            stream.println("import com.adacore.lkql_jit.built_ins.BuiltInBody;");
            stream.println("import com.adacore.lkql_jit.built_ins.BuiltInMethodFactory;");
            stream.println("import org.graalvm.collections.Pair;");
            stream.println("public class " + className + " { ");

            // Built-in functions computation
            stream.println("    public static BuiltInFunctionValue[] getFunctions() {");
            stream.println("        return new BuiltInFunctionValue[] {");
            stream.println(
                builtInFunctions
                    .stream()
                    .map(builtInFn -> {
                        var fnAnnot = builtInFn.getAnnotation(BuiltInFunction.class);

                        return (
                            "          new BuiltInFunctionValue(\"" +
                            escapeString(fnAnnot.name()) +
                            "\", \"" +
                            escapeString(fnAnnot.doc()) +
                            "\", " +
                            builtInArguments(builtInFn, false) +
                            ", BuiltInBody.create(" +
                            builtInFactoryClass.getSimpleName().toString() +
                            "Factory." +
                            builtInFn.getSimpleName().toString() +
                            "Factory.getInstance()" +
                            "))"
                        );
                    })
                    .collect(Collectors.joining(",\n"))
            );
            stream.println("        };");
            stream.println("   }");

            // Built-in methods computation
            stream.println(
                "    public static List<Pair<String[], BuiltInMethodFactory>> getMethods() {"
            );
            stream.println("        return List.of(");
            stream.println(
                builtInMethods
                    .stream()
                    .map(builtInMethod -> {
                        var methodAnnotation = builtInMethod.getAnnotation(BuiltInMethod.class);
                        var fnAnnotation = builtInMethod.getAnnotation(BuiltInFunction.class);

                        var name = methodAnnotation.name();
                        var doc = methodAnnotation.doc();
                        var isProperty = methodAnnotation.isProperty();

                        if (fnAnnotation != null) {
                            if (name.equals("")) {
                                // If the method annotation has no name and there is
                                // a function annotation, take its name
                                name = fnAnnotation.name();
                            }

                            if (doc.equals("")) {
                                // If the method annotation has no doc and there is
                                // a function annotation, take its name
                                doc = fnAnnotation.doc();
                            }
                        }

                        var targetTypes = methodAnnotation.targetTypes();

                        // If there are no explicit targetTypes on the method
                        // annotation, try to get them from the
                        // @BuiltinMethodContainer annotation on the
                        // enclosing class.
                        if (targetTypes.length == 0) {
                            var containerAnnot = builtInMethod
                                .getEnclosingElement()
                                .getAnnotation(BuiltinMethodContainer.class);

                            if (containerAnnot != null) {
                                targetTypes = containerAnnot.targetTypes();
                            }
                        }

                        // If we still don't have explicit target types, then it
                        // means that the method is a method on all types.
                        if (targetTypes.length == 0) {
                            targetTypes = new String[] {};
                        }

                        var wrapper =
                            "BuiltInMethodFactory." +
                            (isProperty ? "createAttribute" : "createMethod") +
                            "(\"" +
                            escapeString(name) +
                            "\", \"" +
                            escapeString(doc) +
                            "\"" +
                            (isProperty ? "" : ", " + builtInArguments(builtInMethod, true)) +
                            ", BuiltInBody.create(" +
                            builtInFactoryClass.getSimpleName().toString() +
                            "Factory." +
                            builtInMethod.getSimpleName().toString() +
                            "Factory.getInstance()" +
                            "))";

                        return (
                            "            Pair.create(new String[] {" +
                            Arrays.stream(targetTypes)
                                .map(t -> "\"" + escapeString(t) + "\"")
                                .collect(Collectors.joining(", ")) +
                            " }," +
                            wrapper +
                            ")"
                        );
                    })
                    .collect(Collectors.joining(",\n"))
            );

            stream.println("        );");
            stream.println("    }");
            stream.println("}");
        }

        builtInPackages.add(Pair.create(packageName, className));
    }

    /** Return the `input` string with all special characters escaped. */
    private static String escapeString(String input) {
        return input
            .replace("\n", "\\n")
            .replace("\r", "\\r")
            .replace("\t", "\\t")
            .replace("\b", "\\b")
            .replace("\f", "\\f")
            .replace("\"", "\\\"");
    }
}

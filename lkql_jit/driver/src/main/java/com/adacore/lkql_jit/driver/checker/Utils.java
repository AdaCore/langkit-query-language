//
//  Copyright (C) 2005-2026, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.driver.checker;

import com.adacore.lkql_jit.driver.diagnostics.DiagnosticCollector;
import com.adacore.lkql_jit.driver.diagnostics.Hint;
import com.adacore.lkql_jit.driver.diagnostics.variants.BaseDiagnostic;
import com.adacore.lkql_jit.driver.diagnostics.variants.Error;
import com.adacore.lkql_jit.driver.diagnostics.variants.Info;
import com.adacore.lkql_jit.driver.diagnostics.variants.Warning;
import com.adacore.lkql_jit.driver.source_support.SourceSection;
import com.adacore.lkql_jit.values.interop.LKQLBaseNamespace;
import com.adacore.lkql_jit.values.interop.LKQLDynamicObject;
import com.adacore.lkql_jit.values.interop.LKQLList;
import java.io.IOException;
import java.nio.file.Path;
import java.util.*;
import java.util.stream.Collectors;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.PolyglotException;
import org.graalvm.polyglot.Source;

/** Utils for checker related processes. */
public class Utils {

    /**
     * Process the provided LKQL rule file to extract all rule instances in it and return them.
     *
     * @param diagnostics Place all diagnostics fetched during the process in it.
     * @param context Execute the LKQL rule file in it.
     * @param repository Repository of rules available to instantiation.
     */
    public static List<RuleInstance> processLKQLRuleFile(
        DiagnosticCollector diagnostics,
        Context context,
        RuleRepository repository,
        Path lkqlRuleFile
    ) {
        try {
            // Evaluate the rule file to get its namespace
            var ruleFileExecutionResult = context.eval(
                Source.newBuilder("lkql", lkqlRuleFile.toFile()).build()
            );
            var defaultLocation = Optional.ofNullable(
                ruleFileExecutionResult.getSourceLocation()
            ).map(SourceSection::from);
            var ruleFileNamespace = ruleFileExecutionResult.as(LKQLBaseNamespace.class);

            // Prepare working variables and the result
            var generalInstances = ruleFileNamespace.getUncached("rules");
            var adaInstances = ruleFileNamespace.getUncached("ada_rules");
            var sparkInstances = ruleFileNamespace.getUncached("spark_rules");
            var res = new ArrayList<RuleInstance>();

            // Process the general instances object
            if (generalInstances instanceof LKQLDynamicObject obj) {
                res.addAll(
                    processInstancesObject(
                        diagnostics,
                        context,
                        repository,
                        obj,
                        RuleInstance.SourceMode.GENERAL
                    )
                );
            } else {
                diagnostics.add(
                    new Error(
                        "An LKQL rule file must define a \"rules\" top level object",
                        defaultLocation
                    )
                );
            }

            // Process the Ada instances object
            if (adaInstances != null) {
                if (adaInstances instanceof LKQLDynamicObject obj) {
                    res.addAll(
                        processInstancesObject(
                            diagnostics,
                            context,
                            repository,
                            obj,
                            RuleInstance.SourceMode.ADA
                        )
                    );
                } else {
                    diagnostics.add(
                        new Error(
                            "Value associated to \"ada_rules\" must be an object",
                            defaultLocation
                        )
                    );
                }
            }

            // Process the Spark instances object
            if (sparkInstances != null) {
                if (sparkInstances instanceof LKQLDynamicObject obj) {
                    res.addAll(
                        processInstancesObject(
                            diagnostics,
                            context,
                            repository,
                            obj,
                            RuleInstance.SourceMode.SPARK
                        )
                    );
                } else {
                    diagnostics.add(
                        new Error(
                            "Value associated to \"spark_rules\" must be an object",
                            defaultLocation
                        )
                    );
                }
            }

            // Finally return the result
            return res;
        } catch (IOException e) {
            diagnostics.add(
                new Error(
                    "Cannot read the LKQL rule file \"" +
                        lkqlRuleFile.getFileName() +
                        "\" (" +
                        e.getClass().getSimpleName() +
                        ": \"" +
                        e.getMessage() +
                        "\")"
                )
            );
        } catch (PolyglotException e) {
            diagnostics.handleException(e);
        }

        // This is the default return case, an empty instance list
        return List.of();
    }

    /**
     * Process the provided LKQL object as an instance container, and return all instances defined
     * in it. Instances are created with the provided source mode.
     */
    private static List<RuleInstance> processInstancesObject(
        DiagnosticCollector diagnostics,
        Context context,
        RuleRepository repository,
        LKQLDynamicObject object,
        RuleInstance.SourceMode sourceMode
    ) {
        // Create the result object
        var res = new ArrayList<RuleInstance>();

        // Get the location of the rule configuration object if possible
        var configLocation = Optional.ofNullable(context.asValue(object).getSourceLocation()).map(
            SourceSection::from
        );

        // Process each instantiated rule
        for (var ruleInstancesEntry : object.asMap().entrySet()) {
            // Get the rule identifier
            var ruleId = ruleInstancesEntry.getKey().toLowerCase();
            var instantiatedRule = repository.getRuleByName(ruleId);

            // Start by ensuring the rule exists
            if (instantiatedRule.isEmpty()) {
                diagnostics.add(
                    new Error(
                        "Unknown rule name \"" + ruleInstancesEntry.getKey() + '"',
                        configLocation
                    )
                );
                continue;
            }

            // Then process all arguments sets for the rule
            if (ruleInstancesEntry.getValue() instanceof LKQLList argSets) {
                if (argSets.size() == 0) {
                    // If not argument set is provided, create a default instance of the rule
                    res.add(
                        new RuleInstance(
                            instantiatedRule.get(),
                            Optional.empty(),
                            sourceMode,
                            Map.of(),
                            configLocation
                        )
                    );
                } else {
                    for (var maybeArgSet : argSets.getContent()) {
                        if (maybeArgSet instanceof LKQLDynamicObject argSet) {
                            instantiateWithArgumentSet(
                                diagnostics,
                                context,
                                sourceMode,
                                instantiatedRule.get(),
                                argSet
                            ).ifPresent(res::add);
                        } else {
                            diagnostics.add(
                                new Error(
                                    "Rule arguments must be in an object value",
                                    configLocation
                                )
                            );
                        }
                    }
                }
            } else {
                diagnostics.add(
                    new Error("The value associated to a rule name must be a list", configLocation)
                );
            }
        }

        // Return the result
        return res;
    }

    /** Internal helper to create an instance of the provided rule with an argument set. */
    private static Optional<RuleInstance> instantiateWithArgumentSet(
        DiagnosticCollector diagnostics,
        Context context,
        RuleInstance.SourceMode sourceMode,
        Rule instantiatedRule,
        LKQLDynamicObject argumentSet
    ) {
        boolean hasError = false;

        // Create a map going from lowered parameter name to their real name as declared in the
        // associated LKQL function.
        var ruleParameters = Arrays.stream(
            instantiatedRule.checker().parameterNames,
            1,
            instantiatedRule.checker().parameterNames.length
        ).collect(Collectors.toMap(String::toLowerCase, s -> s));

        // Create the new instance location
        var instanceLocation = Optional.ofNullable(
            context.asValue(argumentSet).getSourceLocation()
        ).map(SourceSection::from);

        // Process the argument set to extract the new instance config
        var instanceArgs = new HashMap<String, Object>();
        String instanceName = null;
        for (var argEntry : argumentSet.asMap().entrySet()) {
            var argName = argEntry.getKey().toLowerCase();

            // Special case for argument "instance_name" which defines the name of the instance
            if (argName.equals("instance_name")) instanceName = (String) argEntry.getValue();
            // All other arguments are processed normally
            else {
                if (ruleParameters.containsKey(argName)) {
                    instanceArgs.put(ruleParameters.get(argName), argEntry.getValue());
                } else {
                    diagnostics.add(
                        new Error(
                            "Rule \"" +
                                instantiatedRule.name() +
                                "\" doesn't have a parameter named \"" +
                                argName +
                                '"',
                            instanceLocation
                        )
                    );
                    hasError = true;
                }
            }
        }

        // Then return the new instance
        return hasError
            ? Optional.empty()
            : Optional.of(
                  new RuleInstance(
                      instantiatedRule,
                      Optional.ofNullable(instanceName),
                      sourceMode,
                      instanceArgs,
                      instanceLocation
                  )
              );
    }

    /**
     * Post-process the provided instance list to check their validity and unicity, returning the
     * list of valid instances.
     */
    public static List<RuleInstance> postProcessInstances(
        DiagnosticCollector diagnostics,
        List<RuleInstance> instances,
        boolean verbose
    ) {
        // Prepare the result list and a unicity map
        var unicityMap = new HashMap<String, RuleInstance>();

        var res = instances
            .stream()
            .filter(i -> i.isValid(diagnostics))
            .filter(instance -> {
                // Check for instances unicity
                var sameNameInstance = unicityMap.get(instance.identifier());
                if (sameNameInstance != null) {
                    final BaseDiagnostic diag;
                    if (instance.isEquivalent(sameNameInstance)) {
                        diag = new Warning(
                            "Instance \"" +
                                instance.name() +
                                "\" is duplicated, ignoring this declaration",
                            instance.instanceLocation
                        );
                        sameNameInstance.instanceLocation.ifPresent(l ->
                            diag.addHint(new Hint("Previous declaration was here", l))
                        );
                    } else {
                        diag = new Error(
                            "Multiple instances with the name \"" +
                                instance.name() +
                                "\", instance names must be unique",
                            instance.instanceLocation
                        );
                        sameNameInstance.instanceLocation.ifPresent(l ->
                            diag.addHint(
                                new Hint(
                                    "Previous instance named \"" +
                                        sameNameInstance.name() +
                                        "\" was declared here",
                                    l
                                )
                            )
                        );
                    }
                    diagnostics.add(diag);
                    return false;
                } else {
                    unicityMap.put(instance.identifier(), instance);
                    if (verbose) {
                        diagnostics.add(
                            new Info(
                                "Register new instance \"" + instance.name() + '"',
                                instance.instanceLocation
                            )
                        );
                    }
                    return true;
                }
            })
            .toList();

        // Now check for instances running the same rule with the same config
        for (int i = 0; i < res.size(); i++) {
            var instance = res.get(i);
            for (int j = i + 1; j < res.size(); j++) {
                var otherInstance = res.get(j);
                if (instance.isEquivalent(otherInstance)) {
                    var warning = new Warning(
                        "Instance \"" +
                            instance.name() +
                            "\" is running the same check as instance \"" +
                            otherInstance.name() +
                            '"',
                        instance.instanceLocation
                    );
                    otherInstance.instanceLocation.ifPresent(l ->
                        warning.addHint(
                            new Hint(
                                "Instance \"" + otherInstance.name() + "\" was declared here",
                                l
                            )
                        )
                    );
                    diagnostics.add(warning);
                }
            }
        }

        // Finally, return the list of filtered and checked instances
        return res;
    }
}

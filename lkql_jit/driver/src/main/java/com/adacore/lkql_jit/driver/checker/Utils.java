//
//  Copyright (C) 2005-2026, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.driver.checker;

import com.adacore.lkql_jit.driver.diagnostics.DiagnosticCollector;
import com.adacore.lkql_jit.driver.diagnostics.variants.Error;
import com.adacore.lkql_jit.values.interop.LKQLBaseNamespace;
import com.adacore.lkql_jit.values.interop.LKQLDynamicObject;
import com.adacore.lkql_jit.values.interop.LKQLList;
import java.io.IOException;
import java.nio.file.Path;
import java.util.*;
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
            var ruleFileNamespace = context
                .eval(Source.newBuilder("lkql", lkqlRuleFile.toFile()).build())
                .as(LKQLBaseNamespace.class);

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
                        repository,
                        lkqlRuleFile,
                        obj,
                        RuleInstance.SourceMode.GENERAL
                    )
                );
            } else {
                errorInRuleFile(
                    diagnostics,
                    lkqlRuleFile,
                    "An LKQL rule file must define a \"rules\" top level object"
                );
            }

            // Process the Ada instances object
            if (adaInstances != null) {
                if (adaInstances instanceof LKQLDynamicObject obj) {
                    res.addAll(
                        processInstancesObject(
                            diagnostics,
                            repository,
                            lkqlRuleFile,
                            obj,
                            RuleInstance.SourceMode.ADA
                        )
                    );
                } else {
                    errorInRuleFile(
                        diagnostics,
                        lkqlRuleFile,
                        "Value associated to \"ada_rules\" must be an object"
                    );
                }
            }

            // Process the Spark instances object
            if (sparkInstances != null) {
                if (sparkInstances instanceof LKQLDynamicObject obj) {
                    res.addAll(
                        processInstancesObject(
                            diagnostics,
                            repository,
                            lkqlRuleFile,
                            obj,
                            RuleInstance.SourceMode.SPARK
                        )
                    );
                } else {
                    errorInRuleFile(
                        diagnostics,
                        lkqlRuleFile,
                        "Value associated to \"spark_rules\" must be an object"
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
                        e.getMessage() +
                        ')'
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
        RuleRepository repository,
        Path lkqlRuleFile,
        LKQLDynamicObject object,
        RuleInstance.SourceMode sourceMode
    ) {
        // Create the result object
        var res = new ArrayList<RuleInstance>();

        // Process each instantiated rule
        for (var ruleInstancesEntry : object.asMap().entrySet()) {
            // Get the rule identifier
            var ruleId = ruleInstancesEntry.getKey().toLowerCase();
            var instantiatedRule = repository.getRuleByName(ruleId);

            // Start by ensuring the rule exists
            if (instantiatedRule.isEmpty()) {
                errorInRuleFile(
                    diagnostics,
                    lkqlRuleFile,
                    "Unknown rule name \"" + ruleInstancesEntry.getKey() + '"'
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
                            Optional.empty()
                        )
                    );
                } else {
                    for (var maybeArgSet : argSets.getContent()) {
                        if (maybeArgSet instanceof LKQLDynamicObject argSet) {
                            res.add(
                                instantiateWithArgumentSet(
                                    sourceMode,
                                    instantiatedRule.get(),
                                    argSet
                                )
                            );
                        } else {
                            errorInRuleFile(
                                diagnostics,
                                lkqlRuleFile,
                                "Rule arguments must be in an object value"
                            );
                        }
                    }
                }
            } else {
                errorInRuleFile(
                    diagnostics,
                    lkqlRuleFile,
                    "The value associated to a rule name must be a list"
                );
            }
        }

        // Return the result
        return res;
    }

    /** Internal helper to create an instance of the provided rule with an argument set. */
    private static RuleInstance instantiateWithArgumentSet(
        RuleInstance.SourceMode sourceMode,
        Rule instantiatedRule,
        LKQLDynamicObject argumentSet
    ) {
        // Process the argument set to extract the new instance config
        var instanceArgs = new HashMap<String, Object>();
        String instanceName = null;
        for (var argEntry : argumentSet.asMap().entrySet()) {
            var argName = argEntry.getKey().toLowerCase();

            // Special case for argument "instance_name" which defines the name of the instance
            if (argName.equals("instance_name")) instanceName = (String) argEntry.getValue();

            // All other arguments are processed normally
            instanceArgs.put(argName, argEntry.getValue());
        }

        // Then return the new instance
        return new RuleInstance(
            instantiatedRule,
            Optional.ofNullable(instanceName),
            sourceMode,
            instanceArgs,
            Optional.empty()
        );
    }

    /** Internal helper to signal an error in an LKQL rule file. */
    private static void errorInRuleFile(
        DiagnosticCollector diagnostics,
        Path lkqlRuleFile,
        String message
    ) {
        diagnostics.add(new Error(lkqlRuleFile.getFileName().toString() + ": " + message));
    }
}

//
//  Copyright (C) 2005-2026, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.driver.diagnostics;

import com.adacore.lkql_jit.driver.checker.Rule;
import com.adacore.lkql_jit.driver.checker.RuleInstance;
import com.adacore.lkql_jit.driver.diagnostics.variants.Error;
import com.adacore.lkql_jit.driver.diagnostics.variants.Exception;
import com.adacore.lkql_jit.driver.diagnostics.variants.*;
import com.adacore.lkql_jit.driver.source_support.SourceSection;
import de.jcup.sarif_2_1_0.model.Stack;
import de.jcup.sarif_2_1_0.model.*;
import java.util.*;
import java.util.function.Consumer;

public class SarifReportCreator implements Consumer<BaseDiagnostic> {

    // ----- Attributes -----

    /** SARIF result objects, each one corresponding to an instance violation. */
    private final List<Result> results;

    /** Invocation notifications, all other reported diagnostics that aren't rule violation. */
    private final List<Notification> notifications;

    // ----- Constructors -----

    public SarifReportCreator(
        SarifSchema210 output,
        String version,
        List<RuleInstance> instances,
        boolean executionSuccessful
    ) {
        this.results = new ArrayList<>();
        this.notifications = new ArrayList<>();

        // Create a map associating each enabled rules to their default instance and a list of
        // remaining instances.
        Map<Rule, Optional<RuleInstance>> enabledRules = new HashMap<>();
        List<RuleInstance> remainingInstances = new ArrayList<>();
        for (var instance : instances) {
            var rule = instance.instantiatedRule;
            if (instance.instanceName.isPresent()) {
                enabledRules.putIfAbsent(rule, Optional.empty());
                remainingInstances.add(instance);
            } else {
                enabledRules.put(rule, Optional.of(instance));
            }
        }

        // Compute the set of SARIF rules
        Set<ReportingDescriptor> sarifRules = new TreeSet<>(
            Comparator.comparing(ReportingDescriptor::getId)
        );
        enabledRules.forEach((rule, instance) -> {
            // Create the SARIF rule object
            var sarifRule = new ReportingDescriptor();
            sarifRule.setId(rule.name());
            sarifRule.setName(rule.displayName());

            // Create a short description of the rule
            var shortDescription = new MultiformatMessageString();
            shortDescription.setText(rule.message());
            sarifRule.setShortDescription(shortDescription);

            // Create a help message for the rule
            if (!rule.help().equals(rule.name())) {
                var help = new MultiformatMessageString();
                help.setText(rule.help());
                sarifRule.setHelp(help);
            }

            // Set rule configuration
            sarifRule.setDefaultConfiguration(getConfig(rule, instance));

            // Add the SARIF rule object to the rule set
            sarifRules.add(sarifRule);
        });
        remainingInstances.forEach(i -> {
            // Create the SARIF rule object
            var sarifRule = new ReportingDescriptor();
            sarifRule.setId(i.instanceName.get().toLowerCase());
            sarifRule.setName(i.instanceName.get());

            // Link the instance to its instantiated rule
            var ruleRelationship = new ReportingDescriptorRelationship();
            var ruleReference = new ReportingDescriptorReference();
            ruleReference.setId(i.instantiatedRule.name());
            ruleRelationship.setKinds(Set.of("relevant"));
            ruleRelationship.setTarget(ruleReference);
            sarifRule.setRelationships(Set.of(ruleRelationship));

            // Set the instance configuration
            sarifRule.setDefaultConfiguration(getConfig(i.instantiatedRule, Optional.of(i)));

            // Finally, add the sarif rule to the rule set
            sarifRules.add(sarifRule);
        });

        // Initialize the SARIF report
        var run = new Run();
        var tool = new Tool();
        var driver = new ToolComponent();
        var invocation = new Invocation();
        driver.setName("LKQL");
        driver.setVersion(version);
        driver.setRules(sarifRules);
        tool.setDriver(driver);
        invocation.setToolExecutionNotifications(notifications);
        invocation.setExecutionSuccessful(executionSuccessful);
        run.setTool(tool);
        run.setResults(results);
        run.setInvocations(List.of(invocation));
        output.setVersion(SarifSchema210.Version._2_1_0);
        output.setRuns(List.of(run));
    }

    // ----- Instance methods -----

    @Override
    public void accept(BaseDiagnostic diagnostic) {
        // Create the message object
        var message = new Message();
        message.setText(diagnostic.message);

        // Create the location object for the diagnostic if it is present
        var location = diagnostic.location.flatMap(l ->
            toPhysicalLocation(l).map(p -> {
                var res = new Location();
                res.setPhysicalLocation(p);
                return res;
            })
        );

        // Special handling for rule violations
        if (diagnostic instanceof RuleViolation violation) {
            // Create the new result object and initialize it
            var result = new Result();
            result.setRuleId(
                violation.violatedInstance.instanceName
                    .orElse(violation.violatedInstance.instantiatedRule.name())
                    .toLowerCase()
            );
            result.setMessage(message);
            // Here we don't check if the location is present because there is no rule violation
            // without location.
            result.setLocations(List.of(location.get()));

            // Finally, add the result in the report
            results.add(result);
        }
        // General case handling
        else {
            // Create a new notification object to represents the diagnostic
            var notification = new Notification();
            notification.setLevel(
                switch (diagnostic) {
                    case Error _, Exception _ -> Notification.Level.ERROR;
                    case Warning _ -> Notification.Level.WARNING;
                    case Info _ -> Notification.Level.NOTE;
                    case RuleViolation _ -> throw new RuntimeException("Should not reach here");
                }
            );
            notification.setMessage(message);
            location.ifPresent(l -> {
                notification.setLocations(Set.of(l));
            });

            // If the diagnostic is an exception, fill the exception property
            if (diagnostic instanceof Exception e) {
                // Create the exception object
                var sarifException = new de.jcup.sarif_2_1_0.model.Exception();
                sarifException.setKind(e.kind.kindName);
                sarifException.setMessage(e.message);

                // Fill the SARIF stack trace
                var sarifStack = new Stack();
                sarifStack.setFrames(
                    e.callStack
                        .stream()
                        .map(f -> {
                            var sarifFrame = new StackFrame();
                            sarifFrame.setLocation(toLocation(f));
                            return sarifFrame;
                        })
                        .toList()
                );
                sarifException.setStack(sarifStack);

                // Place it in the final report
                notification.setException(sarifException);
            }

            // Finally, add the notification to the report
            notifications.add(notification);
        }
    }

    // ----- Class methods -----

    /** Internal helper to get the SARIF configuration of a rule and its instance. */
    private static ReportingConfiguration getConfig(Rule rule, Optional<RuleInstance> instance) {
        // Create SARIF objects
        var ruleConfig = new ReportingConfiguration();
        var parameters = new PropertyBag();

        // For each rule parameter, get its value
        for (int i = 1; i < rule.checker().parameterNames.length; i++) {
            var name = rule.checker().parameterNames[i];
            var value = instance
                .flatMap(ins -> Optional.ofNullable(toLiteral(ins.arguments.get(name))))
                .orElse(rule.checker().parameterDefaultValues[i].getSourceSection().getCharacters())
                .toString();
            parameters.setAdditionalProperty(name, value);
        }

        // Fill SARIF objects and return the result
        ruleConfig.setLevel(ReportingConfiguration.Level.WARNING);
        if (!parameters.getAdditionalProperties().isEmpty()) ruleConfig.setParameters(parameters);
        return ruleConfig;
    }

    /** Transform the provided object in order to be able to represent it as an LKQL. */
    private static Object toLiteral(Object o) {
        return switch (o) {
            case String s -> '"' + s + '"';
            case null -> null;
            default -> o;
        };
    }

    /** Internal helper to get a SARIF physical location from a diagnostic source section object. */
    private static Optional<PhysicalLocation> toPhysicalLocation(SourceSection location) {
        return location
            .getSourceFile()
            .map(f -> {
                var physicalLoc = new PhysicalLocation();
                var artifactLoc = new ArtifactLocation();
                var region = new Region();
                artifactLoc.setUri(f.toUri().toString());
                region.setStartLine(location.startLine());
                region.setStartColumn(location.startColumn());
                region.setEndLine(location.endLine());
                region.setEndColumn(location.endColumn());
                physicalLoc.setArtifactLocation(artifactLoc);
                physicalLoc.setRegion(region);
                return physicalLoc;
            });
    }

    /** Internal helper to get SARIF location object from a call stack frame. */
    private static Location toLocation(Exception.StackFrame frame) {
        var res = new Location();
        switch (frame) {
            case Exception.CustomFrame f -> {
                toPhysicalLocation(f.callLocation).ifPresent(res::setPhysicalLocation);
                var logicalLocation = new LogicalLocation();
                logicalLocation.setKind("function");
                logicalLocation.setName(f.contextName);
                res.setLogicalLocations(Set.of(logicalLocation));
            }
            case Exception.JavaFrame f -> {
                var logicalLocation = new LogicalLocation();
                logicalLocation.setKind("method");
                logicalLocation.setName(f.stackTraceElement.getMethodName());
                logicalLocation.setFullyQualifiedName(f.callContext());
                res.setLogicalLocations(Set.of(logicalLocation));
            }
        }
        return res;
    }
}

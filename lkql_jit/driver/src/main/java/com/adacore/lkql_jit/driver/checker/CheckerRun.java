//
//  Copyright (C) 2005-2026, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.driver.checker;

import com.adacore.langkit_support.LangkitSupport;
import com.adacore.libadalang.Libadalang;
import com.adacore.lkql_jit.driver.diagnostics.DiagnosticCollector;
import com.adacore.lkql_jit.driver.diagnostics.Hint;
import com.adacore.lkql_jit.driver.diagnostics.variants.Error;
import com.adacore.lkql_jit.driver.diagnostics.variants.RuleViolation;
import com.adacore.lkql_jit.driver.source_support.SourceLinesCache;
import com.adacore.lkql_jit.driver.source_support.SourceSection;
import com.adacore.lkql_jit.values.interop.LKQLDynamicObject;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.*;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.PolyglotException;

/**
 * The main checking class, it contains all the traversal and checking logic. An instance of this
 * class corresponds to 1 checking process.
 */
public final class CheckerRun {

    // ----- Attributes -----

    /** Whether debug information should be displayed. */
    private final boolean debugMode;

    /** Object to cache source lines of analyzed Ada source and avoid multiple decoding. */
    private final SourceLinesCache linesCache;

    /** Rule instances to run during the checking process. */
    private final List<RuleInstance> ruleInstances;

    /** Context to execute LKQL functions in. */
    private final Context executionContext;

    /** Analysis context used to parse sources to analyze */
    private final LangkitSupport.AnalysisContextInterface analysisContext;

    /** Analysis units to apply checkers on. */
    private final List<LangkitSupport.AnalysisUnit> units;

    /** Whether Ada generic instantiations should be traversed by this run. */
    private final boolean followGenericInstantiations;

    /** Whether and how to apply auto-fixing function of executed rules. */
    private final AutoFixMode autoFixMode;

    // ----- Constructors -----

    public CheckerRun(
        boolean debugMode,
        SourceLinesCache linesCache,
        List<RuleInstance> ruleInstances,
        Context executionContext,
        LangkitSupport.AnalysisContextInterface analysisContext,
        List<LangkitSupport.AnalysisUnit> units,
        AutoFixMode autoFixMode
    ) {
        this.debugMode = debugMode;
        this.linesCache = linesCache;
        this.ruleInstances = ruleInstances;
        this.executionContext = executionContext;
        this.analysisContext = analysisContext;
        this.units = units;
        this.followGenericInstantiations = ruleInstances
            .stream()
            .anyMatch(i -> i.instantiatedRule.followGenericInstantiations());
        this.autoFixMode = autoFixMode;
    }

    // ----- Instance methods -----

    /** Perform the checking process and report all diagnostics in the provided collector. */
    public void start(DiagnosticCollector diagnostics) {
        // Start by splitting rule instances by kinds
        List<RuleInstance> nodeRuleInstances = new ArrayList<>();
        List<RuleInstance> unitRuleInstances = new ArrayList<>();
        ruleInstances.forEach(i -> {
            switch (i.instantiatedRule.kind()) {
                case NODE -> nodeRuleInstances.add(i);
                case UNIT -> unitRuleInstances.add(i);
            }
        });

        // First run node rules
        runNodeRules(nodeRuleInstances, diagnostics);

        // Then run unit rules
        runUnitRules(unitRuleInstances, diagnostics);
    }

    /**
     * Apply all given node rule instances, collect violations in the provided diagnostic collector
     * and if required apply auto-fixing functions.
     */
    private void runNodeRules(List<RuleInstance> ruleInstances, DiagnosticCollector diagnostics) {
        // Start by splitting node rule instances regarding their source mode
        List<RuleInstance> generalInstances = new ArrayList<>();
        List<RuleInstance> adaInstances = new ArrayList<>();
        List<RuleInstance> sparkInstances = new ArrayList<>();
        ruleInstances.forEach(i -> {
            switch (i.sourceMode) {
                case GENERAL -> generalInstances.add(i);
                case ADA -> adaInstances.add(i);
                case SPARK -> sparkInstances.add(i);
            }
        });

        // Then for each unit to analyze visit their tree by applying all instances to each node
        for (var unit : units) {
            visitTree(generalInstances, adaInstances, sparkInstances, unit.getRoot(), diagnostics);

            // If the unit has some modifications, then apply them as requested.
            var rewritingContext = analysisContext.getRewritingContext();
            if (rewritingContext != null) {
                // Apply the current rewriting context
                var applyRes = rewritingContext.apply();

                // If there is an error in the rewriting application report them in diagnositcs
                if (!applyRes.success) {
                    rewritingContext.close();
                    diagnostics.add(
                        new Error(
                            "Error(s) while applying a rewriting context: " +
                                Arrays.toString(applyRes.getDiagnostics())
                        )
                    );
                }
                // Otherwise, process the rewrote unit as required
                else {
                    var patchedSource = unit.getText();
                    var basePatchedFileName = unit.getFileName(false);
                    var fullPatchedFileName = unit.getFileName(true);
                    try {
                        switch (autoFixMode) {
                            case DISPLAY -> {
                                // If the required action is the display, just print the patched
                                // unit.
                                var header = "Patched \"" + basePatchedFileName + "\":\n";
                                header = header + "=".repeat(header.length() - 1) + "\n";
                                System.out.println(header);
                                System.out.println(patchedSource);
                            }
                            case NEW_FILE -> {
                                // If the required action is creating a new file, then write in a
                                // file named <original_file_name>.patched the patched unit.
                                var newFile = Paths.get(fullPatchedFileName + ".patched");
                                Files.deleteIfExists(newFile);
                                Files.createFile(newFile);
                                Files.writeString(newFile, patchedSource);
                                System.out.println(
                                    "File \"" +
                                        basePatchedFileName +
                                        "\" has been patched (result in \"" +
                                        basePatchedFileName +
                                        ".patched\")"
                                );
                            }
                            case PATCH_FILE -> {
                                // If the required action is to patch the existing file, just
                                // replace the content of the original file with the patched unit.
                                var originalFile = Paths.get(fullPatchedFileName);
                                Files.writeString(originalFile, patchedSource);
                                System.out.println(
                                    "File \"" + basePatchedFileName + "\" has been patched"
                                );
                            }
                        }
                    } catch (IOException e) {
                        throw new RuntimeException(e);
                    }
                }
            }
        }
    }

    /**
     * Visit the parsing tree from the given root, applying required checkers and collecting their
     * results.
     */
    private void visitTree(
        List<RuleInstance> generalInstances,
        List<RuleInstance> adaInstances,
        List<RuleInstance> sparkInstances,
        LangkitSupport.NodeInterface treeRoot,
        DiagnosticCollector diagnostics
    ) {
        // Create and initialize a list to store steps of the tree visit
        Stack<VisitStep> visitStack = new Stack<>();
        visitStack.push(new VisitStep(treeRoot, false, false));

        // Then visit all the tree
        while (!visitStack.isEmpty()) {
            // Start by getting the current step
            var step = visitStack.pop();

            // Call checkers on the current node
            executeInstancesOnStep(generalInstances, step, diagnostics);

            // If we're in Ada code execute the Ada checkers else execute the SPARK checkers
            if (step.inSparkCode) {
                executeInstancesOnStep(sparkInstances, step, diagnostics);
            } else {
                executeInstancesOnStep(adaInstances, step, diagnostics);
            }

            // If required visit the generic instantiation
            switch (step.node) {
                case Libadalang.GenericInstantiation genInst when followGenericInstantiations:
                    // If the node is a generic instantiation, traverse the instantiated generic
                    var genDecl = genInst.pDesignatedGenericDecl();
                    var genBody = genDecl.pBodyPartForDecl(false);
                    if (!genBody.isNone()) {
                        visitStack.push(new VisitStep(genBody, true, step.inSparkCode));
                    }
                    visitStack.push(new VisitStep(genDecl, true, step.inSparkCode));
                    break;
                case Libadalang.BodyStub stub when step.inGenericInstantiation:
                    // If this node is a body stub, and we are currently traversing a generic
                    // instantiation, we should also traverse the stub's completion.
                    var stubBody = stub.pNextPartForDecl(false);
                    visitStack.push(new VisitStep(stubBody, true, step.inSparkCode));
                default:
            }

            // Then add node's children to visit steps.
            // We iterate on the children list from the end to make a prefix tree traversal.
            for (int i = step.node.getChildrenCount() - 1; i >= 0; i--) {
                var child = step.node.getChild(i);
                if (!child.isNone()) {
                    if (
                        !sparkInstances.isEmpty() &&
                        child instanceof Libadalang.BaseSubpBody subpBody
                    ) {
                        visitStack.push(
                            new VisitStep(
                                child,
                                step.inGenericInstantiation,
                                subpBody.pIsSubjectToProof()
                            )
                        );
                    } else {
                        visitStack.push(
                            new VisitStep(child, step.inGenericInstantiation, step.inSparkCode)
                        );
                    }
                }
            }
        }
    }

    /** Internal helper to run all rule instances on a provided tree visit step. */
    private void executeInstancesOnStep(
        List<RuleInstance> ruleInstances,
        VisitStep step,
        DiagnosticCollector diagnostics
    ) {
        for (var instance : ruleInstances) {
            if (
                !step.inGenericInstantiation ||
                instance.instantiatedRule.followGenericInstantiations()
            ) {
                try {
                    // Create arguments to pass to the rule's function
                    var arguments = instance.checkerArguments;
                    if (instance.instantiatedRule.checker().takesClosure()) {
                        arguments[1] = step.node;
                    } else {
                        arguments[0] = step.node;
                    }

                    // Call the rule's function and get the boolean result
                    var checkRes = executionContext
                        .asValue(instance.instantiatedRule.checker())
                        .execute(arguments);
                    if (!checkRes.isBoolean()) {
                        diagnostics.add(
                            new Error(
                                "The result of a node checking function must be a boolean",
                                SourceSection.wrap(
                                    instance.instantiatedRule
                                        .checker()
                                        .getDeclarationLocation()
                                        .get()
                                )
                            )
                        );
                    } else if (checkRes.asBoolean()) {
                        if (autoFixMode == AutoFixMode.NONE) {
                            var locationNode = switch (step.node) {
                                case Libadalang.BasicDecl decl -> {
                                    var defName = decl.pDefiningName();
                                    yield defName.isNone() ? step.node : defName;
                                }
                                default -> step.node;
                            };
                            diagnostics.add(
                                new RuleViolation(
                                    instance,
                                    SourceSection.wrap(locationNode, linesCache)
                                )
                            );
                        } else if (instance.instantiatedRule.autoFix().isPresent()) {
                            var autoFix = instance.instantiatedRule.autoFix().get();

                            // Create arguments to call the auto-fix function
                            var closureOffset = autoFix.takesClosure() ? 1 : 0;
                            var autoFixArguments = instance.autoFixArguments;
                            autoFixArguments[closureOffset] = step.node;
                            autoFixArguments[1 + closureOffset] = getRewritingContext();

                            // Then call the auto-fix function
                            executionContext.asValue(autoFix).execute(autoFixArguments);
                        }
                    }
                } catch (PolyglotException e) {
                    diagnostics.handleException(
                        e,
                        new Hint(
                            "Error occurred when analyzing " + step.node.toString(),
                            SourceSection.wrap(step.node, linesCache)
                        )
                    );
                }
            }
        }
    }

    /**
     * Apply given unit rule instances on all specified units and collect results in the provided
     * diagnostic collector.
     */
    private void runUnitRules(
        List<RuleInstance> unitRuleInstances,
        DiagnosticCollector diagnostics
    ) {
        for (var instance : unitRuleInstances) {
            for (var unit : units) {
                executeInstanceOnUnit(instance, unit, diagnostics);
            }
        }
    }

    /** Internal helper to run a rule instance on the provided analysis unit. */
    private void executeInstanceOnUnit(
        RuleInstance instance,
        LangkitSupport.AnalysisUnit unit,
        DiagnosticCollector diagnostics
    ) {
        // Create arguments to pass to the rule's checking function
        var arguments = instance.checkerArguments;
        if (instance.instantiatedRule.checker().takesClosure()) {
            arguments[1] = unit;
        } else {
            arguments[0] = unit;
        }

        // Then call the checker
        try {
            var checkRes = this.executionContext.asValue(
                instance.instantiatedRule.checker()
            ).execute(arguments);

            // Ensure the checker result is an iterable value
            if (checkRes.hasIterator()) {
                var iterator = checkRes.getIterator();
                assert iterator.isIterator();

                // For each element consider it as a violation report
                while (iterator.hasIteratorNextElement()) {
                    var resObj = iterator.getIteratorNextElement().as(LKQLDynamicObject.class);
                    var message = (String) resObj.getUncached("message");
                    var location = switch (resObj.getUncached("loc")) {
                        case LangkitSupport.NodeInterface ni -> SourceSection.wrap(ni, linesCache);
                        case LangkitSupport.TokenInterface ti -> SourceSection.wrap(ti, linesCache);
                        default -> null;
                    };

                    // If the violation report location is valid, emit a rule violation
                    if (location != null) {
                        diagnostics.add(new RuleViolation(message, instance, location));
                    } else {
                        diagnostics.add(
                            new Error(
                                "Checker result is not locatable",
                                SourceSection.wrap(
                                    instance.instantiatedRule
                                        .checker()
                                        .getDeclarationLocation()
                                        .get()
                                )
                            )
                        );
                    }
                }
            } else {
                diagnostics.add(
                    new Error(
                        "Checker result is not iterable",
                        SourceSection.wrap(
                            instance.instantiatedRule.checker().getDeclarationLocation().get()
                        )
                    )
                );
            }
        } catch (PolyglotException e) {
            diagnostics.handleException(
                e,
                new Hint(
                    "Error occurred when analyzing " + unit.getFileName(false),
                    SourceSection.wrap(unit.getRoot(), linesCache)
                )
            );
        }
    }

    /** Get the rewriting context for the current run, creating it if it hasn't been. */
    private LangkitSupport.RewritingContextInterface getRewritingContext() {
        var ctx = analysisContext.getRewritingContext();
        return ctx == null ? analysisContext.startRewriting() : ctx;
    }

    // ----- Inner classes and enums -----

    /** Represents the way to apply auto-fix functions when one is available. */
    public enum AutoFixMode {
        /** Do not run auto-fix functions at all. */
        NONE,

        /** Include auto-fix result in the generated report. */
        DISPLAY,

        /** Create a new Ada file containing the patched sources. */
        NEW_FILE,

        /** Apply auto-fix result directly on Ada sources. */
        PATCH_FILE,
    }

    /** Represents a step in the visit of a Libadalang parsing tree in the checking context. */
    private record VisitStep(
        LangkitSupport.NodeInterface node,
        boolean inGenericInstantiation,
        boolean inSparkCode
    ) {}
}

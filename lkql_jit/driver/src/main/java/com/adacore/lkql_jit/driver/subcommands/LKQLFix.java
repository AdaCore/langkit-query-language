//
//  Copyright (C) 2005-2026, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.driver.subcommands;

import com.adacore.lkql_jit.driver.checker.CheckerRun;
import com.adacore.lkql_jit.driver.checker.RuleInstance;
import com.adacore.lkql_jit.driver.diagnostics.variants.Error;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;
import picocli.CommandLine;

/**
 * This class represents the LKQL fixer entry point. The subcommand "fix" of the LKQL driver is
 * mainly used to test auto-fixing functions defined on GNATcheck rules.
 */
@CommandLine.Command(
    name = "fix",
    description = "Fixer driver, to apply LKQL defined auto-fixes to sources. In this mode, you" +
        " can only enable rules that defines an auto-fixing function."
)
public class LKQLFix extends BaseCheckerSubcommand {

    // ---- Attributes -----

    @CommandLine.Option(
        names = { "--auto-fix-mode" },
        description = "Mode to apply auto fixes (default is DISPLAY)" +
            "%nPossible values: ${COMPLETION-CANDIDATES}",
        completionCandidates = AutoFixModeCompletion.class
    )
    public CheckerRun.AutoFixMode autoFixMode = CheckerRun.AutoFixMode.DISPLAY;

    // ----- Constructors -----

    public LKQLFix() {}

    // ----- Instance methods -----

    @Override
    protected List<RuleInstance> postProcessInstances(List<RuleInstance> ruleInstances) {
        return ruleInstances
            .stream()
            .filter(i -> {
                if (i.instantiatedRule.autoFix().isEmpty()) {
                    diagnostics.add(
                        new Error(
                            "Rule \"" +
                                i.instantiatedRule.name() +
                                "\" is not defining any auto-fixing function"
                        )
                    );
                }
                return i.instantiatedRule.autoFix().isPresent();
            })
            .toList();
    }

    @Override
    protected CheckerRun.AutoFixMode getAutoFixMode() {
        return autoFixMode;
    }

    // ----- Inner classes -----

    /** Utility class to provide auto-complete for the auto-fix mode. */
    public static class AutoFixModeCompletion implements Iterable<String> {

        @Override
        public Iterator<String> iterator() {
            return Arrays.stream(CheckerRun.AutoFixMode.values()).map(Object::toString).iterator();
        }
    }
}

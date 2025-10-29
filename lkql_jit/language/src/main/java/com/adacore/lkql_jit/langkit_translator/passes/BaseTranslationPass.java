//
//  Copyright (C) 2005-2025, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.langkit_translator.passes;

import com.adacore.langkit_support.LangkitSupport;
import com.adacore.lkql_jit.LKQLLanguage;
import com.adacore.lkql_jit.built_ins.AllBuiltIns;
import com.adacore.lkql_jit.checker.utils.CheckerUtils;
import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.langkit_translator.passes.framing_utils.ScriptFrames;
import com.adacore.lkql_jit.nodes.arguments.Arg;
import com.adacore.lkql_jit.nodes.arguments.ArgList;
import com.adacore.lkql_jit.nodes.arguments.ExprArg;
import com.adacore.lkql_jit.nodes.arguments.NamedArg;
import com.adacore.lkql_jit.nodes.expressions.Expr;
import com.adacore.lkql_jit.nodes.expressions.value_read.*;
import com.adacore.lkql_jit.utils.source_location.SourceSectionWrapper;
import com.oracle.truffle.api.source.Source;
import com.oracle.truffle.api.source.SourceSection;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

/** This is a class to share common code between the LKQL and Lkt lowering passes. */
public class BaseTranslationPass {

    /** Source currently being translated. */
    final Source source;

    /** Frames in the currently translated source. */
    final ScriptFrames frames;

    protected RuntimeException translationError(SourceSection location, String message) {
        LKQLLanguage.getContext(null)
            .getDiagnosticEmitter()
            .emitDiagnostic(
                CheckerUtils.MessageKind.ERROR,
                message,
                null,
                new SourceSectionWrapper(location)
            );
        return LKQLRuntimeException.create("Errors during analysis");
    }

    protected RuntimeException translationError(LangkitSupport.NodeInterface node, String message) {
        LKQLLanguage.getContext(null)
            .getDiagnosticEmitter()
            .emitDiagnostic(
                CheckerUtils.MessageKind.ERROR,
                message,
                null,
                SourceSectionWrapper.create(node.getSourceLocationRange(), source)
            );
        return LKQLRuntimeException.create("Errors during analysis");
    }

    public BaseTranslationPass(Source source, ScriptFrames frames) {
        this.source = source;
        this.frames = frames;
    }

    /**
     * Create the source location for the given node.
     *
     * @param node The node to create the source location for.
     * @return The source location.
     */
    public SourceSection loc(LangkitSupport.NodeInterface node) {
        return SourceSectionWrapper.createSection(node.getSourceLocationRange(), this.source);
    }

    /**
     * Process arguments into a list, verifying that
     * all named args are placed after unnamed args and
     * no named arg is repeated.
     *
     * @param args The list of args to be processed.
     * @param loc  The source location for the ArgList to have.
     * @return The processed argument list.
     */
    public ArgList buildArgs(List<Arg> args, SourceSection loc) {
        // Visit all arguments to create the argument list and perform static checks
        final Set<String> seenNames = new HashSet<>();
        for (var curArg : args) {
            if (curArg instanceof ExprArg exprArg && !seenNames.isEmpty()) {
                throw LKQLRuntimeException.positionAfterNamedArgument(exprArg); // Found positional argument after named argument
            } else if (curArg instanceof NamedArg namedArg) {
                if (seenNames.contains(namedArg.getArgStringName())) {
                    throw LKQLRuntimeException.multipleSameNameArgument(namedArg); // Found duplicate of named argument
                }

                seenNames.add(namedArg.getArgStringName());
            }
        }
        // Return the new argument list node
        return new ArgList(loc, args.toArray(new Arg[0]));
    }

    public Expr buildRead(String symbol, SourceSection loc) {
        // First look for the symbol in the frame local bindings
        if (this.frames.isBinding(symbol) && this.frames.isBindingDeclared(symbol)) {
            return new ReadLocal(loc, this.frames.getBinding(symbol));
        }
        // In a second time look in the parameters of the frame
        else if (this.frames.isParameter(symbol)) {
            return new ReadParameter(loc, this.frames.getParameter(symbol));
        }
        // Then look in the closure for the symbol
        else if (this.frames.isClosure(symbol)) {
            final var slotInfo = this.frames.getClosure(symbol);
            if (this.frames.isClosureDeclared(symbol)) {
                return ReadClosureNodeGen.create(loc, slotInfo.slot(), slotInfo.isGlobal());
            } else {
                return new ReadClosureUnsafe(loc, slotInfo.slot(), symbol);
            }
        } else if (this.frames.isPrelude(symbol)) {
            return new ReadPrelude(loc, this.frames.getPrelude(symbol));
        }
        // Finally look in the LKQL built-ins
        else if (AllBuiltIns.functions().containsKey(symbol)) {
            return new ReadBuiltIn(loc, AllBuiltIns.functions().get(symbol).getLeft());
        }
        // If we're in interactive mode and the symbol hasn't been found any other way, issue a
        // ReadDynamic, which will read from the global scope. This is only necessary in
        // interactive mode.
        else if (this.source.isInteractive()) {
            return new ReadDynamic(loc, symbol);
        }

        throw translationError(loc, "Unknown symbol: " + symbol);
    }
}

//
//  Copyright (C) 2005-2026, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.exceptions;

import static com.adacore.lkql_jit.exceptions.LKQLRuntimeError.*;

import com.oracle.truffle.api.exception.AbstractTruffleException;
import com.oracle.truffle.api.source.SourceSection;
import java.io.File;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.StreamSupport;

/**
 * This exception class represents (possibly many) errors that have been catch by the static
 * analysis of some LKQL source. All errors in an instance of this class are about invalid LKQL
 * code, and not about bug during the analysis (the latter are handler by
 * {@link LKQLEngineException}).
 */
@SuppressWarnings("serial")
public final class LKQLStaticErrors extends AbstractTruffleException {

    // ----- Attributes -----

    /** All static error diagnostics registered in this exception instance. */
    public final List<StaticError> diagnostics;

    // ----- Constructors -----

    public LKQLStaticErrors() {
        this.diagnostics = new ArrayList<>();
    }

    // ----- Instance methods -----

    /** Internal helper to add a diagnostic in the exception. */
    public void addDiag(String message, SourceSection location) {
        diagnostics.add(new StaticError(message, location));
    }

    // --- Literal errors

    /** Add a diagnostic when the first character of a block-string is not a whitespace. */
    public void invalidBlockStringFirstChar(SourceSection location) {
        addDiag("Invalid blockstring: first character should be whitespace", location);
    }

    /** Add a diagnostic when an integer pattern has an invalid literal value. */
    public void invalidIntegerPattern(SourceSection location) {
        addDiag("Invalid number literal for pattern", location);
    }

    /** Add a diagnostic when a regular expression is syntactically invalid. */
    public void regexSyntaxError(String regex, SourceSection location) {
        addDiag(regexSyntaxErrorMessage(regex), location);
    }

    // --- Symbol errors

    /** Add a diagnostic when an unknown symbol is encountered. */
    public void unknownSymbol(String symbol, SourceSection location) {
        addDiag(unknownSymbolMessage(symbol), location);
    }

    /** Add a diagnostic when a symbol is already existing in a lexical environment. */
    public void alreadyExistingSymbol(String symbol, SourceSection location) {
        addDiag("Already existing symbol: " + symbol, location);
    }

    /** Add a diagnostic multiple parameters are declared with the same name. */
    public void alreadyExistingParameter(String parameterName, SourceSection location) {
        addDiag("Already existing parameter: " + parameterName, location);
    }

    /** Add a diagnostic when an invalid node kind name is encountered. */
    public void invalidKindName(SourceSection location) {
        addDiag("Invalid kind name", location);
    }

    /** Add a diagnostic when an abstract kind is encountered in a constructor call. */
    public void expectConcreteKind(SourceSection location) {
        addDiag("Expect a concrete kind", location);
    }

    /** Add a diagnostic when an invalid field is accessed. */
    public void noSuchField(SourceSection location) {
        addDiag(noSuchFieldMessage(), location);
    }

    /** Add a diagnostic when trying to fetch a struct field that doesn't exist. */
    public void noSuchFieldInStruct(String structName, String fieldName, SourceSection location) {
        addDiag("Struct \"" + structName + "\" has no field \"" + fieldName + '"', location);
    }

    /** Add a diagnostic when multiple keys have the same name in an object literal. */
    public void multipleSameNameKeys(String key, SourceSection location) {
        addDiag("Multiple keys with the same name in the object: \"" + key + "\"", location);
    }

    // --- Module errors

    /** Add a diagnostic when a module file isn't found by the LKQL engine. */
    public void moduleNotFound(String moduleName, SourceSection location) {
        addDiag("Cannot import, module not found \"" + moduleName + '"', location);
    }

    /** Add a diagnostic when an importation is ambiguous. */
    public void ambiguousImport(
        String moduleName,
        Iterable<File> possibleFiles,
        SourceSection location
    ) {
        addDiag(
            "Ambiguous importation, multiple \"" +
                moduleName +
                "\" modules found (" +
                StreamSupport.stream(possibleFiles.spliterator(), false)
                    .map(File::getAbsolutePath)
                    .collect(Collectors.joining(" & ")) +
                ")",
            location
        );
    }

    /** Add a diagnostic when a complex import is encountered. */
    public void complexImportNotSupported(SourceSection location) {
        addDiag("Only simple imports statements are supported", location);
    }

    // --- Arguments related exceptions

    /** Add a diagnostic when multiple arguments have the same name in a call. */
    public void multipleSameNameArguments(String argumentName, SourceSection location) {
        addDiag("Multiple arguments with the name \"" + argumentName + '"', location);
    }

    /** Add a diagnostic when a positional argument comes after a named one. */
    public void positionalAfterNamedArgument(SourceSection location) {
        addDiag("Positional argument after a named one", location);
    }

    /** Create an exception when a named argument re-set the value of a positional one. */
    public void namedOverlapPositional(SourceSection location) {
        addDiag("This named argument overlaps a previous positional argument", location);
    }

    /** Add a diagnostic when a call has the wrong argument arity. */
    public void wrongArity(int expected, int actual, SourceSection location) {
        addDiag(wrongArityMessage(expected, actual), location);
    }

    /** Add a diagnostic when the name of a named argument is unknown. */
    public void unknownArg(String unknown, SourceSection location) {
        addDiag(unknownArgumentMessage(unknown), location);
    }

    // ----- Inner classes -----

    /** This record represents a single diagnostic reported during analysis of an LKQL source. */
    public record StaticError(String message, SourceSection location) {}
}

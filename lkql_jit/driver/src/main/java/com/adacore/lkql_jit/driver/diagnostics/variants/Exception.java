//
//  Copyright (C) 2005-2026, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.driver.diagnostics.variants;

import com.adacore.lkql_jit.driver.source_support.SourceSection;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

/** This class represents an exception diagnostic. */
public final class Exception extends BaseDiagnostic {

    // ----- Attributes -----

    /** The kind of this exception diagnostic. */
    public final Kind kind;

    /** Stack of call that lead to the exception. */
    public final List<StackFrame> callStack;

    // ----- Constructors -----

    public Exception(Kind kind, String message, SourceSection location) {
        super(message, Optional.ofNullable(location));
        this.kind = kind;
        this.callStack = new ArrayList<>();
    }

    // ----- Instance methods -----

    public void addFrame(String contextName, SourceSection callLocation) {
        callStack.add(new CustomFrame(contextName, callLocation));
    }

    public void addFrame(StackTraceElement traceElement) {
        callStack.add(new JavaFrame(traceElement));
    }

    // ----- Inner classes -----

    /** This class is used to give information about what an exception diagnostic is reporting. */
    public static final class Kind {

        // ----- Attributes -----

        /** Human-readable name of the error kind. */
        public final String kindName;

        // ----- Constructors -----

        private Kind(String kindName) {
            this.kindName = kindName;
        }

        /** Create a new exception kind from a throwable instance. */
        public static Kind fromThrowable(Throwable t) {
            return new Kind(t.getClass().getCanonicalName());
        }

        // ----- Common error kinds -----

        /** Error kind when there is an error during the execution of an LKQL source. */
        public static final Kind LKQL_EXECUTION = new Kind("LKQL execution error");

        /** Error kind when there is an error in the LKQL engine. */
        public static final Kind LKQL_ENGINE = new Kind("LKQL engine error");
    }

    /** This class represents the abstract concept of a stack frame. */
    public abstract static sealed class StackFrame {

        /** Get an image of the location where the call has been made. */
        public abstract String locationImage();

        /** Get the name of the context the call has been made into. */
        public abstract String callContext();
    }

    /** A stack frame represented by a custom call context and a precise location. */
    public static final class CustomFrame extends StackFrame {

        /** Name of the context the call has been made into. */
        public final String contextName;

        /** Location of the call. */
        public final SourceSection callLocation;

        public CustomFrame(String contextName, SourceSection callLocation) {
            this.contextName = contextName;
            this.callLocation = callLocation;
        }

        @Override
        public String locationImage() {
            return callLocation.shortImage();
        }

        @Override
        public String callContext() {
            return contextName;
        }
    }

    /** A stack frame that come from a Java stack trace element. */
    public static final class JavaFrame extends StackFrame {

        /** The wrapped Java stack trace element. */
        public final StackTraceElement stackTraceElement;

        public JavaFrame(StackTraceElement stackTraceElement) {
            this.stackTraceElement = stackTraceElement;
        }

        @Override
        public String locationImage() {
            return (
                stackTraceElement.getFileName() +
                (stackTraceElement.getLineNumber() >= 0
                        ? ":" + stackTraceElement.getLineNumber() + ":1"
                        : "")
            );
        }

        @Override
        public String callContext() {
            return stackTraceElement.getClassName() + "." + stackTraceElement.getMethodName();
        }
    }
}

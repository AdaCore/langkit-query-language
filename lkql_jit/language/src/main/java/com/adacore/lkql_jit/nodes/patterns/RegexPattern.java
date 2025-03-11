//
//  Copyright (C) 2005-2025, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.patterns;

import com.adacore.langkit_support.LangkitSupport;
import com.adacore.lkql_jit.runtime.values.LKQLNull;
import com.adacore.lkql_jit.runtime.values.LKQLPattern;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.source.SourceSection;

/**
 * This node represents a regular expression pattern in the LKQL language.
 *
 * @author Hugo GUERRIER
 */
public abstract class RegexPattern extends ValuePattern {

    // ----- Attributes -----

    /** The regex pattern to match the node text with. */
    private final LKQLPattern pattern;

    // ----- Constructors -----

    /**
     * Create a new regex pattern node.
     *
     * @param location The location of the node in the source.
     * @param regex The regular expression string.
     */
    public RegexPattern(SourceSection location, String regex) {
        super(location);
        this.pattern = new LKQLPattern(this, regex, true);
    }

    @Specialization
    public boolean onNodeInterface(
        @SuppressWarnings("unused") VirtualFrame frame,
        LangkitSupport.NodeInterface node
    ) {
        return (node != LKQLNull.INSTANCE && this.pattern.contains(node.getText()));
    }

    @Specialization
    public boolean onString(@SuppressWarnings("unused") VirtualFrame frame, String value) {
        return pattern.contains(value);
    }

    @Fallback
    public boolean onOther(
        @SuppressWarnings("unused") VirtualFrame frame,
        @SuppressWarnings("unused") Object other
    ) {
        return false;
    }

    // ----- Override methods -----

    /**
     * @see com.adacore.lkql_jit.nodes.LKQLNode#toString(int)
     */
    @Override
    public String toString(int indentLevel) {
        return this.nodeRepresentation(
                indentLevel,
                new String[] { "pattern" },
                new Object[] { this.pattern }
            );
    }
}

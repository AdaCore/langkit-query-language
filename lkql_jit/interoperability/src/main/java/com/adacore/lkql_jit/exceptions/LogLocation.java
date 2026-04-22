//
//  Copyright (C) 2005-2026, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.exceptions;

import com.adacore.langkit_support.LangkitSupport;
import com.oracle.truffle.api.source.SourceSection;
import java.io.Serial;

/**
 * This class is a special exception that we use to provide source location information through the
 * logging process.
 */
@SuppressWarnings("serial")
public final class LogLocation extends Throwable {

    // ----- Attributes -----

    @Serial
    private static final long serialVersionUID = -408406714504200838L;

    /** The location object to precise a log message. */
    public final LocationWrapper location;

    // ----- Constructors -----

    public LogLocation(LocationWrapper location) {
        super(null, null, false, false);
        this.location = location;
    }

    // ----- Inner classes -----

    /** Abstract class that represents a location in a source. */
    public abstract static sealed class LocationWrapper {}

    public static final class LangkitLocation extends LocationWrapper {

        public final LangkitSupport.AnalysisUnit unit;
        public final LangkitSupport.SourceLocationRange locationRange;

        public LangkitLocation(
            LangkitSupport.AnalysisUnit unit,
            LangkitSupport.SourceLocationRange locationRange
        ) {
            this.unit = unit;
            this.locationRange = locationRange;
        }
    }

    public static final class TruffleLocation extends LocationWrapper {

        public final SourceSection sourceSection;

        public TruffleLocation(SourceSection sourceSection) {
            this.sourceSection = sourceSection;
        }
    }
}

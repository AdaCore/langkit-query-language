//
//  Copyright (C) 2005-2024, AdaCore
//  SPDX-License-Identifier: GPL-3.0-or-later
//

package com.adacore.lkql_jit.nodes.declarations;

import com.adacore.lkql_jit.LKQLContext;
import com.adacore.lkql_jit.LKQLLanguage;
import com.adacore.lkql_jit.built_ins.values.LKQLNamespace;
import com.adacore.lkql_jit.built_ins.values.LKQLUnit;
import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.nodes.LKQLNode;
import com.adacore.lkql_jit.utils.Constants;
import com.adacore.lkql_jit.utils.functions.FrameUtils;
import com.adacore.lkql_jit.utils.functions.StringUtils;
import com.adacore.lkql_jit.utils.source_location.SourceLocation;
import com.oracle.truffle.api.CallTarget;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.source.Source;
import java.io.File;
import java.io.IOException;
import java.util.*;

/**
 * This node represents the import statement in the LKQL language.
 *
 * @author Hugo GUERRIER
 */
public final class Import extends LKQLNode {

    // ----- Attributes -----

    /** A cache to avoid importing same module multiple times. */
    private static final Map<File, LKQLNamespace> importCache = new HashMap<>();

    /** Name of the module to import. */
    private final String name;

    /** LKQL file of the module. */
    private final File moduleFile;

    /** Slot to put the namespace in (if it's -1 the import is an internal operation rule import) */
    private final int slot;

    // ----- Constructors -----

    /**
     * Create a new import node.
     *
     * @param location The location of the node in the source.
     * @param name The name of the module to import.
     * @param slot The slot to put the namespace in.
     */
    public Import(SourceLocation location, String name, int slot) {
        super(location);
        this.name = name;
        this.slot = slot;

        // Get the module file
        this.moduleFile = this.getModuleFile();
    }

    // ----- Execution methods -----

    /**
     * @see
     *     com.adacore.lkql_jit.nodes.LKQLNode#executeGeneric(com.oracle.truffle.api.frame.VirtualFrame)
     */
    @Override
    public Object executeGeneric(VirtualFrame frame) {
        // Execute the module file
        try {
            LKQLNamespace module = this.importModule(this.moduleFile);
            // TODO: Create a new ImportInternal node to avoid this runtime check
            if (this.slot != -1) {
                FrameUtils.writeLocal(frame, this.slot, module);
            }
        } catch (IOException e) {
            throw LKQLRuntimeException.moduleNotFound(this.name, this);
        }

        // Return the unit value
        return LKQLUnit.INSTANCE;
    }

    /**
     * Get the namespace of a module from a Java file.
     *
     * @param moduleFile The module file to import.
     * @return The namespace of the module.
     * @throws IOException If Truffle cannot create a source from the file.
     */
    @CompilerDirectives.TruffleBoundary
    private LKQLNamespace importModule(File moduleFile) throws IOException {
        // If the file is already in the cache
        if (importCache.containsKey(moduleFile)) {
            return importCache.get(moduleFile);
        }

        // Else, parse the source and execute the result to get the namespace
        else {
            // Get the LKQL context
            LKQLContext context = LKQLLanguage.getContext(this);

            // Prepare the source
            Source source =
                    Source.newBuilder(
                                    Constants.LKQL_ID,
                                    context.getEnv()
                                            .getPublicTruffleFile(moduleFile.getAbsolutePath()))
                            .build();

            // Get the current context and parse the file with the internal strategy
            CallTarget target = context.getEnv().parseInternal(source);
            LKQLNamespace res = (LKQLNamespace) target.call();
            importCache.put(moduleFile, res);
            return res;
        }
    }

    // ----- Internal methods -----

    /**
     * Get a module file from the module name.
     *
     * @return The file representing the module or null.
     */
    private File getModuleFile() {
        // Create the module file name
        final String moduleFileName = this.name + Constants.LKQL_EXTENSION;

        // Search in the current directory if the location is not null
        if (this.location != null) {
            File currentModuleTry = new File(this.location.getCurrentDir(), moduleFileName);
            if (currentModuleTry.isFile() && currentModuleTry.canRead()) {
                return currentModuleTry;
            }
        }

        // Compute the directories to import from
        String lkqlPath = System.getenv().getOrDefault(Constants.LKQL_PATH, "");
        List<File> importableDirs =
                new ArrayList<>(
                        Arrays.stream(StringUtils.splitPaths(lkqlPath))
                                .filter(s -> !s.isEmpty() && !s.isBlank())
                                .map(File::new)
                                .toList());

        importableDirs.addAll(
                Arrays.stream(LKQLLanguage.getContext(this).getRuleDirectories())
                        .filter(s -> !s.isEmpty() && !s.isBlank())
                        .map(File::new)
                        .toList());

        // Search in the importable directories
        for (File dir : importableDirs) {
            if (dir != null && dir.isDirectory()) {
                File moduleTry = new File(dir, moduleFileName);
                if (moduleTry.isFile() && moduleTry.canRead()) {
                    return moduleTry;
                }
            }
        }

        // Raise an exception if the module file is not found
        throw LKQLRuntimeException.moduleNotFound(this.name, this);
    }

    // ----- Override methods -----

    /**
     * @see
     *     com.adacore.lkql_jit.nodes.LKQLNode#executeGeneric(com.oracle.truffle.api.frame.VirtualFrame)
     */
    @Override
    public String toString(int indentLevel) {
        return this.nodeRepresentation(
                indentLevel, new String[] {"name", "slot"}, new Object[] {this.name, this.slot});
    }
}

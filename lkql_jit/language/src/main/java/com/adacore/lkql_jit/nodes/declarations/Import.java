/*----------------------------------------------------------------------------
--                             L K Q L   J I T                              --
--                                                                          --
--                     Copyright (C) 2022, AdaCore                          --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
-----------------------------------------------------------------------------*/

package com.adacore.lkql_jit.nodes.declarations;

import com.adacore.lkql_jit.LKQLLanguage;
import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.nodes.LKQLNode;
import com.adacore.lkql_jit.utils.source_location.SourceLocation;
import com.oracle.truffle.api.CallTarget;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.source.Source;
import com.adacore.lkql_jit.LKQLContext;
import com.adacore.lkql_jit.runtime.values.NamespaceValue;
import com.adacore.lkql_jit.runtime.values.UnitValue;

import java.io.File;
import java.io.IOException;
import java.util.*;


/**
 * This node represents the import statement in the LKQL language
 *
 * @author Hugo GUERRIER
 */
public final class Import extends LKQLNode {

    // ----- Attributes -----

    /** A cache to avoid importing same module multiple times */
    private static final Map<File, NamespaceValue> importCache = new HashMap<>();

    /** The name of the module to import */
    private final String name;

    /** The file of the module */
    private final File moduleFile;

    /** The slot to put the namespace in (if it's -1 the import is an internal operation rule import) */
    private final int slot;

    // ----- Constructors -----

    /**
     * Create a new import node
     *
     * @param location The location of the node in the source
     * @param name The name of the module to import
     * @param slot The slot to put the namespace in
     */
    public Import(
            SourceLocation location,
            String name,
            int slot
    ) {
        super(location);
        this.name = name;
        this.slot = slot;

        // Get the module file
        this.moduleFile = this.getModuleFile();

        // Test if the module file is null
        if(this.moduleFile == null) {
            throw LKQLRuntimeException.moduleNotFound(this.name, this);
        }
    }

    // ----- Execution methods -----

    /** @see com.adacore.lkql_jit.nodes.LKQLNode#executeGeneric(com.oracle.truffle.api.frame.VirtualFrame) */
    @Override
    public Object executeGeneric(VirtualFrame frame) {
        // Execute the module file
        try {
            LKQLContext context = LKQLLanguage.getContext(this);
            NamespaceValue module = this.importModule(this.moduleFile, context);
            context.getGlobalValues().addCheckers(module);
            if(this.slot > -1) {
                context.setGlobal(this.slot, this.name, module);
            }
        } catch (IOException e) {
            throw LKQLRuntimeException.moduleNotFound(this.name, this);
        }

        // Return the unit value
        return UnitValue.getInstance();
    }

    /**
     * Get the namespace of a module from a Java file
     *
     * @param moduleFile The module file to import
     * @param context The LKQL context to import the module in
     * @return The namespace of the module
     * @throws IOException If Truffle cannot create a source from the file
     */
    @CompilerDirectives.TruffleBoundary
    private NamespaceValue importModule(File moduleFile, LKQLContext context) throws IOException {
        // If the file is already in the cache
        if(importCache.containsKey(moduleFile)) {
            return importCache.get(moduleFile);
        }

        // Else, parse the source and execute the result to get the namespace
        else {
            // Prepare the source
            Source source = Source
                    .newBuilder("lkql", context.getEnv().getPublicTruffleFile(moduleFile.getAbsolutePath()))
                    .build();

            // Get the current context and parse the file with the internal strategy
            CallTarget target = context.getEnv().parseInternal(source);
            NamespaceValue res = (NamespaceValue) context.getEnv().asHostObject(target.call());
            importCache.put(moduleFile, res);
            return res;
        }
    }

    // ----- Internal methods -----

    /**
     * Get a module file from the module name
     *
     * @return The file representing the module or null
     */
    private File getModuleFile() {
        // Create the module file name
        String moduleFileName = this.name + ".lkql";

        // Search in the current directory if the location is not null
        if(this.location != null) {
            File currentModuleTry = new File(this.location.getCurrentDir(), moduleFileName);
            if(currentModuleTry.isFile() && currentModuleTry.canRead()) {
                return currentModuleTry;
            }
        }

        // Compute the directories to import from
        String lkqlPath = System.getenv("LKQL_PATH") == null ? "" : System.getenv("LKQL_PATH");
        List<File> importableDirs = new ArrayList<>(
                Arrays.stream(lkqlPath.split(":"))
                        .filter(s -> !s.isEmpty() && !s.isBlank())
                        .map(File::new)
                        .toList()
        );

        // If the import is internal, search in rule dirs
        if(this.slot == -1) {
            importableDirs.addAll(
                    Arrays.stream(LKQLLanguage.getContext(this).getRuleDirs())
                            .filter(s -> !s.isEmpty() && !s.isBlank())
                            .map(File::new)
                            .toList()
            );
        }

        // Search in the importable directories
        for(File dir : importableDirs) {
            if(dir != null && dir.isDirectory()) {
                File moduleTry = new File(dir, moduleFileName);
                if(moduleTry.isFile() && moduleTry.canRead()) {
                    return moduleTry;
                }
            }
        }

        // Return null if the module file was not found
        return null;
    }

    // ----- Override methods -----

    /** @see com.adacore.lkql_jit.nodes.LKQLNode#executeGeneric(com.oracle.truffle.api.frame.VirtualFrame) */
    @Override
    public String toString(int indentLevel) {
        return this.nodeRepresentation(
                indentLevel,
                new String[]{"name", "slot"},
                new Object[]{this.name, this.slot}
        );
    }

}

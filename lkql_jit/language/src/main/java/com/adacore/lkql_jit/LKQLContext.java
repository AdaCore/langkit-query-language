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

package com.adacore.lkql_jit;

import com.adacore.lkql_jit.exception.LKQLRuntimeException;
import com.adacore.lkql_jit.runtime.GlobalScope;
import com.adacore.lkql_jit.runtime.built_ins.BuiltInFunctionValue;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.TruffleLanguage;
import com.adacore.libadalang.Libadalang;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;


/**
 * This class represents the execution context of an LKQL script
 *
 * @author Hugo GUERRIER
 */
public final class LKQLContext {

    // ----- Attributes -----

    /** Environment of the language */
    @CompilerDirectives.CompilationFinal
    private TruffleLanguage.Env env;

    /** The directories that contain the rules */
    @CompilerDirectives.CompilationFinal(dimensions = 1)
    private String[] ruleDirs;

    /** The global values of the LKQL execution */
    private final GlobalScope globalValues;

    // ----- Ada project attributes -----

    /** The analysis context for the ada files */
    private Libadalang.AnalysisContext adaContext;

    /** The project manager for the ada project */
    private Libadalang.ProjectManager projectManager;

    /** The ada source files */
    private final List<String> adaSourceFiles;

    /** If the source files were parsed */
    private boolean parsed;

    /** The analysis units of the parsed ada files */
    private Libadalang.AnalysisUnit[] units;

    /** The root nodes of the parsed ada files */
    private Libadalang.AdaNode[] adaNodes;

    // ----- Constructors -----

    /**
     * Create a new LKQL context
     *
     * @param env The environment
     * @param globalValues The initialized global values
     */
    public LKQLContext(
            TruffleLanguage.Env env,
            GlobalScope globalValues
    ) {
        this.env = env;
        this.globalValues = globalValues;
        this.ruleDirs = this.env.getOptions().get(LKQLLanguage.ruleDirs).replace(" ", "").split(":");
        this.adaSourceFiles = new ArrayList<>();
        this.parsed = false;
    }

    // ----- Destructors -----

    /**
     * Finalize the LKQL context to close libadalang context
     */
    public void finalizeContext() {
        this.adaContext.close();
        this.projectManager.close();
    }

    // ----- Getters -----

    public TruffleLanguage.Env getEnv() {
        return this.env;
    }

    public GlobalScope getGlobalValues() {
        return this.globalValues;
    }

    public List<String> getAdaSourceFiles() {
        return this.adaSourceFiles;
    }

    public Libadalang.AnalysisUnit[] getUnits() {
        if(!this.parsed) {
            this.parseSources();
        }
        return this.units;
    }

    public Libadalang.AdaNode[] getAdaNodes() {
        if(!this.parsed) {
            this.parseSources();
        }
        return this.adaNodes;
    }

    public String[] getRuleDirs() {
        return this.ruleDirs;
    }

    public boolean isRootContext() {
        return this.globalValues.getStackSize() == 0;
    }

    // ----- Setters -----

    public void patchContext(TruffleLanguage.Env newEnv) {
        CompilerDirectives.transferToInterpreterAndInvalidate();
        this.env = newEnv;
        this.ruleDirs = this.env.getOptions().get(LKQLLanguage.ruleDirs).replace(" ", "").split(":");
        this.initSources();
    }

    // ----- Options getting methods -----

    /**
     * Get if the language execution is in verbose mode
     *
     * @return True if the verbose flag is on
     */
    public boolean isVerbose() {
        return this.env.getOptions().get(LKQLLanguage.verbose);
    }

    /**
     * Get if the language is in checker mode
     *
     * @return True if the language is in checher mode
     */
    public boolean isChecker() {
        return this.env.getOptions().get(LKQLLanguage.checkerMode);
    }

    /**
     * Return the project file of the language context
     *
     * @return The project file in a string
     */
    public String getProjectFile() {
        return this.env.getOptions().get(LKQLLanguage.projectFile);
    }

    /**
     * Get the files to analyse
     *
     * @return The files to analyse in an array
     */
    public String[] getFiles() {
        return this.env.getOptions().get(LKQLLanguage.files).replace(" ", "").split(":");
    }

    /**
     * Get the error handling mode
     *
     * @return The mode in a string
     */
    public String getErrorMode() {
        return this.env.getOptions().get(LKQLLanguage.errorMode);
    }

    /**
     * Get the rule to run with the checker
     *
     * @return The rule to run
     */
    @CompilerDirectives.TruffleBoundary
    public String getRule() {
        return this.env.getOptions().get(LKQLLanguage.rule);
    }

    // ----- Value related methods -----

    /**
     * Get the value of a global symbol
     *
     * @param slot The slot to get
     * @return The value in the global scope
     */
    public Object getGlobal(int slot) {
        return this.globalValues.get(slot);
    }

    /**
     * Set a global value in the context
     *
     * @param slot The slot of the variable
     * @param symbol The value symbol
     * @param value The value
     */
    public void setGlobal(int slot, String symbol, Object value) {
        this.globalValues.set(slot, symbol, value);
    }

    /**
     * Get the meta table for the given type
     *
     * @param type The type to get the meta table for
     * @return The meta table for the type
     */
    public Map<String, BuiltInFunctionValue> getMetaTable(String type) {
        return this.globalValues.getMetaTable(type);
    }

    // ----- IO methods -----

    /**
     * Display the given string
     *
     * @param toPrint The string to print
     */
    @CompilerDirectives.TruffleBoundary
    public void print(String toPrint) {
        try {
            this.env.out().write(toPrint.getBytes());
        } catch (IOException e) {
            throw LKQLRuntimeException.fromMessage("Cannot print on the standard output");
        }
    }

    /**
     * Display the given string with a newline
     *
     * @param toPrint The string to print
     */
    @CompilerDirectives.TruffleBoundary
    public void println(String toPrint) {
        try {
            toPrint += "\n";
            this.env.out().write(toPrint.getBytes());
        } catch (IOException e) {
            throw LKQLRuntimeException.fromMessage("Cannot print on the standard output");
        }
    }

    // ----- Project analysis methods -----

    /**
     * Initialize the ada sources
     */
    public void initSources() {
        // Prepare the list of ada files to analyse
        this.adaSourceFiles.clear();

        // Add all files to process after verifying them
        for(String file : this.getFiles()) {
            if(!file.isEmpty() && !file.isBlank()) {
                File sourceFile = new File(file);
                if(sourceFile.isFile()) {
                    this.adaSourceFiles.add(sourceFile.getAbsolutePath());
                } else {
                    System.err.println("Source file '" + file + "' not found");
                }
            }
        }

        // Get the project file and parse it if there is one
        String projectFileName = this.getProjectFile();
        Libadalang.UnitProvider provider = null;
        if(projectFileName != null && !projectFileName.isEmpty() && !projectFileName.isBlank()) {
            // Create the project manager
            this.projectManager = Libadalang.ProjectManager.create(
                    projectFileName,
                    "",
                    "",
                    Libadalang.SourceFileMode.ROOT_PROJECT
            );
            String[] projectFiles = this.projectManager.getFiles();
            if(projectFiles.length > 0) {
                // Add all ada sources
                this.adaSourceFiles.addAll(Arrays.stream(projectFiles).toList());

                // Get the unit provider for the project
                provider = this.projectManager.getProvider();
            } else {
                System.err.println("Project file '" + projectFileName + "' not found");
            }
        }

        // Create the ada context
        this.adaContext = Libadalang.AnalysisContext.create(
                null,
                null,
                provider,
                null,
                true,
                8
        );

        // Set the parsed flag to false
        this.parsed = false;
    }

    /**
     * Parse the ada source files and store analysis units and root nodes
     */
    @CompilerDirectives.TruffleBoundary
    public void parseSources() {
        // Create the new ada nodes list
        this.units = new Libadalang.AnalysisUnit[this.adaSourceFiles.size()];
        this.adaNodes = new Libadalang.AdaNode[this.adaSourceFiles.size()];

        // For each source file, add its parsing result to the roots
        for(int i = 0 ; i < this.adaSourceFiles.size() ; i++) {
            this.units[i] = this.adaContext.getUnitFromFile(this.adaSourceFiles.get(i));
            this.adaNodes[i] = this.units[i].root();
        }

        // Set the parsed flag to true
        this.parsed = true;
    }

}

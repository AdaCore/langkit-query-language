package com.adacore.lkql_jit.drivers;

import picocli.CommandLine;

import java.util.ArrayList;
import java.util.List;

public class LKQLRefactor {

    @CommandLine.Spec public picocli.CommandLine.Model.CommandSpec spec;

    @CommandLine.Parameters(description = "LKQL files to refactor")
    public List<String> files = new ArrayList<>();

    @CommandLine.Option(
        names = {"-r", "--refactoring"},
        description = "Refactoring to run")
    public String refactoringName;
}
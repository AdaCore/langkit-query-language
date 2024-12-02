import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.List;

import com.adacore.liblkqllang.Liblkqllang.*;

/** Helper driver to test LKQL env specs. */
public class EnvSpecsDriver {
    /** LKQL script to analyze. */
    private static String scriptBuffer = null;

    public static void main(String[] args) {
        String scriptFile = args[0];
        try {
            scriptBuffer = new String(Files.readAllBytes(Paths.get(scriptFile)));
            showRefDecls();
        } catch (Exception e) {
            System.out.println("Cannot read the provided LKQL file: \"%s\"".formatted(scriptFile));
            e.printStackTrace();
        }
    }

    private static void showRefDecls() {
        AnalysisContext ctx = AnalysisContext.create();
        AnalysisUnit unit = ctx.getUnitFromBuffer(scriptBuffer, "script.lkql");

        // Display parsing error if there are some
        Diagnostic[] diags = unit.getDiagnostics();
        if (diags.length > 0) {
            System.out.println("Parsing errors in script.lkql:\n");
            for (Diagnostic d : diags) {
                System.out.println("  " + d.toString());
            }
            return;
        }

        // For each identifier, display its referenced declaration
        showRefDecl(unit.getRoot());
    }

    private static void showRefDecl(LkqlNode n) {
        if (n instanceof Identifier id) {
            LkqlNode refDecl = id.pReferencedDecl();

            System.out.println("=====");
            System.out.println("%sidentifier:".formatted(id.fullSlocImage()));
            System.out.println(codeSnippet(id.getSourceLocationRange()));
            if (refDecl.isNone()) {
                System.out.println("(no referenced declaration)");
            } else {
                System.out.println("%sreferenced declaration %s:".formatted(refDecl.fullSlocImage(), refDecl.toString()));
                System.out.println(codeSnippet(refDecl.getSourceLocationRange()));
            }
            System.out.println("=====");
            System.out.println("");
        }
        for (LkqlNode c : n.children()) {
            if (!c.isNone()) showRefDecl(c);
        }
    }

    /**
     * Util function to get the textual representation of a source code
     * location range by the way of a code snippet.
     */
    private static String codeSnippet(SourceLocationRange slocr) {
        StringBuilder res = new StringBuilder();
        List<String> lines = scriptBuffer.lines().toList();
        int numSize = String.valueOf(slocr.end.line).length();
        String numColFmt = "%" + numSize + "d";

        if (slocr.start.line == slocr.end.line) {
            res.append(String.format(numColFmt, slocr.start.line))
                .append(" | ")
                .append(lines.get(slocr.start.line - 1))
                .append('\n')
                .append(" ".repeat(numSize))
                .append(" | ")
                .append(" ".repeat(slocr.start.column - 1))
                .append("^".repeat(slocr.end.column - slocr.start.column));
        } else {
            res.append(String.format(numColFmt, slocr.start.line))
                .append(" |  ")
                .append(lines.get(slocr.start.line - 1))
                .append('\n')
                .append(" ".repeat(numSize))
                .append(" | ")
                .append("_".repeat(slocr.start.column))
                .append("^\n");
            for (int i = slocr.start.line; i < slocr.end.line; i++) {
                res.append(String.format(numColFmt, i + 1))
                    .append(" || ")
                    .append(lines.get(i))
                    .append('\n');
            }
            res.append(" ".repeat(numSize))
                .append(" ||_")
                .append("_".repeat(slocr.end.column - 2))
                .append('^');
        }
        return res.toString();
    }
}

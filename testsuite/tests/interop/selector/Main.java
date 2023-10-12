import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Value;

public class Main {
    private static final String LKQL_SOURCE =
        """
        val node = units()[1].root

        selector my_selector
          | AdaNode => rec *this.children
          | *       => ()
        """;

    private static void print(String messageName, Object value) {
        System.out.println("  " + messageName + ": " + value);
    }

    public static void main(String[] args) {
        Context context = Context.newBuilder("lkql")
                                 .option("lkql.projectFile", "default_project/default.gpr")
                                 .build();
        Value executable = context.parse("lkql", LKQL_SOURCE);

        Value namespace = executable.execute(false);
        Value selector = namespace.getMember("my_selector");
        System.out.println("=== Selector interop messages:");
        print("toString()", selector.toString());
        print("canExecute()", selector.canExecute());
        // TODO (issue #143): Call the selector when it will implements
        // the interop API
    }
}

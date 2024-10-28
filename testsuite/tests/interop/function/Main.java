import com.adacore.lkql_jit.options.LKQLOptions;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Value;

public class Main {
    private static final String LKQL_SOURCE =
        """
        fun my_function(x) = x * 2
        """;

    private static void print(String messageName, Object value) {
        System.out.println("  " + messageName + ": " + value);
    }

    public static void main(String[] args) {
        Context context = Context
            .newBuilder("lkql")
            .option(
                "lkql.options", new LKQLOptions.Builder()
                    .projectFile("default_project/default.gpr")
                    .build()
                    .toJson()
                    .toString()
            ).build();
        Value executable = context.parse("lkql", LKQL_SOURCE);

        Value namespace = executable.execute(false);
        Value function = namespace.getMember("my_function");
        System.out.println("=== Function interop messages:");
        print("toString()", function.toString());
        print("canExecute()", function.canExecute());
        // TODO (issue #143): Remove the "null" argument when function will
        // handle closure itself
        print("execute(5)", function.execute(null, 5l));
    }
}

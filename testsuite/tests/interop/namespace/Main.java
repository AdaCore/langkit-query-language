import com.adacore.lkql_jit.options.LKQLOptions;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Value;

public class Main {
    private static final String LKQL_SOURCE =
        """
        val x = 42
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
        System.out.println("=== Namespace interop messages:");
        print("toString()", namespace.toString());
        print("hasMembers()", namespace.hasMembers());
        print("hasMember('x')", namespace.hasMember("x"));
        print("hasMember('y')", namespace.hasMember("y"));
        print("getMember('x')", namespace.getMember("x"));
        print("getMember('y')", namespace.getMember("y"));
        print("getMemberKeys()", namespace.getMemberKeys());
    }
}

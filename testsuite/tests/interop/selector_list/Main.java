import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Value;

public class Main {
    private static final String LKQL_SOURCE =
        """
        val nodes = children(units()[1].root)
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
        Value nodes = namespace.getMember("nodes");
        System.out.println("=== Selector list interop messages:");
        print("toString()", nodes.toString());
        print("hasArrayElements()", nodes.hasArrayElements());
        print("getArraySize()", nodes.getArraySize());
        print("isBoolean()", nodes.isBoolean());
        print("asBoolean()", nodes.asBoolean());
        // TODO (LATER IN MR): Get first elements of the list when depth node are interop
        print("hasIterator()", nodes.hasIterator());
    }
}

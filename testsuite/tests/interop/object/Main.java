import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Value;

public class Main {
    private static final String LKQL_SOURCE =
        """
        val object = {x: 42, y: "Hello"}
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
        Value object = namespace.getMember("object");
        System.out.println("=== Object interop messages:");
        print("toString()", object.toString());
        print("hasMembers()", object.hasMembers());
        print("hasMember('x')", object.hasMember("x"));
        print("hasMember('y')", object.hasMember("y"));
        print("hasMember('z')", object.hasMember("z"));
        print("getMember('x')", object.getMember("x"));
        print("getMember('y')", object.getMember("y"));
        print("getMember('z')", object.getMember("z"));
        // TODO (LATER IN MR): Call "getMemberKeys" when list values are interop
    }
}

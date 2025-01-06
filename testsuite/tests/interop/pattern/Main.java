import com.adacore.lkql_jit.options.LKQLOptions;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Value;

public class Main {
    private static final String LKQL_SOURCE =
        """
        val my_pattern = pattern("he..o")
        """;

    private static void print(String messageName, Object value) {
        System.out.println("  " + messageName + ": " + value);
    }

    public static void main(String[] args) {
        Context context = Context.newBuilder("lkql").build();
        Value executable = context.parse("lkql", LKQL_SOURCE);

        Value namespace = executable.execute(false);
        Value pattern = namespace.getMember("my_pattern");
        System.out.println("=== Property interop messages:");
        print("toString()", pattern.toString());
        print("hasMembers()", pattern.hasMembers());
        print("getMemberKeys()", pattern.getMemberKeys());
        print("canInvokeMember('contains')", pattern.canInvokeMember("contains"));
        print("canInvokeMember('find')", pattern.canInvokeMember("find"));
        print("invokeMember('contains', 'hello world')", pattern.invokeMember("contains", "hello world"));
        print("invokeMember('contains', 'good morning world')", pattern.invokeMember("contains", "good morning world"));
        print("invokeMember('find', 'this is hello world')", pattern.invokeMember("find", "this is hello world"));
    }
}

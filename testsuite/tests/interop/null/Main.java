import com.adacore.lkql_jit.options.LKQLOptions;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Value;

public class Main {
    private static final String LKQL_SOURCE =
        """
        val my_null = null
        """;

    private static void print(String messageName, Object value) {
        System.out.println("  " + messageName + ": " + value);
    }

    public static void main(String[] args) {
        Context context = Context.newBuilder("lkql").build();
        Value executable = context.parse("lkql", LKQL_SOURCE);

        Value namespace = executable.execute(false);
        Value myNull = namespace.getMember("my_null");
        System.out.println("=== Null interop messages:");
        print("toString()", myNull.toString());
        print("isNull()", myNull.isNull());
        print("isBoolean()", myNull.isBoolean());
        print("asBoolean()", myNull.asBoolean());
    }
}

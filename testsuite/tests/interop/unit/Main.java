import com.adacore.lkql_jit.LKQLOptions;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Value;

public class Main {
    private static final String LKQL_SOURCE =
        """
        val unit = ()
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
        Value unit = namespace.getMember("unit");
        System.out.println("=== Unit interop messages:");
        print("toString()", unit.toString());
        print("isNull()", unit.isNull());
        print("isBoolean()", unit.isBoolean());
        print("asBoolean()", unit.asBoolean());
    }
}

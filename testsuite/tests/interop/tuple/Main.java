import com.adacore.lkql_jit.options.LKQLOptions;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Value;

public class Main {
    private static final String LKQL_SOURCE =
        """
        val tuple = (1, "Coucou", (1, 2))
        """;

    private static void print(String messageName, Object value) {
        System.out.println("  " + messageName + ": " + value);
    }

    public static void main(String[] args) {
        Context context = Context.newBuilder("lkql").build();
        Value executable = context.parse("lkql", LKQL_SOURCE);

        Value namespace = executable.execute(false);
        Value tuple = namespace.getMember("tuple");
        System.out.println("=== Tuple interop messages:");
        print("toString()", tuple.toString());
        print("hasArrayElements()", tuple.hasArrayElements());
        print("getArraySize()", tuple.getArraySize());
        print("getArrayElement(0)", tuple.getArrayElement(0));
        print("getArrayElement(1)", tuple.getArrayElement(1));
        print("getArrayElement(2)", tuple.getArrayElement(2));
    }
}

import com.adacore.lkql_jit.LKQLOptions;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Value;

public class Main {
    private static final String LKQL_SOURCE =
        """
        val list = [1, "Hello", [1, 2]]
        val empty_list = []
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
        Value list = namespace.getMember("list");
        Value emptyList = namespace.getMember("empty_list");
        System.out.println("=== List interop messages:");
        print("toString()", list.toString());
        print("hasArrayElements()", list.hasArrayElements());
        print("getArraySize()", list.getArraySize());
        print("isBoolean()", list.isBoolean());
        print("isBoolean() (empty list)", emptyList.isBoolean());
        print("asBoolean()", list.asBoolean());
        print("asBoolean() (empty list)", emptyList.asBoolean());
        print("getArrayElement(0)", list.getArrayElement(0));
        print("getArrayElement(1)", list.getArrayElement(1));
        print("getArrayElement(2)", list.getArrayElement(2));
        print("hasIterator()", list.hasIterator());
    }
}

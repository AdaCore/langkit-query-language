import com.adacore.lkql_jit.LKQLOptions;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Value;

public class Main {
    private static final String LKQL_SOURCE =
        """
        val lazy_list = [x for x in [1, \"Coucou\", [4, 5]]]
        val empty_list = [x for x in [5] if x != 5]
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
        Value lazyList = namespace.getMember("lazy_list");
        Value emptyList = namespace.getMember("empty_list");
        System.out.println("=== Lazy list interop messages:");
        print("toString()", lazyList.toString());
        print("hasArrayElements()", lazyList.hasArrayElements());
        print("getArraySize()", lazyList.getArraySize());
        print("isBoolean()", lazyList.isBoolean());
        print("isBoolean() (empty lazy list)", emptyList.isBoolean());
        print("asBoolean()", lazyList.asBoolean());
        print("asBoolean() (empty lazy list)", emptyList.asBoolean());
        print("getArrayElement(0)", lazyList.getArrayElement(0));
        print("getArrayElement(1)", lazyList.getArrayElement(1));
        print("getArrayElement(2)", lazyList.getArrayElement(2));
        print("hasIterator()", lazyList.hasIterator());
    }
}

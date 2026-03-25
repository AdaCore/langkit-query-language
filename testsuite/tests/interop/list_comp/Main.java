import com.adacore.lkql_jit.options.LKQLOptions;
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
        Context context = Context.newBuilder("lkql").build();
        Value executable = context.parse("lkql", LKQL_SOURCE);

        Value namespace = executable.execute(false);
        Value lazyList = namespace.getMember("lazy_list");
        Value emptyList = namespace.getMember("empty_list");
        Value iterator = lazyList.getIterator();
        System.out.println("=== Lazy list interop messages:");
        print("toString()", lazyList.toString());
        print("hasArrayElements()", lazyList.hasArrayElements());
        print("isBoolean()", lazyList.isBoolean());
        print("isBoolean() (empty lazy list)", emptyList.isBoolean());
        print("asBoolean()", lazyList.asBoolean());
        print("asBoolean() (empty lazy list)", emptyList.asBoolean());
        print("hasIterator()", lazyList.hasIterator());
        while (iterator.hasIteratorNextElement()) {
            Value elem = iterator.getIteratorNextElement();
            System.out.println("    - " + elem.toString());
        }
    }
}

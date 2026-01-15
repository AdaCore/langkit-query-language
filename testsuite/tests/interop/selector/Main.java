import com.adacore.lkql_jit.options.LKQLOptions;
import com.adacore.lkql_jit.values.interop.LKQLBaseNamespace;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Value;

public class Main {
    private static final String LKQL_SOURCE =
        """
        val node = units()[1].root

        selector my_selector
          | AdaNode => rec(*this.children)
          | *       => ()
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

        LKQLBaseNamespace namespace = executable.execute(false).as(LKQLBaseNamespace.class);
        Object node = namespace.getUncached("node");
        Value selector = context.asValue(namespace.getUncached("my_selector"));
        System.out.println("=== Selector interop messages:");
        print("toString()", selector.toString());
        print("canExecute()", selector.canExecute());
        Value callRes = selector.execute(node);
        long callResSize = callRes.getArraySize();
        print("execute(node)", callRes);
        print("callRes.getArraySize()", callResSize);
        for (long i = 0; i < callResSize; i++) {
            print("callRes.getArrayElement(" + i + ")", callRes.getArrayElement(i));
        }
    }
}

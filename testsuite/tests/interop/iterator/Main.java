import com.adacore.lkql_jit.options.LKQLOptions;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Value;

public class Main {
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
        Value executable = context.parse("lkql", "val list = [1, \"Hello\", [1, 2]]");

        Value namespace = executable.execute(false);
        Value list = namespace.getMember("list");
        Value iterator = list.getIterator();
        System.out.println("=== List interop messages:");
        print("toString() (list)", list.toString());
        print("hasIterator()", list.hasIterator());
        print("isIterator()", iterator.isIterator());
        print("hasIteratorNextElement()", iterator.hasIteratorNextElement());
        System.out.println("  iterator elements:");
        while (iterator.hasIteratorNextElement()) {
            Value elem = iterator.getIteratorNextElement();
            System.out.println("    - " + elem.toString());
        }
    }
}

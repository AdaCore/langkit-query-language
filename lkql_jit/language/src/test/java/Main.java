import org.graalvm.polyglot.Context;

public class Main {

    public static final String ASPECT_DECLS = "/home/guerrier/Documents/AdaCore/langkit-query-language/testsuite/ada_projects/generic_inst/default.gpr";

    public static void main(String[] args) {
        try(Context context = Context.newBuilder("lkql")
                .option("lkql.projectFile", ASPECT_DECLS)
                .option("lkql.verbose", "false")
                .build()) {
            long start = System.currentTimeMillis();
            context.eval(
                    "lkql",
                            """
                                    val unit = units()[1]
                                    
                                    print(unit.tokens)
                                    """

            );
            long end = System.currentTimeMillis();
            System.out.println("Execution time " + ((double) end - start) / 1000);
        }
    }

}

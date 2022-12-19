import org.graalvm.polyglot.Context;

public class Main {

    public static final String DEFAULT = "/home/guerrier/Documents/AdaCore/langkit-query-language/testsuite/ada_projects/default_project/default.gpr";

    public static final String ASPECT_DECLS = "/home/guerrier/Documents/AdaCore/langkit-query-language/testsuite/ada_projects/generic_inst/default.gpr";

    public static final String KP = "/home/guerrier/Documents/AdaCore/langkit-query-language/testsuite/ada_projects/kp_R517_023_simple_testsuite/prj.gpr";

    public static void main(String[] args) {
        try(Context context = Context.newBuilder("lkql")
                .option("lkql.projectFile", DEFAULT)
                .option("lkql.verbose", "false")
                .build()) {
            long start = System.currentTimeMillis();
            context.eval(
                    "lkql",
                            """
                                    print("COUCOU")
                                    """

            );
            long end = System.currentTimeMillis();
            System.out.println("Execution time " + ((double) end - start) / 1000);
        } catch (Exception e) {
            System.out.println(e.getMessage());
        }
    }

}

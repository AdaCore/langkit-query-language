import com.adacore.libadalang.Libadalang;
import org.graalvm.polyglot.Context;

public class Main {

    public static final String DEFAULT = "/home/guerrier/Documents/AdaCore/langkit-query-language/testsuite/ada_projects/default_project/default.gpr";

    public static final String ACCESS_DECLS = "/home/guerrier/Documents/AdaCore/langkit-query-language/testsuite/ada_projects/access_decls/access_decls.gpr";

    public static final String ASPECT_DECLS = "/home/guerrier/Documents/AdaCore/langkit-query-language/testsuite/ada_projects/generic_inst/default.gpr";

    public static final String NESTED_GEN = "/home/guerrier/Documents/AdaCore/langkit-query-language/testsuite/ada_projects/nested_generics/nested_generics.gpr";

    public static final String TEST = "/home/guerrier/Documents/AdaCore/langkit-query-language/testsuite/tests/checks/parameters_aliasing/prj.gpr";

    public static void main(String[] args) {
        try(Context context = Context.newBuilder("lkql")
                .option("lkql.projectFile", TEST)
                .option("lkql.verbose", "false")
                .option("lkql.checkerMode", "false")
                .allowIO(true)
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
            e.printStackTrace();
        }
    }

}

import org.graalvm.polyglot.Context;

public class Main {

    public static final String ASPECT_DECLS = "/home/guerrier/Documents/AdaCore/langkit-query-language/testsuite/ada_projects/generic_inst/default.gpr";

    public static void main(String[] args) {
        try(Context context = Context.newBuilder("lkql")
                .option("lkql.projectFile", ASPECT_DECLS)
                .build()) {
            long start = System.currentTimeMillis();
            context.eval(
                    "lkql",
                    """
                    fun fibo(x) =
                        if (x == 0)
                        then 0
                        else if(x == 1)
                            then 1
                            else fibo(x - 1) + fibo(x - 2)
                                                
                    print(fibo(32))
                    """
            );
            long end = System.currentTimeMillis();
            System.out.println("Execution time " + ((double) end - start) / 1000);
        }
    }

}

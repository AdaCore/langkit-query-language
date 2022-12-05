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
                                    val stdlib = ()
                                    val outside_refs = ()
                                    
                                    @unit_check(message="outside references from subprogram",
                                                category="Style", subcategory="Program Structure")
                                    fun outside_references_from_subprograms(unit) = [
                                        {message: "outside references from subprogram", loc: n}
                                        for n in concat([outside_refs(body)
                                                         for body in from unit.root through follow_generics
                                                         select subp@BaseSubpBody
                                                         when not stdlib.in_generic_template(subp)].to_list)]
                                    """

            );
            long end = System.currentTimeMillis();
            System.out.println("Execution time " + ((double) end - start) / 1000);
        }
    }

}

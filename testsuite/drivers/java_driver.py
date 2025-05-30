import os

from drivers.base_driver import BaseDriver
from e3.testsuite.driver.classic import TestAbortWithFailure


class JavaDriver(BaseDriver):
    """
    This driver runs the provided Java application in the GraalVM with a direct
    access to the LKQL JIT Truffle language. This driver is mainly used to
    test the LKQL Truffle interoperability features.

    The Java application to run must be placed into a file named `Main.java`
    in the test directory.
    """

    perf_supported = False
    flag_checking_supported = False

    def run(self) -> None:
        # Get and check the test Java main file
        main_java_file = self.working_dir("Main.java")
        if not os.path.isfile(main_java_file):
            raise TestAbortWithFailure("Missing 'Main.java' file")

        # Get the needed environment variable
        graal_home = os.environ["GRAAL_HOME"]
        lkql_jit_home = os.environ.get(
            "LKQL_JIT_HOME", os.path.join(graal_home, "languages", "lkql")
        )

        # Get the GraalVM Java executable
        java = (
            os.path.join(graal_home, "bin", "java.exe")
            if os.name == "nt"
            else os.path.join(graal_home, "bin", "java")
        )

        # Create the class path
        class_path = os.pathsep.join(
            [
                os.path.join(graal_home, "lib", "truffle", "truffle-api.jar"),
                os.path.join(lkql_jit_home, "lkql_jit.jar"),
            ]
        )

        # Create the value for java.library.path property
        java_library_path = (
            os.environ.get("PATH", "")
            if os.name == "nt"
            else os.environ.get("LD_LIBRARY_PATH", "")
        )

        # Run Java with the main file
        self.check_run(
            [
                java,
                "-cp",
                class_path,
                f"-Djava.library.path={java_library_path}",
                f"-Dtruffle.class.path.append={os.path.join(lkql_jit_home, 'lkql_jit.jar')}",
                main_java_file,
            ]
        )

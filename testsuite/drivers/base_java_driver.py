import os

from drivers.base_driver import BaseDriver


class BaseJavaDriver(BaseDriver):
    """
    This driver is the base of all drivers that need to run a Java application
    in the context of the LKQL JIT library.
    """

    def java(self, *args) -> None:
        """
        Run the GraalVM Java executable with the provided arguments. Resulting
        output is added to the test case output.
        """
        # Get the needed environment variable
        graal_home = os.environ["GRAAL_HOME"]
        lkql_jit_home = os.environ.get(
            "LKQL_JIT_HOME", os.path.join(graal_home, "languages", "lkql")
        )

        # Get the GraalVM Java executable
        java_exe = (
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
                java_exe,
                "-cp",
                class_path,
                f"-Djava.library.path={java_library_path}",
                f"-Dtruffle.class.path.append={os.path.join(lkql_jit_home, 'lkql_jit.jar')}",
                '--add-opens=org.graalvm.truffle/com.oracle.truffle.api.strings=ALL-UNNAMED',
                *args,
            ]
        )

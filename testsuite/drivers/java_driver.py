from drivers.base_java_driver import BaseJavaDriver


class JavaDriver(BaseJavaDriver):
    """
    This driver runs the provided Java application in the GraalVM with a direct
    access to the LKQL JIT Truffle language. This driver is mainly used to
    test the LKQL Truffle interoperability features.

    The Java application to run must be placed into a file named `Main.java`
    in the test directory.
    """

    @property
    def required_files(self) -> list[str]:
        return ["Main.java"]

    def run(self) -> None:
        self.java(self.working_dir("Main.java"))

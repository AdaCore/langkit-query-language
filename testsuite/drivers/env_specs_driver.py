import os

from drivers.base_java_driver import BaseJavaDriver


class EnvSpecsDriver(BaseJavaDriver):
    """
    Parse the "script.lkql" file in the current working directory and for
    each identifier, display its referenced declaration.
    """
    @property
    def required_files(self) -> list[str]:
        return ["script.lkql"]

    def run(self) -> None:
        self.java(
            os.path.join(self.env.java_support, "EnvSpecsDriver.java"),
            self.working_dir("script.lkql"),
        )

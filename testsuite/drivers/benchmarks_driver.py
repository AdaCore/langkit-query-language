import json
import os

from drivers.base_driver import BaseDriver


class BenchmarksDriver(BaseDriver):
    """
    This driver runs the LKQL JIT benchmarks using the Maven JMH entry point.
    This driver doens't compare any output and only report the result of the
    benchmarks execution.
    """

    perf_supported = True
    flag_checking_supported = False

    @property
    def baseline(self) -> tuple[str, str, bool]:
        # This driver never uses the baselines.
        return (None, "", False)

    def run(self):
        # Call the Maven command to run the JMH benchmarking
        benchmark_results_json = self.working_dir('benchmark_result.json')
        mvn_cmd = [
            "mvn",
            "-f", os.path.join(self.lkql_jit_dir, "benchmarks"),
            "jmh:benchmark",
            "-Djmh.rf=json",
            f"-Djmh.v=SILENT",
            f"-Djmh.rff={benchmark_results_json}",
        ]
        self.shell(
            mvn_cmd,
            analyze_output=False,
        )

        # Read the benchmark result and parse it
        res = {}
        with open(benchmark_results_json, 'r') as json_file:
            benchmark_results = json.load(json_file)
            for result in benchmark_results:
                # Compute the interesting information about the benchmark
                split = result['benchmark'].split(".")
                benchmark = ".".join(split[:-1])
                run = split[-1]

                # Place them in the result
                runs = res.get(benchmark, {})
                runs[run] = {
                    'score': result['primaryMetric']['score'],
                    'unit': result['primaryMetric']['scoreUnit'],
                }
                res[benchmark] = runs

        self.result.info['benchmark_results'] = res

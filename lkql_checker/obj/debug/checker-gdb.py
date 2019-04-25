import os
import sys

dir_path = os.path.dirname(os.path.realpath(__file__))
printers_path = os.path.join(dir_path, "..", "..", "..")

sys.path.append(printers_path)

import printers
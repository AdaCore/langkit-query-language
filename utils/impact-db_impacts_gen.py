#!/usr/bin/env python3

# This script should be run at the top level of the langkit-query-language.

import glob
import inspect
import json
import os
from collections import OrderedDict
from pathlib import Path
from impactdb import db

res = {}


def format_impacts(impacts: [str]) -> str:
    gnat = list(db.config["gnat"].keys())
    return ",".join(list(map(lambda i: i + ".*" if i in gnat else i, impacts)))


def format_name(name: str) -> str:
    return "kp_" + name.lower().replace("-", "_")


def list_impacts(issues: [str]) -> None:
    entries = os.path.join(os.path.dirname(inspect.getfile(db)), "..", "entries")
    entries = [e for e in db.load(entries) if e.type == "kp"]
    for e in entries:
        if e.name in issues:
            res.update({format_name(e.name): format_impacts(e.impacts)})
        elif e.origin in issues:
            res.update({format_name(e.origin): format_impacts(e.impacts)})


kps = [kp for kp in glob.glob("./lkql_checker/share/lkql/kp/KP-*")]
ids = [Path(id).stem[3:] for id in kps]
list_impacts(sorted(ids))

impacts = {"impacts": OrderedDict(sorted(res.items()))}
impacts.update({"gnat": sorted(db.config["gnat"].keys())})

with open("./lkql_checker/share/lkql/kp/kp.json", "w", encoding="utf-8") as f:
    json.dump(impacts, f, ensure_ascii=False, indent=4)
    f.write("\n")

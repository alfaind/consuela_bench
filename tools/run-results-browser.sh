#!/bin/bash

ENUM_CSV_SCRIPT='import glob, json; print json.dumps(glob.glob("*_latencies.csv"))'

mkdir -p /results
cd /results
cp -rv /src/* /tools/results-browser/* /tools/results-browser.py ./
python -c "${ENUM_CSV_SCRIPT}" > web/data.json
exec python results-browser.py --host 0.0.0.0 --port 8080

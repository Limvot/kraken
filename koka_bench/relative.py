#!/usr/bin/env python3
import sys
with open(sys.argv[1], "r") as f:
    csv = [ [ v.strip() for v in l.split(',') ] for l in f.readlines() ]
csv[0] = csv[0] + [ 'relative' ]
min = min( float(r[1]) for r in csv[1:] )
subset = csv[1:]
for i in range(len(subset)):
    subset[i] = subset[i] + [ float(subset[i][1]) / min ]
csv[1:] = sorted(subset, key=lambda x: x[8])

out = "\n".join(",".join(str(x) for x in r) for r in csv)
with open(sys.argv[1] + "with_relative.csv", "w") as f:
    f.write(out)

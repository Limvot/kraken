#!/usr/bin/env python3

import sys
import matplotlib.pyplot as plt
import numpy as np
import math

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

print(csv)
csv = [ x for x in csv if 'slow' in x[0] or 'rbtree' not in x[0] or 'kraken' not in x[0] or 'opt' in x[0] ]
print(csv)

def make_name(n):
    replace_dict = {"kk": "Koka", "cpp": "C++", "ml": "ML", "hs": "Haskell", "sw": "Swift", "wavm": "WAVM"}
    out  = " ".join(replace_dict.get(word, word.title()) for word in n.split('/')[-1]\
                                                              .split(' ')[0]\
                                                              .replace("-", " ").split(" ")\
                    if word not in {"rbtree"})
    if "java" in n:
        out = "Java"
    print(f"changed {n} to {out}")
    return out
names    = [ make_name(x[0]) for x in csv[1:] ]
benchmark_size = csv[1][0].split('/')[-1].split(' ')[1]
times    = [ float(x[1]) for x in csv[1:] ]
relative = [ float(x[8]) for x in csv[1:] ]
print(names)
print(times)
print(relative)
out_name = " ".join(sys.argv[1].removesuffix('.csv')\
                               .replace("_", " ").title()\
                               .replace("Rbtree", "RB-Tree")\
                               .split(" ")[:-1] + [benchmark_size])

n_groups = len(names)
a = times
d = names

for do_log in [False, True]:
    fig, ax = plt.subplots()
    index = np.arange(n_groups)
    bar_width = 0.4
    opacity = 0.9

    ax.set_facecolor('gainsboro')
    rects1 = plt.bar(index, a, bar_width, alpha=opacity, color='orange')
    for k_index  in (i for i in range(len(d)) if 'Kraken' in d[i]):
        rects1[k_index].set_color('r')
    plt.xlabel(f"{out_name} Benchmark" + (" (Log Scale)" if do_log else ""))
    plt.ylabel('Runtime (s)' + (" (Log Scale)" if do_log else ""))
    ax.ticklabel_format(useOffset=False, style='plain')
    plt.xticks(index, d, rotation=0)
    plt.legend()

    plt.tight_layout()
    plt.xticks(rotation = 45)
    if do_log:
        plt.subplots_adjust(left=0.10)
        plt.semilogy()
    plt.subplots_adjust(bottom=0.32)
    plt.savefig(f"{sys.argv[1]}_{'log' if do_log else ''}.png", dpi = 96 * 2 * 2)
    #plt.show()

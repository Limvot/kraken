#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""

Created on Mon Apr 18 18:47:57 2022
Modified Wednesday July 7th

@author: bodhisatwa, Nathan Braswell

"""

import matplotlib.pyplot as plt
import numpy as np
import math

def plot_graph3():
     n_groups = 10
     a = [0.02,0.04,0.04,1.15,2.21,1.01,1.58,0.92,0.21,0.39]
     d = ["Single Pointer","Simple Loop","Loop Nest","519.lbm","505.mcf","525.x264","bzip2","gzip","grep","tar"]

     fig, ax = plt.subplots()
     index = np.arange(n_groups)
     bar_width = 0.4
     opacity = 0.9

     ax.set_facecolor('gainsboro')
     #ax.set_facecolor((1.0, 0.47, 0.42))

     rects1 = plt.bar(index, a, bar_width, alpha=opacity, color='orange')

     plt.xlabel('Benchmarks')
     #plt.xlabel('Mix #')

     plt.ylabel('Improvement in Execution Time (%)')
     #plt.title("Performance Improvement of Parallelization")
     #plt.title("Mix$_{16}$ execution time normalized to original time")

     ax.ticklabel_format(useOffset=False, style='plain')
     plt.xticks(index, d, rotation=0)
     plt.legend()

     plt.tight_layout()
     plt.xticks(rotation = 45)
     plt.subplots_adjust(bottom=0.25)
     plt.savefig("performance-backend.png", dpi = 96 * 2 * 2)
     plt.show()

def plot_graph2():

     n_groups = 10
     a = [0.02,0.03,0.21,2.97,4.01,1.63,3.16,2.62,0.92,0.64]
     d = ["Single Pointer","Simple Loop","Loop Nest","519.lbm","505.mcf","525.x264","bzip2","gzip","grep","tar"]

     fig, ax = plt.subplots()
     index = np.arange(n_groups)
     bar_width = 0.4
     opacity = 0.9

     ax.set_facecolor('gainsboro')
     #ax.set_facecolor((1.0, 0.47, 0.42))

     rects1 = plt.bar(index, a, bar_width, alpha=opacity, color='orange')

     plt.xlabel('Benchmarks')
     #plt.xlabel('Mix #')
     plt.ylabel('Binary Size Reduction (%)')
     #plt.title("Performance Improvement of Parallelization")
     #plt.title("Mix$_{16}$ execution time normalized to original time")

     ax.ticklabel_format(useOffset=False, style='plain')
     plt.xticks(index, d, rotation=0)
     plt.legend()

     plt.tight_layout()
     plt.xticks(rotation = 45)
     plt.subplots_adjust(bottom=0.25)
     plt.savefig("size_reduction.png", dpi = 96 * 2 * 2)
     plt.show()

def plot_graph():
     n_groups = 6
     a = [3.383796596,44.3223119,0.467799154,8.831168831,26.36934002,4.855710338]
     b = [10.04489527,5.241043846,0.4112731088,6.908163265,3.153844655,10.67011095]
     d = ['Adi', 'Fdtd-2D', 'Heat-3D', "Jacobi-1D", "Jacobi-2D", "Seidel-2D"]

     fig, ax = plt.subplots()
     index = np.arange(n_groups)
     bar_width = 0.4
     opacity = 0.9

     ax.set_facecolor('gainsboro')

     #ax.set_facecolor((1.0, 0.47, 0.42))
     rects1 = plt.bar(index-0.2, a, bar_width, alpha=opacity, color='orange', label="Apple M1 Pro")

     rects3 = plt.bar(index+0.2, b, bar_width, alpha=opacity, color='cornflowerblue', label='Intel Xeon E5-2660')

     plt.xlabel('Benchmarks')
     #plt.xlabel('Mix #')
     plt.ylabel('Improvement in Execution Time (X)')
     #plt.title("Performance Improvement of Parallelization")
     #plt.title("Mix$_{16}$ execution time normalized to original time")

     ax.ticklabel_format(useOffset=False, style='plain')
     plt.xticks(index, d, rotation=0)
     plt.legend()

     plt.tight_layout()
     plt.savefig("bin_style1.png", dpi = 96 * 2)
     plt.show()

def plot_graph4():
     n_groups = 22
     a = [2,2,2,2,6,4,4,4,5,15,6,4,5,2,2,7,3,4,5,5,5,2]
     b = [0,0,0,0,0,0,0,0,1,7,0,0,1,0,0,1,1,0,0,1,1,0]
     d = ['1', '2', '3', "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22"]

     fig, ax = plt.subplots()
     index = np.arange(n_groups)
     bar_width = 0.4
     opacity = 0.9


     ax.set_facecolor('gainsboro')
     #ax.set_facecolor((1.0, 0.47, 0.42))

     rects1 = plt.bar(index-0.2, a, bar_width, alpha=opacity, color='orange', label="Without Invariant Knowledge")

     rects3 = plt.bar(index+0.2, b, bar_width, alpha=opacity, color='cornflowerblue', label='With Invariant Knowledge')

     plt.xlabel('Chapter 2 Loop Nest #')
     #plt.xlabel('Mix #')
     plt.ylabel('Number of Dependencies')
     #plt.title("Performance Improvement of Parallelization")
     #plt.title("Mix$_{16}$ execution time normalized to original time")

     ax.ticklabel_format(useOffset=False, style='plain')
     plt.xticks(index, d, rotation=0)
     plt.legend()

     plt.tight_layout()
     plt.savefig("bin_style1.png", dpi = 96 * 2)
     plt.show()

plot_graph()
plot_graph2()
plot_graph3()
plot_graph4()



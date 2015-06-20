#!/usr/bin/env python
import numpy.numarray as na

from pylab import *

labels = ["Baseline", "System", "Zxc"]
data1 =  [3.75, 4.75, 1]
data2 =  [3.95, 4.15, 3]

width = 1200

series = len(data1)
span = width / series

xlocations = na.array(range(series)) * span + span * 0.5
bwidth = 40
bar(xlocations - 1.1 * bwidth, data1, width=bwidth, color='red')
bar(xlocations + 0.1 * bwidth, data2, width=bwidth)
yticks(range(0, 8))
xticks(xlocations, labels)
xlim(0, width)
title("Average Ratings on the Training Set")
gca().get_xaxis().tick_bottom()
gca().get_yaxis().tick_left()

#show()
savefig("barcharts2.png")
